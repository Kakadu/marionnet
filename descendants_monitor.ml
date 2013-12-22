(* This file is part of Marionnet
   Copyright (C) 2013  Jean-Vincent Loddo
   Copyright (C) 2013  Université Paris 13

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>. *)

module Mtx = MutexExtra.Just_give_me_an_apply_with_mutex (struct end)
let apply_with_mutex = Mtx.apply_with_mutex

module Process_set =
  SetExtra.Destructive.Make (struct  type t = int * int64  let compare = Pervasives.compare end)

let start_monitor_and_get_kill_method ?(pid=Unix.getpid ()) ?(wake_up_interval=4.) ?(garbage_collection_frequence=5) () =
  let pset = Process_set.create () in
  let add_list ?(garbage_collection=false) xs =
    begin
      List.iter (fun elt -> Process_set.add elt pset) xs ;
      let () =
	if garbage_collection then
	  Process_set.filter (fun (pid,_starttime) -> UnixExtra.is_process_alive pid) pset
      in
      Log.printf1 ~v:2 "Descendants monitor: %d descendants currently observed\n" (Process_set.cardinal pset);
    end
  in
  let rec loop i =
    let ds = Linux.Process.get_descendant_stats ~pid () in
    let pid_starttime_list = List.map (fun s -> s.Linux.Process.pid, s.Linux.Process.starttime) ds in
    let garbage_collection = (i mod garbage_collection_frequence = 0) in
    let () = apply_with_mutex (add_list ~garbage_collection) pid_starttime_list in
    let () = Thread.delay (wake_up_interval) in
    loop (i+1)
  in
  let _ = Thread.create (loop) 0 in
  (* Method provided to the main thread: *)
  let kill_process_set () =
    let kill_action (pid,_starttime) =
      begin
        try Unix.kill pid Sys.sigkill with _ -> ();
      end
    in
    apply_with_mutex (Process_set.iter kill_action) pset
  in
  kill_process_set
