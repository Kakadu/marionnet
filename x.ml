(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007, 2008  Luca Saiu
   Copyright (C) 2011  Jean-Vincent Loddo
   Copyright (C) 2007, 2008, 2011  Université Paris 13

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


let fail x = failwith (Printf.sprintf "Ill-formed DISPLAY string: '%s'" x)
;;

(** The syntax of $DISPLAY is: [host]:display[.screen] *)
let get_host_display_screen_from_string x =
 let split_rigth_part y =
   match (StringExtra.split ~d:'.' y) with
   | [ display; screen ] -> (display, screen)
   | [ display ]         -> (display, "0")
   | _ -> fail x
 in 
 let host, (display, screen) =
   match (StringExtra.split ~d:':' x) with
   | [ host; right_part ] -> host, (split_rigth_part right_part) 
   | [ right_part ]       -> "localhost", (split_rigth_part right_part)
   | _ -> fail x
 in 
 let strip_and_use_default_if_empty ~default x=
   let x = StringExtra.strip x in
   if x = "" then default else x
 in
 let host    = strip_and_use_default_if_empty ~default:"localhost" host in
 let screen  = strip_and_use_default_if_empty ~default:"0" screen in
 let display = StringExtra.strip display in
 (host, display, screen)
;;

let get_host_display_screen () =
  try
    let x = Sys.getenv "DISPLAY" in
    if x = "" then
      raise Not_found (* It's just like it weren't defined... *)
    else
      get_host_display_screen_from_string x
  with Not_found ->
    failwith "The environment variable DISPLAY is not defined or empty, and Marionnet requires X.\nBailing out...";;

(* Global variables: *)
let host, display, screen = get_host_display_screen ()
;;

let _last_used_local_display_index =
  ref 0;;

let mutex = Mutex.create ();;

let get_unused_local_display () =
  Mutex.lock mutex;
  let index_to_socket_file index =
    Printf.sprintf "/tmp/.X11-unix/X%i" index in
  let exists pathname =
    try
      ignore (Unix.stat pathname);
      true;
    with _ ->
      false in
  let i = ref (!_last_used_local_display_index + 1) in
  while exists (index_to_socket_file !i) do
    i := !i + 1;
  done;
  _last_used_local_display_index := !i;
  Mutex.unlock mutex;
  Printf.sprintf ":%i" !i;;

Log.printf
  "-------------------------------------\nHost X data from $DISPLAY:\n\nHost: %s\nDisplay: %s\nScreen: %s\n\n"
  host
  display
  screen;;

(** This has to be performed *early* in the initialization process: *)
let _ = GtkMain.Main.init ();;

(** This is a workaround for some threading issues suggested by Jacques Garrigue;
    it's needed to be able to use the 'run' method in GTK and Glade objects 
    without preventing other unrelated threads to run: *)
let _ =
  GMain.Timeout.add ~ms:100 ~callback:(fun () -> Thread.delay 0.001; true);;
