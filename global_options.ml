(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007, 2008  Luca Saiu
   Copyright (C) 2010  Jean-Vincent Loddo
   Copyright (C) 2007, 2008, 2010  Université Paris 13

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

module Recursive_mutex = MutexExtra.Recursive ;;

let mutex = Recursive_mutex.create ();;

(** Here we only use one mutex; let's not specify it every time: *)
let with_mutex thunk =
  Recursive_mutex.with_mutex mutex thunk;;

(** Debug mode related functions are accessible also from this module: *)
module Debug_level = Initialization.Debug_level;;

(** Automatically generate IP addresses: *)
let autogenerate_ip_addresses_default =
  false (*false*);;

let autogenerate_ip_addresses =
  ref autogenerate_ip_addresses_default;;
let set_autogenerate_ip_addresses value =
  with_mutex
    (fun () ->
      autogenerate_ip_addresses := value);;
let get_autogenerate_ip_addresses () =
  with_mutex
    (fun () ->
      !autogenerate_ip_addresses);;

(** Work-around the wirefilter bug (which is probably due to my patches to VDE): *)
let workaround_wirefilter_problem_default =
  true;; (* true *)
let workaround_wirefilter_problem =
  ref workaround_wirefilter_problem_default;;
let set_workaround_wirefilter_problem value =
  with_mutex
    (fun () ->
      workaround_wirefilter_problem := value);;
let get_workaround_wirefilter_problem () =
  with_mutex
    (fun () ->
      !workaround_wirefilter_problem);;

(** How often we should restart wirefilters (average) *)
let automatic_reboot_thread_interval =
  180.0;;

(** The name of the host bridge device used to implement network sockets: *)
let ethernet_socket_bridge_name =
  Initialization.configuration#string "MARIONNET_BRIDGE";;

(** Keyboard layout in Xnest sessions; `None' means `don't set anything' *)
let keyboard_layout =
  let default_layout = None (*Some "fr"*) in
  try
    Some (Initialization.configuration#string "MARIONNET_KEYBOARD_LAYOUT")
  with Not_found ->
    default_layout;;


module Keep_all_snapshots_when_saving =
  Stateful_modules.Variable (struct
    type t = bool
    let name = Some "keep_all_snapshots_when_saving"
  end);;
let () = Keep_all_snapshots_when_saving.set Initialization.keep_all_snapshots_when_saving ;;

