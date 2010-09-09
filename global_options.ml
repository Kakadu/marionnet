(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007, 2008  Luca Saiu

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

(** Debug mode: *)
let debug_mode_default =
  Initialization.configuration#bool "MARIONNET_DEBUG";;

let debug_mode =
  ref debug_mode_default;;
let set_debug_mode value =
  with_mutex
    (fun () ->
      debug_mode := value);;
let get_debug_mode () =
  with_mutex
    (fun () ->
      let result = !debug_mode in
      result);;

(** Link the function used by Log with get_debug_mode: *)
let () = Log.set_debug_mode_function get_debug_mode;;

(** Interpret the value of debug_mode as suffix to append to shell commands. *)
let debug_mode_redirection () : string =
    if get_debug_mode () then "" else " >/dev/null 2>/dev/null " ;;

(** Automatically generate IP addresses: *)
let autogenerate_ip_addresses_default =
  false (*false*);;

let autogenerate_ip_addresses =
  ref autogenerate_ip_addresses_default;;
let set_autogenerate_ip_addresses value =
  with_mutex
    (fun () ->
(*      Log.printf "'Autogenerate IP addresses' now has value %b\n" value; flush_all ();*)
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
(*      Log.printf "'Work-around the wirefilter problem' now has value %b\n" value; flush_all ();*)
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

(** Project working directory: *)
let project_working_directory_default : string option = None;;
let project_working_directory = ref project_working_directory_default;;

let set_project_working_directory value =
  with_mutex
    (fun () -> project_working_directory := value);;

let get_project_working_directory () =
  with_mutex
    (fun () ->
      match !project_working_directory with
      | None -> failwith "the current working directory is currently not set"
      | Some value -> value);;

let get_project_working_directory_as_option_value () =
  with_mutex
    (fun () -> !project_working_directory ) ;;

