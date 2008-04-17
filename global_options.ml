(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007  Luca Saiu

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

open PreludeExtra.Prelude;; (* We want synchronous terminal output *)
open Recursive_mutex;;

let mutex = create ();;

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
      Printf.printf "'Debug mode' now has value %b\n" value; flush_all ();
      debug_mode := value);;
let get_debug_mode () =
  with_mutex
    (fun () ->
      !debug_mode);;

(** Automatically generate IP addresses: *)
let autogenerate_ip_addresses_default =
  false (*false*);;

let autogenerate_ip_addresses =
  ref autogenerate_ip_addresses_default;;
let set_autogenerate_ip_addresses value =
  with_mutex
    (fun () ->
      Printf.printf "'Autogenerate IP addresses' now has value %b\n" value; flush_all ();
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
      Printf.printf "'Work-around the wirefilter problem' now has value %b\n" value; flush_all ();
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
