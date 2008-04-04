(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007  Luca Saiu
   Minor changes in 2008 by Luca Saiu

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


let display_environment_variable =
  try
    let result = Sys.getenv "DISPLAY" in
    if result = "" then
      raise Not_found (* It's just like it weren't defined... *)
    else
      result
  with Not_found ->
    failwith "The environment variable DISPLAY is not defined or empty, and Marionnet requires X.\nBailing out...";;

let screen = 
  try
    let dot_index = String.rindex display_environment_variable '.' in
    let result =
      String.sub
        display_environment_variable
        (dot_index + 1)
        ((String.length display_environment_variable) - dot_index - 1)
    in
    if String.length result = 0 then
      failwith
        (Printf.sprintf "Ill-formed DISPLAY string: the screen number in \"%s\" is empty"
           display_environment_variable)
    else
      result
  with Not_found ->
    "0";;

let get_display_given_display_string_with_no_screen s = 
  try
    let colon_index = String.rindex s ':' in
    let result =
      String.sub
        s
        (colon_index + 1)
        ((String.length s) - colon_index - 1) in
    if String.length result = 0 then
      failwith
        (Printf.sprintf "Ill-formed DISPLAY string: the display number in \"%s\" is empty" s)
    else
      result
  with Not_found ->
    failwith
      (Printf.sprintf "Ill-formed DISPLAY string: there is no display number in \"%s\"" s);;

let display = 
  try
    let dot_index = String.rindex display_environment_variable '.' in
    get_display_given_display_string_with_no_screen
      (String.sub
         display_environment_variable
         0
         dot_index)
  with Not_found ->
    get_display_given_display_string_with_no_screen display_environment_variable;;

let get_display_host_name_given_display_string_with_no_screen s = 
  try
    let colon_index = String.rindex s ':' in
    let result =
      String.sub
        s
        0
        colon_index in
    if String.length result = 0 then
      "localhost"
    else
      result
  with Not_found ->
    failwith
      (Printf.sprintf "Ill-formed DISPLAY string: there is no display number in \"%s\"" s);;

let host_name = 
  try
    let dot_index = String.rindex display_environment_variable '.' in
    get_display_host_name_given_display_string_with_no_screen
      (String.sub
         display_environment_variable
         0
         dot_index)
  with Not_found ->
    get_display_host_name_given_display_string_with_no_screen
      display_environment_variable;;

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

Printf.printf
  "-------------------------------------\nHost X data from $DISPLAY:\n\nHost: %s\nDisplay: %s\nScreen: %s\n\n"
  host_name
  display
  screen;;

(** This has to be performed *early* in the initialization process: *)
let _ = GtkMain.Main.init ();;

(** This is a workaround for some threading issues suggested by Jacques Garrigue;
    it's needed to be able to use the 'run' method in GTK and Glade objects 
    without preventing other unrelated threads to run: *)
let _ =
  GMain.Timeout.add ~ms:100 ~callback:(fun () -> Thread.delay 0.001; true);;
