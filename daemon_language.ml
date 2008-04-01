(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2008  Luca Saiu

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


(** The Marionnet daemon is controlled by a simple command language. Messages
    are passed as strings over sockets, which are printed from [parsed to]
    very simple abstract syntax terms. *)

(** Tap names and bridge names are just strings: *)
type tap_name =
    string;;
type bridge_name =
    string;;
type ip_address =
    string;;
type uid =
    int;;

(** The abstract syntax of requests, responses and parameters: *)
type resource_pattern =
  | AnyTap of uid * ip_address
  | AnyGatewayTap of uid
  | AnyBridge
type resource =
  | Tap of tap_name
  | Bridge of bridge_name
and daemon_request =
  | IAmAlive
  | Make of resource_pattern
  | Destroy of resource
  | DestroyAllMyResources
and daemon_response =
  | Success
  | Error of string
  | Created of resource
  | SorryIThoughtYouWereDead;;

(** Printer: this is useful for debugging. *)
let rec string_of_daemon_resource resource =
  match resource with
  | Tap name ->
      Printf.sprintf "(tap %s)" name
  | Bridge name ->
      Printf.sprintf "(bridge %s)" name
let rec string_of_daemon_resource_pattern resource_pattern =
  match resource_pattern with
  | AnyTap(uid, ip_address) ->
      Printf.sprintf "(any-tap %i %s)" uid ip_address
  | AnyGatewayTap uid ->
      Printf.sprintf "(any-gateway-tap %i)" uid
  | AnyBridge ->
      "any-bridge"
and string_of_daemon_request request =
  match request with
  | IAmAlive ->
      "i-am-alive"
  | Make resource_pattern ->
      Printf.sprintf "(make %s)" (string_of_daemon_resource_pattern resource_pattern)
  | Destroy resource ->
      Printf.sprintf "(destroy %s)" (string_of_daemon_resource resource)
  | DestroyAllMyResources ->
      "destroy-all-my-resources"
and string_of_daemon_response response =
  match response with
  | Success ->
      "success"
  | SorryIThoughtYouWereDead ->
      "sorry-i-thought-you-were-dead"
  | Error message ->
      Printf.sprintf "(error \"%s\")" message
  | Created resource ->
      Printf.sprintf "(created %s)" (string_of_daemon_resource resource);;

(** The length of all requests and responses in our protocol: *)
let message_length = 128;;

(** Return a fixed-length string of exactly message_length bytes, where the first
    character is the given opcode, the following characters are the given parameters,
    and the remaining characters, if any, are filled with spaces. The length of the
    parameter is checked: *)
let make_fixed_length_message opcode parameter =
  let parameter =
    if ((String.length parameter) + 1) > message_length then begin
      Printf.printf "Warning: the parameter \"%s\" is too long. Truncating...\n" parameter;
      flush_all ();
      String.sub parameter 0 ((String.length parameter) - 1)
    end else
      parameter in
  (Printf.sprintf "%c" opcode) ^
  parameter ^
  (String.make (message_length - (String.length parameter) - 1) ' ');;

(** Request printer (this is for the actually communication language, not for
    debugging): *)
let print_request request =
  match request with
  | IAmAlive ->
      make_fixed_length_message 'i' ""
  | Make AnyTap(uid, ip_address) ->
      make_fixed_length_message 'c' (Printf.sprintf "%i %s" uid ip_address)
  | Make (AnyGatewayTap uid) ->
      make_fixed_length_message 'g' (Printf.sprintf "%i" uid)
  | Make AnyBridge ->
      failwith "Make AnyBridge is not printable"
  | Destroy (Bridge _) ->
      failwith "Destroy (Bridge _) is not printable"
  | Destroy (Tap tap_name) ->
      make_fixed_length_message 'd' tap_name
  | DestroyAllMyResources ->
      make_fixed_length_message 'D' "";;

(** Response printer (this is for the actually communication language, not for
    debugging): *)
let print_response response =
  match response with
  | Success ->
      make_fixed_length_message 's' ""
  | Error message ->
      make_fixed_length_message 'e' message
  | Created (Bridge _) ->
      failwith "Created (Bridge _) is not printable"
  | Created (Tap tap_name) ->
      make_fixed_length_message 'c' tap_name
  | SorryIThoughtYouWereDead ->
      make_fixed_length_message '!' "";;

let remove_trailing_spaces string =
  let rec index_of_the_last_nonblank string index =
    (* We return -1 if the string is completely made of spaces. This is
       coherent with the way we use this local funcion below. *)
    if index = -1 then
      -1
    else if String.get string index = ' ' then
      index_of_the_last_nonblank string (index - 1)
    else
      index
  in
  String.sub
    string
    0
    (1 + (index_of_the_last_nonblank string ((String.length string) - 1)));;

(** Return the opcode and parameter of the given message: *)
let split_message message =
  assert((String.length message) = message_length);
  let opcode = String.get message 0 in
  let rest = String.sub message 1 (message_length - 1) in
  let parameter = remove_trailing_spaces rest in
  opcode, parameter;;

let parse_request request =
  let (opcode, parameter) = split_message request in
  match opcode with
  | 'i' -> IAmAlive
  | 'c' ->
      Scanf.sscanf parameter "%i %s" (fun uid ip_address -> Make (AnyTap(uid, ip_address)))
  | 'g' ->
      Scanf.sscanf parameter "%i" (fun uid -> Make (AnyGatewayTap uid))
  | 'd' -> Destroy (Tap parameter)
  | 'D' -> DestroyAllMyResources
  | _ -> failwith ("Could not parse the request \"" ^ request ^ "\"");;

let parse_response response  =
  let (opcode, parameter) = split_message response in
  match opcode with
  | 's' -> Success
  | 'e' -> Error parameter
  | 'c' -> Created (Tap parameter)
  | '!' -> SorryIThoughtYouWereDead
  | _ -> failwith ("Could not parse the response \"" ^ response ^ "\"");;

(** We need to handle SIGPIPE when working with sockets, as a SIGPIPE
    is the visible effect of an interrupted primitive at the OCaml level.
    Not doing this leads to extremely nasty bugs, very hard to reproduce.
    This may not the "correct" module to implement this, but in this way
    I'm sure that every process, both Marionnet (client) and the daemon
    (server) always handle the signal. *)
let signal_handler =
  fun signal ->
    Printf.printf "=========================\n";
    Printf.printf "I received the signal %i!\n" signal;
    Printf.printf "=========================\n";
    flush_all ();
    (* Raise an exception instead of silently killing a process... *)
    failwith (Printf.sprintf "got the signal %i" signal);;
Sys.set_signal Sys.sigpipe (Sys.Signal_handle signal_handler);;
