(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2008, 2009  Luca Saiu
   Copyright (C) 2009, 2010  Jean-Vincent Loddo
   Copyright (C) 2008, 2009, 2010  Université Paris 13

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


(** This is the client side of the Marionnet-daemon support: *)

open Daemon_language;;
open Gettext;;

module Recursive_mutex = MutexExtra.Recursive ;;
let socket_name = Daemon_parameters.socket_name;;
let inter_keepalive_interval = Daemon_parameters.inter_keepalive_interval;;

(** The mutex we use to avoid sending concurrent messages to the same socket
    from different threads: *)
let the_daemon_client_mutex =
  Recursive_mutex.create ();;

(** The socket used to communicate with the daemon: *)
let the_daemon_client_socket =
  Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0;;

(** Is the connection with the daemon currently up? *)
let can_we_communicate_with_the_daemon_bool_ref =
  ref true;;
let can_we_communicate_with_the_daemon () =
  Recursive_mutex.with_mutex the_daemon_client_mutex
    (fun () ->
      !can_we_communicate_with_the_daemon_bool_ref);;

(** Stop trying to communicate with the daemon: *)
let disable_daemon_support () =
  Recursive_mutex.with_mutex the_daemon_client_mutex
    (fun () ->
      can_we_communicate_with_the_daemon_bool_ref := false);;

(** Send the given request (in abstract syntax) to the server, and return
    its response, still in abstract syntax.
    Synchronization is correctly performed *within* this function, so the
    caller doesn't need to worry about it: *)
let ask_the_server request =
  Recursive_mutex.with_mutex the_daemon_client_mutex
    (fun () ->
      try
        if can_we_communicate_with_the_daemon () then begin
          let buffer = String.make message_length 'x' in
          let request_as_string = print_request request in
          let sent_byte_no = Unix.send the_daemon_client_socket request_as_string 0 message_length [] in
          (if not (sent_byte_no == sent_byte_no) then
            failwith "send() failed");
          let received_byte_no =
            Unix.read the_daemon_client_socket buffer 0 message_length in
          (if received_byte_no < message_length then
            failwith "recv() failed, or the message is ill-formed");
          let response = parse_response buffer in
          response
        end else
          (Error "the socket to the daemon is down");
      with e -> begin
        Log.printf1 "ask_the_server failed: %s\n" (Printexc.to_string e);
        disable_daemon_support ();
        Simple_dialogs.error
          (s_ "Failure in daemon communication")
          (s_ "Error in trying to communicate with the daemon.\nThe application should remain usable, but without the features requiring root access...")
          ();
        (Error "the socket to the daemon just went down");
      end);;

(** The thunk implementing the thread which periodically sends keepalives: *)
let thread_sending_keepalives_thunk () =
  try
    while true do
      let _ = ask_the_server IAmAlive in
      (try
        Thread.delay inter_keepalive_interval;
      with e -> begin
        Log.printf1
          "delay failed (%s). This should not be a problem.\n"
          (Printexc.to_string e);
      end);
    done;
  with e -> begin
    Log.printf1 "The keepalive-sending thread failed: %s.\n" (Printexc.to_string e);
    Log.printf "Bailing out.\n";
  end;;

(** This should be called *before* communicating with the daemon in any way: *)
let initialize_daemon_client () =
  Log.printf "Connecting to the daemon socket...\n";
  Unix.connect the_daemon_client_socket (Unix.ADDR_UNIX socket_name);
  Log.printf "Ok, connected with success.\n"
;;

(** Make a new thread sending keepalives to the daemon: *)
let start_thread_sending_keepalives () =
  ignore (Thread.create thread_sending_keepalives_thunk ());;
