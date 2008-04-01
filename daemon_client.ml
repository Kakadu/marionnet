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


(** This is the client side of the Marionnet-daemon support: *)

open PreludeExtra.Prelude;; (* We want synchronous terminal output *)
open Daemon_language;;
open Daemon_parameters;;
open Recursive_mutex;;

(** The mutex we use to avoid sending concurrent messages to the same socket
    from different threads: *)
let the_daemon_client_mutex =
  Recursive_mutex.create ();;

(** The socket used to communicate with the daemon: *)
let the_daemon_client_socket =
  Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0;;

(** Send the given request (in abstract syntax) to the server, and return
    its response, still in abstract syntax.
    Synchronization is correctly performed *within* this function, so the
    caller doesn't need to worry about it: *)
let ask_the_server request =
  Printf.printf "I am about to send %s\n" (string_of_daemon_request request);
  flush_all ();
  with_mutex the_daemon_client_mutex
    (fun () ->
      let buffer = String.make message_length 'x' in
      let request_as_string = print_request request in
 Printf.printf "The request is %s\n" (string_of_daemon_request request);
 flush_all ();
      let sent_bytes_no = Unix.send the_daemon_client_socket request_as_string 0 message_length [] in
      (if not (sent_bytes_no == sent_bytes_no) then
        failwith "send() failed");
      let received_bytes_no =
        Unix.read the_daemon_client_socket buffer 0 message_length in
      (if received_bytes_no < message_length then
        failwith "recv() failed, or the message is ill-formed");
      let response = parse_response buffer in
 Printf.printf "The response is %s\n" (string_of_daemon_response response);
 flush_all ();
      response
      (* match response with *)
      (*   Error message -> *)
      (*     failwith ("Server-side failure: " ^ message) *)
      (* | _ -> *)
      (*     response *)
);;

(** The thunk implementing the thread which periodically sends keepalives: *)
let thread_sending_keepalives_thunk () = 
  try
    while true do
      let _ = ask_the_server IAmAlive in
      (try
        Thread.delay inter_keepalive_interval;
      with e -> begin
        Printf.printf
          "delay failed (%s). This should not be a problem.\n"
          (Printexc.to_string e);
        flush_all ();
      end);
    done;
  with e -> begin
    Printf.printf "The keepalive-sending thread failed: %s.\n" (Printexc.to_string e);
    Printf.printf "Bailing out.\n";
    flush_all ();
  end;;

(** This should be called *before* communicating with the daemon in any way: *)
let initialize_daemon_client () =
  Printf.printf "Connecting to the daemon socket...\n"; flush_all ();
  Unix.connect the_daemon_client_socket (Unix.ADDR_UNIX socket_name);
  Printf.printf "Ok, connected with success.\n"; flush_all ();;

(** Make a new thread sending keepalives to the daemon: *)
let start_thread_sending_keepalives () =
  ignore (Thread.create thread_sending_keepalives_thunk ());;

(* initialize_daemon_client ();; *)
(* start_thread_sending_keepalives ();; *)
(* let main = *)
(*   try *)
(*     while true do *)
(*       Thread.delay 20.0; *)
(*       let request = *)
(*         if (Random.float 10.0) < 1.0 then *)
(*           DestroyAllMyResources *)
(*         else *)
(*           Make (AnyTap(42, "42.42.42.42")) in *)
(*       Printf.printf "Request:  %s\n" (string_of_daemon_request request); *)
(*       flush_all (); *)
(*       let response = *)
(*         ask_the_server request in *)
(*       Printf.printf "Response: %s\n" (string_of_daemon_response response); *)
(*       Printf.printf "\n"; *)
(*       flush_all (); *)
(*     done *)
(*   with e -> begin *)
(*     Printf.printf "The daemon client failed: %s\n." (Printexc.to_string e); *)
(*     Printf.printf "Bailing out.\n"; *)
(*     flush_all (); *)
(*   end;; *)
