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


open PreludeExtra.Prelude;; (* We want synchronous terminal output *)
open Daemon_language;;
open Daemon_parameters;;
open Thread;;
open Recursive_mutex;;
open Hashmap;;
open Hashmmap;;

type client = int;;

let string_of_client client =
  Printf.sprintf "<client #%i>" client;;

(** The mutex used to protect the resource map from concurrent access: *)
let the_daemon_mutex =
  Recursive_mutex.create ();;

(** An associative structure mapping each client to its resources: *)
let resource_map =
  new hashmultimap ();;

(** An associative structure mapping each client to the time of the death
    of its resources (unless they send messages, of course): *)
let client_death_time_map =
  new hashmap ();;

(** An associative structure mapping each client to its socket: *)
let socket_map =
  new hashmap ();;

(** Seed the random number generator: *)
Random.self_init ();;

(** Generate a random name, very probably unique, with the given prefix: *)
let make_fresh_name prefix =
  let random_number = Random.int 1000000 in
  Printf.sprintf "%s%i" prefix random_number;;

(** Generate a random name, very probably unique, for a new tap: *)
let make_fresh_tap_name () =
   make_fresh_name "tap";;

(** Generate a random name, very probably unique, for a new bridge: *)
let make_fresh_bridge_name () =
  make_fresh_name "bridge";;

(* To do: move this into UnixExtra or something like that: *)
(** Run system with the given argument, and raise exception in case of failure;
    return unit on success. *)
let system_or_fail command_line =
  Printf.printf "Executing \'%s\'...\n" command_line;
  flush_all ();
  match Unix.system command_line with
    Unix.WEXITED 0 ->
      ()
  | Unix.WEXITED n ->
      failwith (Printf.sprintf "Unix.system: the process exited with %i" n)
  | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
      failwith "Unix.system: the process was signaled or stopped";;

let make_system_tap (tap_name : tap_name) uid ip_address =
  Printf.printf "Creating the tap %s...\n" tap_name;
  let command_line =
    Printf.sprintf
      "tunctl -u %i -t %s && ifconfig %s 172.23.0.254 netmask 255.255.255.255 up; route add %s %s"
      uid tap_name tap_name ip_address tap_name in
  system_or_fail command_line;
  Printf.printf "The tap %s was created with success\n" tap_name;
  flush_all ();;
let make_system_gateway_tap (tap_name : tap_name) uid =
  Printf.printf "Creating the tap %s [To do: actually do it]\n" tap_name;
  ();;
let make_system_bridge (bridge_name : bridge_name) =
  Printf.printf "Creating the bridge %s...\n" bridge_name;
  let command_line =
    Printf.sprintf "/usr/marionnet/bin/prepare_bridge.sh %s" bridge_name in
  system_or_fail command_line;
  Printf.printf "The bridge %s was created with success\n" bridge_name;
  flush_all ();;
let destroy_system_tap (tap_name : tap_name) =
  Printf.printf "Destroying the tap %s...\n" tap_name;
  let command_line =
    Printf.sprintf
      "while ! (ifconfig %s down && tunctl -d %s); do echo 'I can not destroy %s yet...'; sleep 1; done&"
      tap_name tap_name tap_name in
  system_or_fail command_line;
  Printf.printf "The tap %s was destroyed with success\n" tap_name;
  flush_all ();;
let destroy_system_bridge (bridge_name : bridge_name) =
  Printf.printf "Destroying the bridge %s [To do: actually do it]\n" bridge_name;
  ();;

(** Instantiate the given pattern, actually create the system object, and return
    the instantiated resource: *)
let make_system_resource resource_pattern =
  match resource_pattern with
  | AnyTap(uid, ip_address) ->
      let name = make_fresh_tap_name () in
      make_system_tap name uid ip_address;
      Tap name
  | AnyGatewayTap uid ->
      let name = make_fresh_tap_name () in
      make_system_gateway_tap name uid;
      Tap name
  | AnyBridge ->
      let name = make_fresh_bridge_name () in
      make_system_bridge name;
      Bridge name;;

(** Actually destroyed the system object named by the given resource: *)
let destroy_system_resource resource =
  match resource with
  | Tap tap_name ->
      destroy_system_tap tap_name
  | Bridge bridge_name ->
      destroy_system_bridge bridge_name;;

(** Create a suitable resource matching the given pattern, and return it.
    Synchronization is performed inside this function, hence the caller doesn't need
    to worry about it: *)
let make_resource client resource_pattern =
  with_mutex the_daemon_mutex
    (fun () ->
      try
        (* Create a resource satisfying the given specification, and return it: *)
        Printf.printf
          "Making %s for %s\n"
          (string_of_daemon_resource_pattern resource_pattern)
          (string_of_client client);
        let resource = make_system_resource resource_pattern in
        Printf.printf "Adding %s for %s\n" (string_of_daemon_resource resource) (string_of_client client);
        resource_map#add client resource;
        resource
      with e -> begin
        Printf.printf "Failed (%s) when making the resource %s for %s; bailing out.\n"
          (Printexc.to_string e)
          (string_of_daemon_resource_pattern resource_pattern)
          (string_of_client client);
        raise e;
      end);;

(** Destroy the given resource. Synchronization is performed inside this function,
    hence the caller doesn't need to worry about it: *)
let destroy_resource client resource =
  with_mutex the_daemon_mutex
    (fun () ->
      try
        Printf.printf "Removing %s %s\n" (string_of_client client) (string_of_daemon_resource resource);
        resource_map#remove_key_value_or_fail client resource;
        destroy_system_resource resource;
        (* resource_map#remove_key_value_or_fail client resource; *)
      with e -> begin
        Printf.printf "WARNING: failed (%s) when destroying %s for %s.\n"
          (Printexc.to_string e)
          (string_of_daemon_resource resource)
          (string_of_client client);
        raise e;
      end);;

let destroy_all_client_resources client =
  with_mutex the_daemon_mutex
    (fun () ->
      try
        Printf.printf "Removing all %s's resources:\n" (string_of_client client);
        List.iter
          (fun resource -> destroy_resource client resource)
          (resource_map#lookup client);
        Printf.printf "All %s's resources were removed with success.\n" (string_of_client client);
        flush_all ();
      with e -> begin
        Printf.printf "Failed (%s) when removing %s's resources; continuing anyway.\n"
          (Printexc.to_string e)
          (string_of_client client);
      end);;

let destroy_all_resources () =
  with_mutex the_daemon_mutex
    (fun () ->
      List.iter
        (fun (client, _) ->
          try
            destroy_all_client_resources client
          with e -> begin
            Printf.printf "Failed (%s) when removing %s's resources (while removing *all* resources); continuing anyway.\n"
              (Printexc.to_string e)
              (string_of_client client);
          end))
        client_death_time_map#to_alist;;

let keep_alive_client client =
  with_mutex the_daemon_mutex
    (fun () ->
      try
        (* Immediately raise an exception if the client is not alive: *)
        let _ = client_death_time_map#lookup client in
        let current_time = Unix.time () in
        let death_time = current_time +. timeout_interval in
        client_death_time_map#add client death_time;
        Printf.printf
          "I will not kill %s until %f (it's now %f)\n"
          (string_of_client client)
          death_time
          current_time;
        flush_all ();
      with Not_found -> begin
        Printf.printf
          "keep_client_alive failed because the client %s is not alive.\n"
          (string_of_client client);
        failwith ("keep_alive_client: " ^ (string_of_client client) ^ " is not alive.");
      end);;

(** Some resources [only a bridge, as of now] are global, i.e. shared by all
    clients whenever there is at least one. We use a reference-counter to keep
    track of the number of currently existing clients; global resources are
    created when the counter raises from 0 to 1, and destroyed when it drops
    from 1 to 0. *)
let clients_no = ref 0;;
let the_bridge_if_any = ref None;;
let global_bridge () =
  with_mutex the_daemon_mutex
    (fun () ->
      match !the_bridge_if_any with
      | None ->
          failwith "the global bridge does not exist; this should never happen"
      | Some (Bridge bridge_name) ->
          bridge_name
      | _ ->
          failwith "the global bridge is not a bridge; this should never happen");;
let create_global_resources_unlocked_ () =
  assert(!the_bridge_if_any = None);
  the_bridge_if_any := Some (make_system_resource AnyBridge);;
let destroy_global_resources_unlocked_ () =
  match !the_bridge_if_any with
  | None -> assert false
  | Some bridge -> begin
      destroy_system_resource bridge;
      the_bridge_if_any := None;
      flush_all ();
  end;;
let increment_clients_no () =
  with_mutex the_daemon_mutex
    (fun () ->
      (if !clients_no = 0 then begin
        Printf.printf "There is at least one client now. Creating global resources...\n";
        create_global_resources_unlocked_ ();
        Printf.printf "Global resources were created with success.\n";
        flush_all ();
      end);
      clients_no := !clients_no + 1);;
let decrement_clients_no () =
  with_mutex the_daemon_mutex
    (fun () ->
      clients_no := !clients_no - 1;
      (if !clients_no = 0 then begin
        Printf.printf "There are no more clients now. Destroying global resources...\n";
        destroy_global_resources_unlocked_ ();
        Printf.printf "Global resources were destroyed with success.\n";
        flush_all ();
      end));;

(** Create a new client on which we're going to interact with the given socket,
    and return its identifier: *)
let make_client = 
  let next_client_no = ref 1 in
  fun socket ->
    with_mutex the_daemon_mutex
      (fun () ->
        (* Generate a new unique identifier: *)
        let result = !next_client_no in
        next_client_no := !next_client_no + 1;
        (* First add any number to the data structure, then call keep_alive_client to make
           the death time correct: *)
        Printf.printf "Creating %s.\n" (string_of_client result); flush_all ();
        client_death_time_map#add result 42.42;
        socket_map#add result socket;
        keep_alive_client result;
        increment_clients_no ();
        Printf.printf "Created %s.\n" (string_of_client result); flush_all ();
        result);;

let destroy_client client =
  with_mutex the_daemon_mutex
    (fun () ->
      Printf.printf "Killing %s.\n" (string_of_client client); flush_all ();
      (try client_death_time_map#remove client with _ -> ());
      (try destroy_all_client_resources client with _ -> ());
      decrement_clients_no ();
      (try
        Unix.close (socket_map#lookup client);
        Printf.printf
          "The socket serving the client %i was closed with success.\n" client;
        flush_all ();
      with e -> begin
        Printf.printf
          "Closing the socket serving the client %i failed (%s).\n"
          client (Printexc.to_string e);
        flush_all ();
      end);
      (try socket_map#remove client with _ -> ());
      Printf.printf "%s was killed.\n" (string_of_client client); flush_all ());;

let debugging_thread_thunk () =
  while true do
    Thread.delay debug_interval;
    with_mutex the_daemon_mutex
      (fun () ->
        Printf.printf "--------------------------------------------\n";
        Printf.printf "Currently existing non-global resources are:\n";
        List.iter
          (fun (client, resource) ->
            Printf.printf "* %s (owned by %s)\n" (string_of_daemon_resource resource) (string_of_client client))
          (resource_map#to_alist);
        Printf.printf "--------------------------------------------\n";
        flush_all ();
        );
    flush_all ();
  done;;  

(** The 'timeout thread' wakes up every timeout_interval seconds and kills
    all clients whose death time is past. *)
let timeout_thread_thunk () =
  while true do
    (* Sleep: *)
    Thread.delay timeout_interval;

    (* Some variables are shared, so we have to synchronize this block; it's not
       a problem as this should be very quick: *)
    with_mutex the_daemon_mutex
      (fun () -> 
        (* Get up-to-date death time information for all clients: *)
        let current_time = Unix.time () in
        let client_death_times = client_death_time_map#to_alist in
        (* Kill all clients whose death time is past: *)
        List.iter
          (fun (client, death_time) ->
            if current_time >= death_time then begin
              Printf.printf "Client %s didn't send enough keep-alive's.\n" (string_of_client client);
              destroy_client client;
              flush_all ();
            end)
          client_death_times;
        flush_all ());
  done;;

(** Serve the given single request from the given client, and return the
    response. This does not include the keep-alive. *)
let serve_request request client =
  match request with
  | IAmAlive ->
      Success
  | Make resource_pattern ->
      Created (make_resource client resource_pattern)
  | Destroy resource ->
      destroy_resource client resource;
      Success
  | DestroyAllMyResources -> begin
      destroy_all_client_resources client;
      Success
  end;;

(** This thread serves *one* client whose socket is given and is assumed
    to be open: *)
let connection_server_thread (client, socket) =
  try
    Printf.printf "This is the connection server thread for client %i.\n" client;
    flush_all ();
    while true do
      Printf.printf "Beginning of the iteration.\n"; flush_all ();
      (* We want the message to be initially invalid, at every iteration, to
         avoid the risk of not seeing a receive error. Just to play it extra
        safe: *)
      let buffer = String.make message_length 'x' in
      (* We don't want to block indefinitely on read() because the socket could
         be closed by another thread; so we simply select() with a timeout: *)
      let (ready_for_read, _, failed) =
(****)
        try
          Unix.select [socket] [] [socket] select_timeout
        with _ -> begin
          Printf.printf "!!!!FAILED IN select (connection_server_thread)!!!!\n";
          flush_all ();
          ([], [], []);
        end
in
(****)
        (* Unix.select [socket] [] [socket] select_timeout in *)
      if (List.length failed) > 0 then
        failwith "select() reported failure with the socket"
      else if (List.length ready_for_read) > 0 then begin
        let received_bytes_no =
          Unix.read socket buffer 0 message_length in
        if received_bytes_no < message_length then
          failwith "recv() failed, or the message is ill-formed"
        else begin
          let request = parse_request buffer in
          Printf.printf "The request is\n  %s\n" (string_of_daemon_request request); flush_all ();
    (* Printf.printf "Before keep-alive.\n"; flush_all (); *)
          keep_alive_client client;
    (* Printf.printf "After keep-alive.\n"; flush_all (); *)
          let response =
            try
              serve_request request client
            with e ->
              Error (Printexc.to_string e)
          in
          Printf.printf "My response is\n  %s\n" (string_of_daemon_response response); flush_all ();
    (* Printf.printf "Ok-Q 0\n"; flush_all (); *)
          let sent_bytes_no = Unix.send socket (print_response response) 0 message_length [] in
    (* Printf.printf "Ok-Q 1 (%i bytes sent)\n" sent_bytes_no; flush_all (); *)
          (if not (sent_bytes_no == sent_bytes_no) then
            failwith "send() failed");
    (* Printf.printf "Ok-Q 2\n"; flush_all (); *)
        end; (* inner else *)
      end else begin
        (* If we arrived here select() returned due to the timeout, and we
           didn't receive anything: loop again. *)
    (* Printf.printf "select() returned due to timeout.\n"; flush_all (); *)
      end;
    (* Printf.printf "End of the iteration.\n"; flush_all (); *)
    done;
  with e -> begin
    Printf.printf
      "Failed in connection_server_thread (%s) for client %i.\nBailing out.\n"
      (Printexc.to_string e)
      client;
    destroy_client client; (* This also closes the socket *)
    Printf.printf "Exiting from the thread which was serving client %i\n" client;
    flush_all ();
  end;;

(*
let the_server_main_thread =
  ignore (Thread.create timeout_thread_thunk ());
  ignore (Thread.create debugging_thread_thunk ());
  let port = 12345 in 
  let connections_no_limit = 10 in
  let accepting_socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let host = Unix.gethostbyname (Unix.gethostname ()) in
  let h_addr = host.Unix.h_addr_list.(0) in
  let sock_addr = Unix.ADDR_INET(h_addr, port) in
  Printf.printf "I am waiting on %s, port %i.\n" (Unix.gethostname ()) port; flush_all ();
  Unix.bind accepting_socket sock_addr;
  Unix.listen accepting_socket connections_no_limit;
  while true do 
    Printf.printf "Waiting for the next connection...\n"; flush_all ();
    let (socket_to_client, socket_to_client_address) = Unix.accept accepting_socket in
    Printf.printf "A new connection was accepted.\n"; flush_all ();
    let client_no = make_client socket_to_client in
    Printf.printf "The new client id is %i\n" client_no; flush_all ();
    ignore (Thread.create connection_server_thread (client_no, socket_to_client));
  done;;
*)

(** Remove an old socket file, remained from an old instance or from ours
    (when we're about to exit). Do nothing if there is no such file: *)
let remove_socket_file_if_any () =
  try
    Unix.unlink socket_name;
    Printf.printf "[Removed the old socket file %s]\n" socket_name;
    flush_all ();
  with _ ->
    Printf.printf "[There was no need to remove the socket file %s]\n" socket_name;;

(** Destroy all resources, destroy the socket and exit on either SIGINT and SIGTERM: *)
let signal_handler signal =
  Printf.printf "=========================\n";
  Printf.printf "I received the signal %i!\n" signal;
  Printf.printf "=========================\n";
  Printf.printf "Destroying all resources...\n";
  destroy_all_resources ();
  Printf.printf "Ok, all resources were destroyed.\n";
  Printf.printf "Removing the socket file...\n";
  remove_socket_file_if_any (); 
  Printf.printf "Ok, the socket file was removed.\n";
  flush_all ();
  raise Exit;;

(** Strangely, without calling this the program is uninterruptable from the
    console: *)
Sys.catch_break false;;
Sys.set_signal Sys.sigint (Sys.Signal_handle signal_handler);;
Sys.set_signal Sys.sigterm (Sys.Signal_handle signal_handler);;

let the_server_main_thread =
  ignore (Thread.create timeout_thread_thunk ());
  ignore (Thread.create debugging_thread_thunk ());
  let connections_no_limit = 10 in
  let accepting_socket = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let sock_addr = Unix.ADDR_UNIX socket_name in
  (* Remove the socket file, if it already exists: *)
  remove_socket_file_if_any ();
  (* Bind the file to the socket; this creates the file, or fails if there
     are permission or disk space problems: *)
  Unix.bind accepting_socket sock_addr;
  (* Everybody must be able to send messages to us: *)
  Unix.chmod socket_name 438 (* a+rw *);
  Printf.printf "I am waiting on %s.\n" socket_name; flush_all ();
  Unix.listen accepting_socket connections_no_limit;
  while true do 
    try
      Printf.printf "Waiting for the next connection...\n"; flush_all ();
      let (socket_to_client, socket_to_client_address) = Unix.accept accepting_socket in
      let client_no = make_client socket_to_client in
      Printf.printf "A new connection was accepted; the new client id is %i\n" client_no; flush_all ();
      ignore (Thread.create connection_server_thread (client_no, socket_to_client));
    with e -> begin
    Printf.printf
      "Failed in the main thread (%s). Bailing out.\n"
      (Printexc.to_string e);
      flush_all ();
      raise e;
    end;
  done;;
