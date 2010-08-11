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


open Daemon_language;;
open Daemon_parameters;;
open Thread;;
open Hashmap;;
open Hashmmap;;

module Recursive_mutex = MutexExtra.Recursive ;;

(** Read configuration files: *)
let configuration =
  new Configuration_files.configuration
    ~software_name:"marionnet"
    ~variables:["MARIONNET_SOCKET_NAME"]
    ();;

let socket_name =
  configuration#string "MARIONNET_SOCKET_NAME";;

(** Client identifiers are simply automatically-generated sequential
    integers: *)
type client =
    int;;

(** Pretty-print a client identifier: *)
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

(** Generate a random name, very probably unique, for a new tap
    for the socket component: *)
let make_fresh_tap_name_for_socket () =
  make_fresh_name "sktap";;

(** Actaully make a tap at the OS level: *)
let make_system_tap (tap_name : tap_name) uid ip_address =
  Log.printf "Making the tap %s...\n" tap_name;
  let command_line =
    Printf.sprintf
      "{ tunctl -u %i -t %s && ifconfig %s 172.23.0.254 netmask 255.255.255.255 up; route add %s %s; }"
      uid tap_name tap_name ip_address tap_name in
  Log.system_or_fail command_line;
  Log.printf "The tap %s was created with success\n" tap_name
  ;;

(** Actually make a tap at the OS level for the bridge socket component: *)
let make_system_tap_for_socket (tap_name : tap_name) uid bridge_name =
  Log.printf "Making the tap %s...\n" tap_name;
  let command_line =
    Printf.sprintf
      "{ tunctl -u %i -t %s && ifconfig %s 0.0.0.0 promisc up && brctl addif %s %s; }"
      uid
      tap_name
      tap_name
      bridge_name tap_name in
  Log.system_or_fail command_line;
  Log.printf "The tap %s was created with success\n" tap_name
  ;;

(** Actaully destroy a tap at the OS level: *)
let destroy_system_tap (tap_name : tap_name) =
  Log.printf "Destroying the tap %s...\n" tap_name;
  let redirection = Global_options.debug_mode_redirection () in
  let command_line =
    Printf.sprintf
      "while ! (ifconfig %s down && tunctl -d %s %s); do echo 'I can not destroy %s yet %s...'; sleep 1; done&"
      tap_name tap_name redirection tap_name redirection  in
  Log.system_or_fail ~hide_output:false ~hide_errors:false command_line;
  Log.printf "The tap %s was destroyed with success\n" tap_name
  ;;

(** Actually destroy a tap at the OS level for the socket component: *)
let destroy_system_tap_for_socket (tap_name : tap_name) uid bridge_name =
  Log.printf "Destroying the tap %s...\n" tap_name;
  let command_line =
    (* This is currently disabled. We have to decide what to do about this: *)
    Printf.sprintf
      "{ ifconfig %s down && brctl delif %s %s && tunctl -d %s; }"
      tap_name bridge_name tap_name tap_name in
  Log.system_or_fail command_line;
  Log.printf "The tap %s was destroyed with success\n" tap_name;
  ;;

(** Instantiate the given pattern, actually create the system object, and return
    the instantiated resource: *)
let make_system_resource resource_pattern =
  match resource_pattern with
  | AnyTap(uid, ip_address) ->
      let tap_name = make_fresh_tap_name () in
      make_system_tap tap_name uid ip_address;
      Tap tap_name
  | AnySocketTap(uid, bridge_name) ->
      let tap_name = make_fresh_tap_name_for_socket () in
      make_system_tap_for_socket tap_name uid bridge_name;
      SocketTap(tap_name, uid, bridge_name);;

(** Actually destroyed the system object named by the given resource: *)
let destroy_system_resource resource =
  match resource with
  | Tap tap_name ->
      destroy_system_tap tap_name
  | SocketTap(tap_name, uid, bridge_name) ->
      destroy_system_tap_for_socket tap_name uid bridge_name;;

(** Create a suitable resource matching the given pattern, and return it.
    Synchronization is performed inside this function, hence the caller doesn't need
    to worry about it: *)
let make_resource client resource_pattern =
  Recursive_mutex.with_mutex the_daemon_mutex
    (fun () ->
      try
        (* Create a resource satisfying the given specification, and return it: *)
        Log.printf
          "Making %s for %s\n"
          (string_of_daemon_resource_pattern resource_pattern)
          (string_of_client client);
        let resource = make_system_resource resource_pattern in
        Log.printf "Adding %s for %s\n" (string_of_daemon_resource resource) (string_of_client client);
        resource_map#add client resource;
        resource
      with e -> begin
        Log.printf "Failed (%s) when making the resource %s for %s; bailing out.\n"
          (Printexc.to_string e)
          (string_of_daemon_resource_pattern resource_pattern)
          (string_of_client client);
        raise e;
      end);;

(** Destroy the given resource. Synchronization is performed inside this function,
    hence the caller doesn't need to worry about it: *)
let destroy_resource client resource =
  Recursive_mutex.with_mutex the_daemon_mutex
    (fun () ->
      try
        Log.printf "Removing %s %s\n" (string_of_client client) (string_of_daemon_resource resource);
        Log.printf "** resource_map has %i bindings\n" (List.length resource_map#to_list); flush_all ();
        resource_map#remove_key_value_or_fail client resource;
        (* resource_map#remove_key_value client resource; *)
        Log.printf "** resource_map has %i bindings\n" (List.length resource_map#to_list); flush_all ();
        destroy_system_resource resource;
      with e -> begin
        Log.printf "WARNING: failed (%s) when destroying %s for %s.\n"
          (Printexc.to_string e)
          (string_of_daemon_resource resource)
          (string_of_client client);
        raise e;
      end);;

let destroy_all_client_resources client =
  Recursive_mutex.with_mutex the_daemon_mutex
    (fun () ->
      try
        Log.printf "Removing all %s's resources:\n" (string_of_client client);
        List.iter
          (fun resource -> destroy_resource client resource)
          (resource_map#lookup client);
        Log.printf "All %s's resources were removed with success.\n" (string_of_client client);
        flush_all ();
      with e -> begin
        Log.printf "Failed (%s) when removing %s's resources; continuing anyway.\n"
          (Printexc.to_string e)
          (string_of_client client);
      end);;

let destroy_all_resources () =
  Recursive_mutex.with_mutex the_daemon_mutex
    (fun () ->
      List.iter
        (fun (client, _) ->
          try
            destroy_all_client_resources client
          with e -> begin
            Log.printf "Failed (%s) when removing %s's resources (while removing *all* resources); continuing anyway.\n"
              (Printexc.to_string e)
              (string_of_client client);
          end))
        client_death_time_map#to_list;;

let keep_alive_client client =
  Recursive_mutex.with_mutex the_daemon_mutex
    (fun () ->
      try
        (* Immediately raise an exception if the client is not alive: *)
        let _ = client_death_time_map#lookup client in
        let current_time = Unix.time () in
        let death_time = current_time +. timeout_interval in
        client_death_time_map#add client death_time;
        Log.printf
          "I will not kill %s until %f (it's now %f)\n"
          (string_of_client client)
          death_time
          current_time;
        flush_all ();
      with Not_found -> begin
        Log.printf
          "keep_client_alive failed because the client %s is not alive.\n"
          (string_of_client client);
        failwith ("keep_alive_client: " ^ (string_of_client client) ^ " is not alive.");
      end);;

(** Some resources [well, none as of now] are global, i.e. shared by all
    clients whenever there is at least one. We use a reference-counter to keep
    track of the number of currently existing clients; global resources are
    created when the counter raises from 0 to 1, and destroyed when it drops
    from 1 to 0. *)
let clients_no = ref 0;;
let the_resources_if_any = ref None;;
let global_resources () =
  Recursive_mutex.with_mutex the_daemon_mutex
    (fun () ->
      match !the_resources_if_any with
      | None ->
          failwith "the global resources do not exist; this should never happen"
      | Some resources ->
          resources);;
let make_global_resources_unlocked_ () =
  assert(!the_resources_if_any = None);
  (* To do: actually create something, if needed. *)
  the_resources_if_any := Some ();;
let destroy_global_resources_unlocked_ () =
  match !the_resources_if_any with
  | None -> assert false
  | Some resources -> begin
      (* To do: actually destroy something, if needed. *)
      the_resources_if_any := None;
      flush_all ();
  end;;
let increment_clients_no () =
  Recursive_mutex.with_mutex the_daemon_mutex
    (fun () ->
      (if !clients_no = 0 then begin
        Log.printf "There is at least one client now. Creating global resources...\n";
        make_global_resources_unlocked_ ();
        Log.printf "Global resources were created with success.\n";
        flush_all ();
      end);
      clients_no := !clients_no + 1);;
let decrement_clients_no () =
  Recursive_mutex.with_mutex the_daemon_mutex
    (fun () ->
      clients_no := !clients_no - 1;
      (if !clients_no = 0 then begin
        Log.printf "There are no more clients now. Destroying global resources...\n";
        destroy_global_resources_unlocked_ ();
        Log.printf "Global resources were destroyed with success.\n";
        flush_all ();
      end));;

(** Create a new client on which we're going to interact with the given socket,
    and return its identifier: *)
let make_client =
  let next_client_no = ref 1 in
  fun socket ->
    Recursive_mutex.with_mutex the_daemon_mutex
      (fun () ->
        (* Generate a new unique identifier: *)
        let result = !next_client_no in
        next_client_no := !next_client_no + 1;
        (* First add any number to the data structure, then call keep_alive_client to make
           the death time correct: *)
        Log.printf "Creating %s.\n" (string_of_client result); flush_all ();
        client_death_time_map#add result 42.42;
        socket_map#add result socket;
        keep_alive_client result;
        increment_clients_no ();
        Log.printf "Created %s.\n" (string_of_client result); flush_all ();
        result);;

let destroy_client client =
  Recursive_mutex.with_mutex the_daemon_mutex
    (fun () ->
      Log.printf "Killing %s.\n" (string_of_client client); flush_all ();
      (try client_death_time_map#remove client with _ -> ());
      (try destroy_all_client_resources client with _ -> ());
      decrement_clients_no ();
      (try
        Unix.close (socket_map#lookup client);
        Log.printf
          "The socket serving the client %i was closed with success.\n" client;
        flush_all ();
      with e -> begin
        Log.printf
          "Closing the socket serving the client %i failed (%s).\n"
          client (Printexc.to_string e);
        flush_all ();
      end);
      (try socket_map#remove client with _ -> ());
      Log.printf "%s was killed.\n" (string_of_client client); flush_all ());;

let debugging_thread_thunk () =
  while true do
    Thread.delay debug_interval;
    Recursive_mutex.with_mutex the_daemon_mutex
      (fun () ->
        Log.printf "--------------------------------------------\n";
        Log.printf "Currently existing non-global resources are:\n";
        List.iter
          (fun (client, resource) ->
            Log.printf "* %s (owned by %s)\n" (string_of_daemon_resource resource) (string_of_client client))
          (resource_map#to_list);
        Log.printf "--------------------------------------------\n";
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
    Recursive_mutex.with_mutex the_daemon_mutex
      (fun () ->
        (* Get up-to-date death time information for all clients: *)
        let current_time = Unix.time () in
        let client_death_times = client_death_time_map#to_list in
        (* Kill all clients whose death time is past: *)
        List.iter
          (fun (client, death_time) ->
            if current_time >= death_time then begin
              Log.printf "Client %s didn't send enough keep-alive's.\n" (string_of_client client);
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
    Log.printf "This is the connection server thread for client %i.\n" client;
    flush_all ();
    while true do
      Log.printf "Beginning of the iteration.\n"; flush_all ();
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
          Log.printf "!!!!FAILED IN select (connection_server_thread)!!!!\n";
          flush_all ();
          failwith "select() failed";
          (* ([], [], []); *)
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
          Log.printf "The request is\n  %s\n" (string_of_daemon_request request); flush_all ();
          keep_alive_client client;
          let response =
            try
              serve_request request client
            with e ->
              Error (Printexc.to_string e)
          in
          Log.printf "My response is\n  %s\n" (string_of_daemon_response response); flush_all ();
          let sent_bytes_no = Unix.send socket (print_response response) 0 message_length [] in
          (if not (sent_bytes_no == sent_bytes_no) then
            failwith "send() failed");
        end; (* inner else *)
      end else begin
        (* If we arrived here select() returned due to the timeout, and we
           didn't receive anything: loop again. *)
      end;
    done;
  with e -> begin
    Log.printf
      "Failed in connection_server_thread (%s) for client %i.\nBailing out.\n"
      (Printexc.to_string e)
      client;
    destroy_client client; (* This also closes the socket *)
    Log.printf "Exiting from the thread which was serving client %i\n" client;
    flush_all ();
  end;;

(** Remove an old socket file, remained from an old instance or from ours
    (when we're about to exit). Do nothing if there is no such file: *)
let remove_socket_file_if_any () =
  try
    Unix.unlink socket_name;
    Log.printf "[Removed the old socket file %s]\n" socket_name;
    flush_all ();
  with _ ->
    Log.printf "[There was no need to remove the socket file %s]\n" socket_name;;

(** Destroy all resources, destroy the socket and exit on either SIGINT and SIGTERM: *)
let signal_handler signal =
  Log.printf "=========================\n";
  Log.printf "I received the signal %i!\n" signal;
  Log.printf "=========================\n";
  Log.printf "Destroying all resources...\n";
  destroy_all_resources ();
  Log.printf "Ok, all resources were destroyed.\n";
  Log.printf "Removing the socket file...\n";
  remove_socket_file_if_any ();
  Log.printf "Ok, the socket file was removed.\n";
  flush_all ();
  raise Exit;;

(** Strangely, without calling this the program is uninterruptable from the
    console: *)
Sys.catch_break false;;
Sys.set_signal Sys.sigint (Sys.Signal_handle signal_handler);;
Sys.set_signal Sys.sigterm (Sys.Signal_handle signal_handler);;

let check_that_we_are_root () =
  if (Unix.getuid ()) != 0 then begin
    Log.printf "\n*********************************************\n";
    Log.printf "* The Marionnet daemon must be run as root. *\n";
    Log.printf "* Bailing out.                              *\n";
    Log.printf "*********************************************\n\n";
    raise Exit;
  end;;

let the_server_main_thread =
  check_that_we_are_root ();
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
  Log.printf "I am waiting on %s.\n" socket_name; flush_all ();
  Unix.listen accepting_socket connections_no_limit;
  while true do
    try
      Log.printf "Waiting for the next connection...\n"; flush_all ();
      let (socket_to_client, socket_to_client_address) = Unix.accept accepting_socket in
      let client_no = make_client socket_to_client in
      Log.printf "A new connection was accepted; the new client id is %i\n" client_no; flush_all ();
      ignore (Thread.create connection_server_thread (client_no, socket_to_client));
    with e -> begin
    Log.printf
      "Failed in the main thread (%s). Bailing out.\n"
      (Printexc.to_string e);
      flush_all ();
      raise e;
    end;
  done;;
