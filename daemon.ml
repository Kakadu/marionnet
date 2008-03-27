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

(** An associative structure mapping clients to their resources: *)
let resource_map =
  new hashmultimap ();;

(** An associative structure mapping clients to the time of the death of their
    resources (unless they send messages, of course): *)
let client_death_time_map =
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

let make_system_tap (tap_name : tap_name) =
  Printf.printf "Creating the tap %s [To do: actually do it]\n" tap_name;
  ();;
let make_system_bridge (bridge_name : bridge_name) =
  Printf.printf "Creating the bridge %s [To do: actually do it]\n" bridge_name;
  ();;
let destroy_system_tap (tap_name : tap_name) =
  Printf.printf "Destroying the tap %s [To do: actually do it]\n" tap_name;
  ();;
let destroy_system_bridge (bridge_name : bridge_name) =
  Printf.printf "Destroying the bridge %s [To do: actually do it]\n" bridge_name;
  ();;

(** Instantiate the given pattern, actually create the system object, and return
    the instantiated resource: *)
let make_system_resource resource_pattern =
  match resource_pattern with
  | AnyTap ->
      let name = make_fresh_tap_name () in
      make_system_tap name;
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
          "Adding pattern %s %s\n"
          (string_of_client client)
          (string_of_daemon_resource_pattern resource_pattern);
        let resource = make_system_resource resource_pattern in
        Printf.printf "Adding %s %s\n" (string_of_client client) (string_of_daemon_resource resource);
        resource_map#add client resource;
      with e -> begin
        Printf.printf "Failed (%s) when adding %s %s; continuing anyway.\n"
          (Printexc.to_string e)
          (string_of_client client) (string_of_daemon_resource_pattern resource_pattern);
      end);;

(** Destroy the given resource. Synchronization is performed inside this function,
    hence the caller doesn't need to worry about it: *)
let destroy_resource client resource =
  with_mutex the_daemon_mutex
    (fun () ->
      try
        Printf.printf "Removing %s %s\n" (string_of_client client) (string_of_daemon_resource resource);
        destroy_system_resource resource;
        resource_map#remove_key_value_or_fail client resource
      with e -> begin
        Printf.printf "Failed (%s) when removing %s %s; continuing anyway.\n"
          (Printexc.to_string e)
          (string_of_client client) (string_of_daemon_resource resource);
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

let clients_no = ref 0;;
let the_bridge_if_any = ref None;;
let increment_clients_no () =
  with_mutex the_daemon_mutex
    (fun () ->
      (if !clients_no = 0 then begin
        assert(!the_bridge_if_any = None);
        the_bridge_if_any := Some (make_system_resource AnyBridge);
      end);
      clients_no := !clients_no + 1);;
let decrement_clients_no () =
  with_mutex the_daemon_mutex
    (fun () ->
      clients_no := !clients_no - 1;
      (if !clients_no = 0 then begin
        match !the_bridge_if_any with
        | None -> assert false
        | Some bridge -> begin
            destroy_system_resource bridge;
            the_bridge_if_any := None;
        end;
      end));;

let make_client client =
  with_mutex the_daemon_mutex
    (fun () ->
      (* First add any number to the data structure, then call keep_alive_client to make
         the death time correct: *)
      Printf.printf "Creating %s.\n" (string_of_client client); flush_all ();
      client_death_time_map#add client 42.42;
      keep_alive_client client;
      increment_clients_no ();
      Printf.printf "Created %s.\n" (string_of_client client); flush_all ()
    );;

let destroy_client client =
  with_mutex the_daemon_mutex
    (fun () ->
      Printf.printf "Killing %s.\n" (string_of_client client); flush_all ();
      client_death_time_map#remove client;
      destroy_all_client_resources client;
      decrement_clients_no ();
      Printf.printf "%s was killed.\n" (string_of_client client); flush_all ());;

let debugging_thread_thunk () =
  while true do
    Thread.delay 5.0;
    with_mutex the_daemon_mutex
      (fun () ->
        Printf.printf "---------------------------------\n";
        Printf.printf "Currently existing resources are:\n";
        List.iter
          (fun (client, resource) ->
            Printf.printf "* %s (owned by %s)\n" (string_of_daemon_resource resource) (string_of_client client))
          (resource_map#to_alist);
        Printf.printf "---------------------------------\n";
        flush_all ();
        );
    flush_all ();
  done;;  

let timeout_thread_thunk () =
  while true do
    Thread.delay timeout_interval;
    let client_death_times = client_death_time_map#to_alist in
    let current_time = Unix.time () in
    Printf.printf "It's now %f.\n" current_time;
    List.iter
      (fun (client, death_time) ->
        if current_time >= death_time then begin
          Printf.printf "It's about time to kill the client %s\n" (string_of_client client);
          destroy_client client;
          Printf.printf "The client %s was killed with success.\n" (string_of_client client);
          flush_all ();
        end else begin
          Printf.printf "It's not yet time to kill the client %s\n" (string_of_client client);
          flush_all ();
        end)
      client_death_times;
    flush_all ();
  done;;

let thread1_thunk () =
  while true do
    Thread.delay 10.0;
    (try
      keep_alive_client 1;
      make_resource 1 AnyTap;
    with e -> begin
      Printf.printf "Failed (%s). Continuning anyway.\n" (Printexc.to_string e);
    end);
    flush_all ();
  done;;  

let main _ =
  make_client 1;
  make_client 2;
  make_client 3;
  make_resource 2 AnyTap;
  make_resource 2 AnyTap;
  make_resource 2 AnyTap;
  make_resource 3 AnyBridge;
  ignore (Thread.create timeout_thread_thunk ());
  ignore (Thread.create debugging_thread_thunk ());
  ignore (Thread.create thread1_thunk ());
  (* ignore (Thread.create thread2_thunk ()); *)
  while true do
    Thread.delay 10.0;
  done;;

let connection_server socket =
  let buffer = String.make message_length ' ' in
  Printf.printf "Hello from a connection server thread\n"; flush_all ();
  while true do
    Printf.printf "Reading...\n"; flush_all ();
    Printf.printf "I received >%s<\n" buffer; flush_all ();
    let actual_length = Unix.read socket buffer 0 message_length in
    if actual_length < message_length then begin
      Printf.printf "I ignore this message: its length is %i\n" actual_length;
      flush_all ();
    end
    else begin
      Printf.printf "Ok, I will process this message\n";
    end;
  done;;

let server () =
  let port = 12347 in 
  let connections_no_limit = 10 in
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let host = Unix.gethostbyname (Unix.gethostname ()) in
  let h_addr = host.Unix.h_addr_list.(0) in
  let sock_addr = Unix.ADDR_INET(h_addr, port) in
  Printf.printf "I am waiting at %s:%i\n" (Unix.gethostname ()) port; flush_all ();
  Unix.bind sock sock_addr;
  Unix.listen sock connections_no_limit;
  while true do 
    Printf.printf "accepting...\n"; flush_all ();
    let (service_sock, client_sock_addr) = Unix.accept sock in
    Printf.printf "accepted.\n"; flush_all ();
    ignore (Thread.create connection_server service_sock) 
  done;;

server ();;
