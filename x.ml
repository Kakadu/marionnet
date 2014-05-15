(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007, 2008  Luca Saiu
   Copyright (C) 2011  Jean-Vincent Loddo
   Copyright (C) 2007, 2008, 2011  Université Paris 13

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

let fail x = failwith (Printf.sprintf "Ill-formed DISPLAY string: '%s'" x)
;;

(** The syntax of $DISPLAY is: [host]:display[.screen] *)
let get_host_display_screen_from_string x =
 let split_rigth_part y =
   match (StringExtra.split ~d:'.' y) with
   | [ display; screen ] -> (display, screen)
   | [ display ]         -> (display, "0")
   | _ -> fail x
 in
 let host, (display, screen) =
   match (StringExtra.split ~d:':' x) with
   | [ host; right_part ] -> host, (split_rigth_part right_part)
   | [ right_part ]       -> "localhost", (split_rigth_part right_part)
   | _ -> fail x
 in
 let strip_and_use_default_if_empty ~default x=
   let x = StringExtra.strip x in
   if x = "" then default else x
 in
 let host    = strip_and_use_default_if_empty ~default:"localhost" host in
 let screen  = strip_and_use_default_if_empty ~default:"0" screen in
 let display = StringExtra.strip display in
 (host, display, screen)
;;

let get_host_display_screen () =
  try
    let x = Sys.getenv "DISPLAY" in
    if x = "" then
      raise Not_found (* It's just like it weren't defined... *)
    else
      get_host_display_screen_from_string x
  with Not_found ->
    failwith "The environment variable DISPLAY is not defined or empty, and Marionnet requires X.\nBailing out...";;

(* Global variables: *)
let host, display, screen = get_host_display_screen ()
;;

let get_cookie_by_xauth ?(display="0") ?(screen="0") () : string option =
  let command = Printf.sprintf "xauth list :%s.%s" display screen in
  let result, code = UnixExtra.run (command) in
  if code <> Unix.WEXITED 0 then None else (* continue: *)
  let xss = StringExtra.Text.Matrix.of_string result in (* [["socrates/unix:12"; "MIT-MAGIC-COOKIE-1"; "0fca956092856af8f4cfae3951f837e7"]] *)
  match xss with
  | [hostname; "MIT-MAGIC-COOKIE-1"; cookie]::_ -> Some cookie
  | _ -> None

(* Global variable set to the MIT-MAGIC-COOKIE-1 of the current display and screen: *)
let mit_magic_cookie_1 : string option =
  get_cookie_by_xauth ~display ~screen ()

(* Just an alias: *)
let cookie = mit_magic_cookie_1

let _last_used_local_display_index =
  ref 0;;

let mutex = Mutex.create ();;

(* Useful for xnest: *)
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


(* Note that this function really tries to establish a connection (which is immediately closed).
   Do not use with a one-shot service (it must accept more than one connection): *)
let is_local_service_open ?(host_addr:string option) ~(port:int) () =
  let host_addr = match host_addr with
    | None     -> Unix.inet_addr_loopback
    | Some str -> Unix.inet_addr_of_string str
  in
  try
    let (in_channel, out_channel) = Unix.open_connection (Unix.ADDR_INET(host_addr, port)) in
    Unix.shutdown_connection in_channel;
    true
  with
    Unix.Unix_error (Unix.ECONNREFUSED, _,_) -> false
  ;;


(* Global variables: *)
let host_addr = Unix.string_of_inet_addr ((Unix.gethostbyname host).Unix.h_addr_list.(0))
and port = 6000 + (try (int_of_string display) with _ -> 0)
;;

(* Global variable: *)
let is_X_server_listening_TCP_connections =
  is_local_service_open ~host_addr ~port ()
;;

Log.printf7
  "---\nHost X data from $DISPLAY:\nHost: %s\nHost address: %s\nDisplay: %s\nScreen: %s\nCookie: %s\nListening on port %d: %b\n---\n"
  host host_addr
  display
  screen
  (Option.extract_or cookie "None")
  port
  is_X_server_listening_TCP_connections
;;

exception No_problem ;;
exception No_listening_server ;;

(*
$ socat TCP-LISTEN:6000,fork,reuseaddr,range=172.23.0.254 UNIX-CONNECT:/tmp/.X11-unix/X0  & # local connection
$ socat TCP-LISTEN:6000,fork,reuseaddr,range=172.23.0.254 TCP:202.54.1.5:6003             & # DISPLAY=202.54.1.5:3
*)
let fix_X_problems : unit =
  let socketfile = Printf.sprintf "/tmp/.X11-unix/X%s" display in
  let socketfile_exists = Sys.file_exists socketfile in
  let no_fork = None (* Yes fork, i.e. create a process for each connection *) in
  (* let no_fork = Some () (* use Marionnet's threads *) in *)
  let range4 = "172.23.0.0/24" in
  match is_X_server_listening_TCP_connections, host_addr with

  (* Case n°1: an X server runs on localhost:0 and accepts TCP connection: *)
  | true,  "127.0.0.1" when port=6000 ->
      Log.printf "(case 1) No X problems have to be fixed: connection seems working fine. Ok.\n"

  (* Case n°2: an X server runs on localhost and accepts TCP connection,
      but on a display Y<>0. We morally set up a PAT (Port Address Translation)
      172.23.0.254:6000 -> 127.0.0.1:(6000+Y) simply using the unix socket.
      In this way, the virtual machines setting DISPLAY=172.23.0.254:0 will be
      able to connect to the host X server: *)
  | true,  "127.0.0.1" when port<>6000 && socketfile_exists ->
      (* Equivalent to: socat TCP-LISTEN:6000,fork,reuseaddr UNIX-CONNECT:/tmp/.X11-unix/X? *)
      Log.printf1 "(case 2) Starting a socat service: 0.0.0.0:6000 -> %s\n" socketfile;
      ignore (Network.Socat.inet4_of_unix_stream_server ?no_fork ~range4 ~port:6000 ~socketfile ())

  (* Case n°3: an X server seems to run on localhost accepting TCP connection,
      but the display is Y<>0 and there isn't a corresponding unix socket.
      This is quite unusual: we are probably in a ssh -X connection.
      We have to pay attention to the fact that processes asking for a connexion
      are not from the machine 127.0.0.1 but are from the virtual machines 172.23.0.0/24.
      Note that the following command doesn't solve completely the problem: we have also to
      provide the X cookies in ~/.Xauthority to the virtual machines. *)
  | true,  "127.0.0.1" when port<>6000 && (not socketfile_exists) ->
      (* Equivalent to: socat TCP-LISTEN:6000,fork,reuseaddr TCP:host_addr:port *)
      Log.printf2 "(case 3) Starting a socat service: 0.0.0.0:6000 -> %s:%d\n" host_addr port;
      ignore (Network.Socat.inet4_of_inet_stream_server ?no_fork ~range4 ~port:6000 ~ipv4_or_v6:host_addr ~dport:port ())

  (* Case n°4: probably a telnet or a ssh -X connection.
      Idem: the following command doesn't solve completely the problem: we have also to
      provide the X cookies in ~/.Xauthority to the virtual machines.    *)
  | true,  _  (* when host_addr<>"127.0.0.1" *) ->
      (* Equivalent to: socat TCP-LISTEN:6000,fork,reuseaddr TCP:host_addr:port *)
      Log.printf2 "(case 4) Starting a socat service: 0.0.0.0:6000 -> %s:%d\n" host_addr port;
      ignore (Network.Socat.inet4_of_inet_stream_server ?no_fork ~range4 ~port:6000 ~ipv4_or_v6:host_addr ~dport:port ())

  (* Case n°5: an X server seems to run on localhost but it doesn't accept TCP connections.
      We simply redirect connection requests to the unix socket: *)
  | false, "127.0.0.1" when socketfile_exists ->
      (* Equivalent to: socat TCP-LISTEN:6000,fork,reuseaddr UNIX-CONNECT:/tmp/.X11-unix/X? *)
      Log.printf1 "(case 5) Starting a socat service: 0.0.0.0:6000 -> %s\n" socketfile;
      ignore (Network.Socat.inet4_of_unix_stream_server ?no_fork ~range4 ~port:6000 ~socketfile ())

  | false, _ ->
      Log.printf "(case 6) Warning: X connections are not available for virtual machines.\n"
;;

(** This has to be performed *early* in the initialization process: *)
let _ = GtkMain.Main.init ();;

(** This is a workaround for some threading issues suggested by Jacques Garrigue;
    it's needed to be able to use the 'run' method in GTK and Glade objects
    without preventing other unrelated threads to run: *)
let _ =
  GMain.Timeout.add ~ms:100 ~callback:(fun () -> Thread.delay 0.001; true);;
