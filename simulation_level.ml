(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007, 2008, 2009  Luca Saiu
   Copyright (C) 2009, 2010  Jean-Vincent Loddo
   Copyright (C) 2007, 2008, 2009, 2010  Université Paris 13

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

open Gettext;;

#load "include_type_definitions_p4.cmo"
;;
INCLUDE DEFINITIONS "simulation_level.mli" ;;

open Treeview_defects;;
open Daemon_language;;
open X;; (* Not really needed: this works around a problem with OCamlBuild 3.10.0 *)

module Recursive_mutex = MutexExtra.Recursive ;;

(** Fork a process which just sleeps forever without doing any output. Its stdout is
    perfect to be used as stdin for processes created with create_process which wait
    for input from an interactive console, and exit when their stdin is closed *)
let make_input_for_spawned_processes () =
  let (an_input_descriptor_never_sending_anything, _) = Unix.pipe () in
  an_input_descriptor_never_sending_anything;;

let an_input_descriptor_never_sending_anything =
  make_input_for_spawned_processes ();

(** {2 Lower-level interface to device-simulating processes} *)

(** What {e may} happen when the user tries to interact with a process which is
    not in the right state, for example trying to stop a non-spawned process.
    Note however that not all such errors are caught {e here}. This class is
    "unsafe" from this point of view, but it becomes safe when incapsulated
    within a [device] class (see the comments about DFA states below).
    It makes no sense to implement {e two} distinct protection mechanisms
    performing the same checks *)
exception ProcessIsntInTheRightState of string;;

(** This is used to spawn and control a concurrent Unix process: *)
class virtual process =
fun program
    (arguments : string list)
    ?stdin:(stdin=Unix.stdin)
    ?stdout:(stdout=Unix.stdout)
    ?stderr:(stderr=Unix.stderr)
    ~unexpected_death_callback
    ()
  ->
  let basename = Filename.basename program in
  object(self)

  val mutable arguments = arguments
  method append_arguments xs = (arguments <- List.append arguments xs)

  val pid : int option ref = ref None

  (** Get the spawn process pid, or fail if the process has
      not been spawn yet: *)
  method get_pid =
    match !pid with
      (Some p) -> p
    | _ -> raise (ProcessIsntInTheRightState "get_pid")

  (** Get the spawn process pid, as an option: *)
  method get_pid_option =
    !pid

  (** Startup the process using command_line, and return its pid *)
  method spawn =
    match !pid with
      (Some _) ->
        raise (ProcessIsntInTheRightState "spawn")
    | None ->
        let _just_for_logging =
          let cmdline = String.concat " " (program::arguments) in
          Log.printf3 "process#spawn: `%s' called with %d arguments; the complete command line is:\n---\n%s\n---\n"
            basename
            (List.length arguments)
            (StringExtra.fmt ~tab:2 ~width:60 cmdline)
        in
        let new_pid =
          Unix.create_process
            program
            (Array.of_list (program :: arguments))
            stdin
            stdout
            stderr
        in
        pid := (Some new_pid);
        self#start_thread_waiting ~current_pid:new_pid;
        Death_monitor.start_monitoring new_pid program unexpected_death_callback;
        Log.printf2
          "process#spawn: a process (%s) was just spawned (pid %i).\n"
          basename
          new_pid

  method stop_monitoring ?(current_pid=self#get_pid) () =
    try
      Log.printf2 ~v:2 "process#stop_monitoring: about to call the Death_monitor for %s (pid %i)\n"
        basename current_pid;
      Death_monitor.stop_monitoring current_pid;
      Log.printf2 ~v:2 "process#stop_monitoring: exited from Death_monitor for %s (pid %i). Success.\n"
        basename current_pid;
    with _ ->
      () (* We allow to 'stop monitoring' a process more than once *)

  (** Return true iff the process is currently alive; this checks the actual running
      process, independently from the automaton state: *)
  method is_alive =
    try
      UnixExtra.is_process_alive self#get_pid
    with _ ->
      false (* self#get_pid failed *)

  (** Wait for 'seconds' or until pid dies, whatever occours first: *)
  method private allow_for_some_seconds_to_die seconds =
    let time = ref 0.0 in
    let interval = 0.1 in
    try
      while (self#is_alive) && !time < seconds do
        Thread.delay interval;
        time := !time +. interval;
      done;
    with _ ->
      ()

  (** Kill the process with a SIGINT. This forbids any
      interaction, until the process is started again: *)
  method terminate =
    match !pid with
      Some current_pid ->
        self#stop_monitoring ~current_pid ();
        Thread.delay 0.1;
        self#kill_with_signal ~current_pid Sys.sigint;
        Thread.delay 0.1;
        self#kill_with_signal ~tries:10 ~delay:0.1 ~current_pid Sys.sigkill;
        pid := None
    | None ->
        raise (ProcessIsntInTheRightState "terminate")

  method private start_thread_waiting ~current_pid =
    ignore
      (Thread.create
         (fun () ->
            ignore (UnixExtra.Process.waitpid_non_intr current_pid);
            Log.printf1 "process#start_thread_waiting: waitpid %d exited.\n" current_pid)
          ())

  (** Note that this does *not* affect death monitoring. *)
  method private kill_with_signal ?(tries=1) ?(delay=0.1) ~current_pid signal =
   let rec loop i =
    begin
      if not (UnixExtra.is_process_alive current_pid) then () else (* continue: *)
      Log.printf1 ~v:2 "process#kill_with_signal: about to kill %d...\n" current_pid;
      (try
	(* Send the signal: *)
	Unix.kill current_pid signal;
	Log.printf1 ~v:2 "process#kill_with_signal: pid %d killed. Success.\n" current_pid;
      with _ ->
	Log.printf1 ~v:2 "process#kill_with_signal: failed to kill pid %d.\n" current_pid
      );
      if i >= tries then () else (Thread.delay delay; loop (i+1))
      end
   in
   loop 1

  (** By default gracefully_terminate is just an alias for terminate.
      Of course some subclasses can override it to do something different *)
  method gracefully_terminate =
    self#terminate

  (** Stop the process with a SIGSTOP. This forbids any interaction, until
      self#continue is called: *)
  method stop =
    match !pid with
      (Some p) ->
        Unix.kill p Sys.sigstop
    | None ->
        raise (ProcessIsntInTheRightState "stop")

  (** Make a stopped process continue, with a SIGCONT. *)
  method continue =
    match !pid with
      (Some p) ->
        Unix.kill p Sys.sigcont
    | None ->
        raise (ProcessIsntInTheRightState "continue")

  initializer
    (** Not really safe, but it can be useful for debugging: terminate a
        process when its OCaml object gets GC'd: *)
    Gc.finalise
      (fun process ->
(*        Log.printf "GC'ing a process object. I hope it's not running :-)\n"; *)
        try process#terminate with _ -> ())
      self
end;;

(** Sometimes we aren't interested in the input or the output of some program *)
let dev_null_in = Unix.descr_of_in_channel (open_in "/dev/null");;
let dev_null_out = Unix.descr_of_out_channel (open_out "/dev/null");;

(** {2 Example of low-level interaction} *)

(** Play with xeyes for ten seconds, then terminate it:
{[let _ =
    let p = new process "xeyes" [] () in
    p#spawn;
    Log.printf "%d\n" (p#get_pid);
    Thread.delay 10.0;
    p#terminate;;]} *)

(** {2 Implementation of stuff simulated with Unix processes} *)

(** Generate a unique identifier for a switch process *)
let gensym = Counter.make_int_generator ();;

class xnest_process =
  fun ?(host_name_as_client=X.host)
      ?(display_as_client=X.display)
      ?(screen_as_client=X.screen)
      ?(display_number_as_server=X.get_unused_local_display ())
      ~unexpected_death_callback
      ~title
      () ->
object(self)
  inherit process
      "Xephyr"
      [ "-ac"; (* this is temporary; or should I leave it this way? *)
(*         "-name"; title; *)
        "-nolock";
        "-screen"; "800x600";
        "-nozap";
        "+kb"; (* Enable the X keyboard extension *)
        display_number_as_server ]
(*      "Xnest"
      [ "-display";
        (Printf.sprintf "%s:%s.%s" host_name_as_client display_as_client screen_as_client);
        "-ac"; (* this is temporary; or should I leave it this way? *)
        "-name"; title;
        display_number_as_server ] *)
      ~stdin:dev_null_in
      ~stdout:dev_null_out
      ~stderr:dev_null_out
      ~unexpected_death_callback
      ()
      as super

  method display_string_as_client =
    Printf.sprintf "%s:%s.%s" host_name_as_client display_as_client screen_as_client

  method display_number_as_server =
    display_number_as_server
end;;

class reserved_socket_name ~prefix ~program () =
 let motherboard = Motherboard.extract () in
 let socket_name =
  UnixExtra.temp_file
    ~parent:motherboard#project_working_directory
    ~prefix
    ()
 in
 object (self)
  method name = socket_name

  method exists =
    let redirection = Global_options.Debug_level.redirection () in
    let command_line =
      Printf.sprintf "grep \"%s\" /proc/net/unix %s" socket_name redirection
    in
    (Unix.system command_line = (Unix.WEXITED 0))

  method unlink = (try Unix.unlink socket_name with _ -> ())

  initializer
    Log.printf2 "reserved_socket_name#initializer:\n\tsocket name \"%s\" reserved for %s\n" socket_name program;
    self#unlink;

end (* reserved_socket_name *)

(** Process creating a socket. Spawning and terminating methods are specific. *)
class virtual process_which_creates_a_socket_at_spawning_time =
 fun program
    (arguments : string list)
    ?stdin
    ?stdout
    ?stderr
    ?(socket_name_prefix="socket-")
    ?management_socket
    ~unexpected_death_callback
    () ->

 object(self)
  inherit process program arguments ?stdin ?stdout ?stderr ~unexpected_death_callback ()
  as super

  val listening_socket  =
    new reserved_socket_name
      ~prefix:socket_name_prefix
      ~program
      ()

  val management_socket =
    let prefix = socket_name_prefix^"mgmt-" in
    Option.map
      (new reserved_socket_name ~prefix ~program)
      management_socket

  method private management_socket_unused_or_exists =
    match management_socket with
    | None   -> true
    | Some s -> s#exists

  (** Return the automatically-generated Unix socket name. The name is generated
      once and for all at initialization time, so this method can be safely used
      also before spawning the process. *)
  method get_socket_name = listening_socket#name
  method get_management_socket_name = Option.map (fun s -> s#name) management_socket

  method private sockets_have_been_created =
    listening_socket#exists && self#management_socket_unused_or_exists

  (** vde_switch_processes need to be up before we connect cables or UMLs to
      them, so they have to be spawned in a *synchronous* way: *)
  method spawn =
    Log.printf1 "process_w_c_a_socket_at_s_time#spawn: spawning the process which will create the socket %s\n" (Shell.escaped_filename self#get_socket_name);
    super#spawn;
    (* We also check that the process is alive: if spawning it failed than the death
       monitor will take care of everything it's needed and destroy the device: in
       this case we just exit and let the death monitor clean up after us. *)
    while self#is_alive && not (self#sockets_have_been_created) do
      (* The socket is not ready yet, but the process is up: let's wait and then
         check again: *)
      Thread.delay 0.05;
      Log.printf "process_w_c_a_socket_at_s_time#spawn: the process has not created the socket yet.\n";
    done;
    Log.printf "process_w_c_a_socket_at_s_time#spawn: Ok, the socket now exists. Spawning succeeded.\n";
    (* This should not be needed, but we want to play it super-safe for the first public
       release: *)
    Thread.delay 0.3;

  (** We want to be absolutely sure to remove the socket, so we also send a SIGKILL to the
      process and explicitly delete the file: *)
  method terminate =
    super#terminate;
    (* super#kill_with_signal Sys.sigkill;*)
    listening_socket#unlink;
    Option.iter (fun s -> s#unlink) management_socket;

end;; (* class process_which_creates_a_socket_at_spawning_time *)

(** This is used to implement Switch, Hub, Hublet and Gateway Hub processes.
    Only Unix socket is used as a transport if no tap_name is specified: *)
class vde_switch_process =
 fun ?hub:(hub:bool=false)
     ?port_no:(port_no:int=32)
     ?tap_name
     ?socket_name_prefix
     ?management_socket
     ?fstp
     ?rcfile
     ~unexpected_death_callback
     () ->
 let socket_name_prefix =
  match socket_name_prefix with
  | Some p -> p
    | None -> Printf.sprintf "%s-socket-" (if hub then "hub" else "switch")
 in
 object(self)
  inherit process_which_creates_a_socket_at_spawning_time
      (Initialization.Path.vde_prefix ^ "vde_switch")
      (let arguments =
         let tap_name_related =
           match tap_name with
           | None -> []
           | Some tap_name -> ["-tap"; tap_name]
         in
         let hub_related = (if hub then ["-x"] else []) in
         let port_no_related = [ "-n"; (string_of_int (port_no + 1)) ] in
         (* TODO: find a reasonable value for this: *)
         let permissions_related = [ "-mod"; "777" ] in
         let fstp_related = (if fstp=Some () then ["--fstp"] else []) in
         let rcfile_related =
           match rcfile with
           | None -> []
           | Some rcfile -> ["--rcfile"; rcfile]
         in
         List.concat [
           tap_name_related;
           hub_related;
           port_no_related;
           permissions_related;
           fstp_related;
           rcfile_related;
           ]
      in arguments)
      ~stdin:an_input_descriptor_never_sending_anything
      ~stdout:dev_null_out
      ~stderr:dev_null_out
      ~socket_name_prefix
      ?management_socket
      ~unexpected_death_callback
      ()
  initializer
    let optional_mgmt =
      List.concat
        (Option.to_list
           (Option.map (fun name -> ["--mgmt"; (Shell.escaped_filename name)]) self#get_management_socket_name))
    in
    self#append_arguments ("-unix" :: (Shell.escaped_filename self#get_socket_name) :: optional_mgmt);

end;; (* class vde_switch_process *)

(** A Swtich process is, well, a Switch or Hub process but not a hub: *)
class switch_process =
  fun ~(port_no:int)
      ?socket_name_prefix
      ?management_socket
      ~unexpected_death_callback
      () ->
object(self)
  inherit vde_switch_process
      ~hub:false
      ~port_no
      ?socket_name_prefix
      ?management_socket
      ~unexpected_death_callback
      ()
      as super
end;;

(** A Hub process is, well, a Switch or Hub process and also a hub *)
class hub_process =
  fun ~(port_no:int)
      ?socket_name_prefix
      ?management_socket
      ~unexpected_death_callback
      () ->
object(self)
  inherit vde_switch_process
      ~hub:true
      ~port_no
      ?socket_name_prefix
      ?management_socket
      ~unexpected_death_callback
      ()
      as super
end;;

(** A Hublet process is just a Hub process with exactly two ports *)
class hublet_process =
  fun ?index
      ~unexpected_death_callback
      () ->
  let socket_name_prefix = match index with
  | None   -> "hublet-socket-"
  | Some i -> Printf.sprintf "hublet-%i-socket-" i
  in
  object(self)
   inherit hub_process
      ~port_no:2
      ~socket_name_prefix
      ~unexpected_death_callback
      ()
      as super
end;;


(** This is used to implement the world gateway component. *)
class slirpvde_process =
  fun ?network
      ?dhcp
      ~existing_socket_name
      ~unexpected_death_callback
      () ->

  let network = match network with
   | None -> [] (* slirpvde sets by default 10.0.2.0 *)
   | Some n  -> ["--network"; n ]
  in
  let dhcp = match dhcp with
   | None -> []
   | Some () -> ["--dhcp" ] (* turn on the DHCP server *)
  in
  let arguments = List.concat [
       [ "--mod";  "777";       (* To do: find a reasonable value for this *) ];
       [ "--unix"; (Shell.escaped_filename existing_socket_name) ];
       network;
       dhcp;
       ]
  in
  object(self)
   inherit process
      (Initialization.Path.vde_prefix ^ "slirpvde")
      arguments
      ~stdin:an_input_descriptor_never_sending_anything
      ~stdout:dev_null_out
      ~stderr:dev_null_out
      ~unexpected_death_callback
      ()
end;; (* class slirpvde_process *)


(** This is used to implement the switch component. *)
class unixterm_process =
  fun ?xterm_title
      ~management_socket_name
      ~unexpected_death_callback
      () ->
  let xterm_title = match xterm_title with
   | None    -> []
   | Some t  -> ["-T"; t]
  in
  let unixterm = (Initialization.Path.vde_prefix ^ "unixterm") in
  let command_launched_by_xterm =
    Printf.sprintf
       "%s %s"
       unixterm (Shell.escaped_filename management_socket_name)
  in
  (* Redefined if rlwrap, ledit or rlfe are installed: *)
  let command_launched_by_xterm =
   try
     let wrapper =
       (* TODO: move in Initialization: *)
       List.find (fun p -> (UnixExtra.path_of_implicit p)<>None) ["ledit"; "rlfe"; "rlwrap"; ]
     in
     Printf.sprintf "%s %s" wrapper command_launched_by_xterm
   with
     Not_found -> command_launched_by_xterm
  in
  let arguments = List.concat [
       xterm_title;
       [ "-e"; command_launched_by_xterm ];
       ]
  in
  object(self)
   inherit process
      "xterm"
      arguments
      ~stdin:an_input_descriptor_never_sending_anything
      ~stdout:dev_null_out
      ~stderr:dev_null_out
      ~unexpected_death_callback
      () as super

end;; (* class unixterm_process *)

(** Return a list of option arguments to be passed to wirefilter in order to implement
    the given defects: *)
let defects_to_command_line_options
  ?(rightward_loss=0.0)
  ?(rightward_duplication=0.0)
  ?(rightward_flip=0.0)
  ?(rightward_min_delay=0.0)
  ?(rightward_max_delay=0.0)
  ?(leftward_loss=0.0)
  ?(leftward_duplication=0.0)
  ?(leftward_flip=0.0)
  ?(leftward_min_delay=0.0)
  ?(leftward_max_delay=0.0)
  () =
  [ "-l"; Printf.sprintf "LR%f" rightward_loss;
    "-D"; Printf.sprintf "LR%f" rightward_duplication;
    "--noise"; Printf.sprintf "LR%i" (truncate (rightward_flip /. 100.0 *. 1024.0 *. 1024.0 *. 8.0));
    "-d"; Printf.sprintf "LR%f+%f" rightward_min_delay rightward_max_delay;
    "-l"; Printf.sprintf "RL%f" leftward_loss;
    "-D"; Printf.sprintf "RL%f" leftward_duplication;
    "--noise"; Printf.sprintf "RL%i" (truncate (leftward_flip /. 100.0 *. 1024.0 *. 1024.0 *. 8.0));
    "-d"; Printf.sprintf "RL%f+%f" leftward_min_delay leftward_max_delay; ];;

(** The process used to implement a Cable *)
class ethernet_cable_process =
  fun ~left_end
      ~right_end
      ?(blinker_thread_socket_file_name=None)
      ?(left_blink_command=None)
      ?(right_blink_command=None)
      ?(rightward_loss=0.0)
      ?(rightward_duplication=0.0)
      ?(rightward_flip=0.0)
      ?(rightward_min_delay=0.0)
      ?(rightward_max_delay=0.0)
      ?(leftward_loss=0.0)
      ?(leftward_duplication=0.0)
      ?(leftward_flip=0.0)
      ?(leftward_min_delay=0.0)
      ?(leftward_max_delay=0.0)
      ~unexpected_death_callback
      () ->
object(self)
  inherit process
      (Initialization.Path.vde_prefix ^ "wirefilter")
      ((List.fold_left List.append []) (* append all the lists within the given list *)
         [ (match left_blink_command with
             Some(c) -> [](* [ "-L"; c ] *) (* !!! old blinking support *)
           | None -> []);
           (match right_blink_command with
             Some(c) -> [](* [ "-R"; c ] *) (* !!! old blinking support *)
           | None -> []);
           (match blinker_thread_socket_file_name with
             Some socket_file_name -> (* [ "-S"; socket_file_name ] *) (* !!! old blinking support *)
               [ "--blink"; socket_file_name;
                 "--blinkid";
                 "(" ^
                 (match left_blink_command with Some s -> s | _ -> "(id: -1; port: -1)") ^
                 "" ^
                 (match right_blink_command with Some s -> s | _ -> "(id: -1; port: -1)") ^
                 ")" ]
           | None -> []);
           defects_to_command_line_options
             ~rightward_loss
             ~rightward_duplication
             ~rightward_flip
             ~rightward_min_delay
             ~rightward_max_delay
             ~leftward_loss
             ~leftward_duplication
             ~leftward_flip
             ~leftward_min_delay
             ~leftward_max_delay
             ();
           [ "-v"; ((Shell.escaped_filename left_end#get_socket_name) ^ ":" ^ (Shell.escaped_filename right_end#get_socket_name)) ]])
      ~stdin:an_input_descriptor_never_sending_anything
      ~stdout:dev_null_out
      ~stderr:dev_null_out
      ~unexpected_death_callback
      ()
      as super

end;;

(* Simplified constructor. Defects are accessible by objects: *)
let make_ethernet_cable_process
  ~left_end
  ~right_end
  ?blinker_thread_socket_file_name
  ?left_blink_command
  ?right_blink_command
  ~(leftward_defects:defects_object)
  ~(rightward_defects:defects_object)
  ~unexpected_death_callback
  () =
  let leftward = leftward_defects in
  let rightward = rightward_defects in
  new ethernet_cable_process
    ~left_end
    ~right_end
    ?blinker_thread_socket_file_name
    ?left_blink_command
    ?right_blink_command
    ~rightward_loss:rightward#loss
    ~rightward_duplication:rightward#duplication
    ~rightward_flip:rightward#flip
    ~rightward_min_delay:rightward#min_delay
    ~rightward_max_delay:rightward#max_delay
    ~leftward_loss:leftward#loss
    ~leftward_duplication:leftward#duplication
    ~leftward_flip:leftward#flip
    ~leftward_min_delay:leftward#min_delay
    ~leftward_max_delay:leftward#max_delay
    ~unexpected_death_callback
    ()
;;

let ethernet_interface_to_boot_parameters_bindings umid port_index hublet =
  let port_index_as_string = string_of_int port_index in
  let ifconfig = Treeview_ifconfig.extract () in
  let make_port_binding ~prefix ~key =
    ((prefix ^ port_index_as_string), (ifconfig#get_port_attribute_by_index umid port_index key))
  in
  (* We treat the ipv4_address in a special way because some small distributions (e.g. buildroot)
     may have a version of `ifconfig' which doesn't support the CIDR notation. So, we split the
     address/cidr into the address and netmask: *)
  let ipv4_address_related_bindings =
    let x = ifconfig#get_port_attribute_by_index umid port_index "IPv4 address" in
    match (Ipv4.String.is_valid_ipv4 x), (Ipv4.String.is_valid_config x) with
    | false, false -> []
    | true,  false -> [(("ipv4_address_eth" ^ port_index_as_string), x)]
    | false, true  ->
        let config  : ((int*int*int*int) * int) = Ipv4.config_of_string x in
        let address = Ipv4.to_string (fst config) in
        let netmask = Ipv4.to_string (Ipv4.netmask_of_cidr (snd config)) in
        [(("ipv4_address_eth" ^ port_index_as_string), address);
         (("ipv4_netmask_eth" ^ port_index_as_string), netmask) ]
    | true, true -> [] (* assert false *)
  in
  List.append
    (ipv4_address_related_bindings)
    [
     make_port_binding "mac_address_eth"  "MAC address";
     make_port_binding "mtu_eth"          "MTU";
     make_port_binding "ipv4_gateway_eth" "IPv4 gateway";
     make_port_binding "ipv6_address_eth" "IPv6 address";
     make_port_binding "ipv6_gateway_eth" "IPv6 gateway";
    ]
  ;;

(** Convert the tuple we use to represent information about an ethernet interface
    into a command line argument for UML *)
let ethernet_interface_to_uml_command_line_argument umid port_index hublet =
  let ifconfig = Treeview_ifconfig.extract () in
  "eth" ^ (string_of_int port_index) ^ "=daemon," ^
  (ifconfig#get_port_attribute_by_index umid port_index "MAC address") ^
  ",unix," ^ (Shell.escaped_filename (hublet#get_socket_name)) ^ "/ctl";;

let random_ghost_mac_address () =
  let random () = Printf.sprintf "%02x" (Random.int 256) in
  let octet0 = "42" in
  let octet1 = "42" in
  let octet2 = random () in
  let octet3 = random () in
  let octet4 = random () in
  let octet5 = random () in
  Printf.sprintf "%s:%s:%s:%s:%s:%s" octet0 octet1 octet2 octet3 octet4 octet5;;

(** Create a fresh sparse file name for swap and return it: *)
let create_swap_file_name ~parent =
  UnixExtra.temp_file
    ~parent
    ~prefix:"sparse-swap-"
    ();;

(** The UML process used to implement machines and routers: *)
class uml_process =
  fun ~(kernel_file_name)
      ?(kernel_console_arguments:string option)
      ~(filesystem_file_name)
      ?(filesystem_relay_script:string option)
      ~(dynamically_get_the_cow_file_name_source:unit->string option)
      ~(cow_file_name)
      ~states_directory
      ?swap_file_name
      ~(ethernet_interface_no)
      ~(hublet_processes)
      ~(memory) (* in megabytes *)
      ~(console_no)
      ~(console)
      ?umid:(umid="uml-" ^ (string_of_int (gensym ())))
      ~id
      ?(show_unix_terminal=false)
      ?xnest_display_number
      ?(guestkind="machine") (* or "router" *)
      ~unexpected_death_callback
      () ->
  let motherboard = Motherboard.extract () in
  let swap_file_name =
    match swap_file_name with
    | None -> create_swap_file_name ~parent:motherboard#project_working_directory
    | Some f -> f
  in
  let debug_mode =
    Global_options.Debug_level.are_we_debugging ()
  in
  let console =
    (* Always use an xterm in debug mode: *)
    if debug_mode || show_unix_terminal then
      "xterm"
    else
      (* Don't show the xterm console if we're using an Xnest, in non-debug mode. *)
      match xnest_display_number with
        Some xnest_display_number -> "none"
      | None -> console
  in
  let hostfs_pathname =
    Printf.sprintf "%s/hostfs/%i" (Filename.dirname (Filename.dirname cow_file_name)) id
  in
  let boot_parameters_pathname =
    Printf.sprintf "%s/boot_parameters" hostfs_pathname
  in
  let truncated_id = id mod 65535 in
  let octet2 = truncated_id / 255 in
  let octet3 = truncated_id mod 254 in
  let ip42 = Printf.sprintf "172.23.%i.%i" octet2 octet3 in
  let _ = Log.printf2 "uml_process: creating %s: eth42 has IP %s\n" umid ip42 in
  let tap_name =
    match Daemon_client.ask_the_server
            (Make (AnyTap((Unix.getuid ()),
                          (* "172.23.0.254" *) ip42))) with
    | Created (Tap tap_name) ->
        tap_name
    | _ ->
        "wrong-tap-name"
  in
  (* Basic parameters: *)
  let command_line_arguments =
    List.append
      (List.map
         (fun (ei, h) -> ethernet_interface_to_uml_command_line_argument umid ei h)
         (List.combine (ListExtra.range 0 (ethernet_interface_no - 1)) hublet_processes))
      [
       "ubda=" ^ (Shell.escaped_filename cow_file_name) ^ "," ^ (Shell.escaped_filename filesystem_file_name);
       "ubdb=" ^ (Shell.escaped_filename swap_file_name);
       "umid=" ^ umid;
       "mem=" ^ (string_of_int memory) ^ "M";
       "root=98:0";
       "hostfs=" ^ (Shell.escaped_filename hostfs_pathname);
       "hostname="^umid;
       "guestkind="^guestkind;
       "xterm="^Initialization.marionnet_terminal;
       (* Ghost interface configuration. The IP address is relative to a *host* tap: *)
       "eth42=tuntap,"^tap_name^","^(random_ghost_mac_address ())^",172.23.0.254";
     ]
  in
  (* Exam *)
  let command_line_arguments =
    if Initialization.are_we_in_exam_mode then
      "exam=1" :: command_line_arguments
    else
      command_line_arguments
  in
  (* xnest_display_number *)
  let command_line_arguments =
    match xnest_display_number with
    | None                      -> command_line_arguments
    | Some xnest_display_number -> ("xnest_display_number="^xnest_display_number)::command_line_arguments
  in
  (* keyboard_layout *)
  let command_line_arguments =
    match Global_options.keyboard_layout with
    | None                 -> command_line_arguments
    | Some keyboard_layout -> ("keyboard_layout="^keyboard_layout)::command_line_arguments
  in
  (* timezone (something like "Europe/Paris") *)
  let command_line_arguments =
    match Initialization.marionnet_timezone with
    | None          -> command_line_arguments
    | Some timezone -> ("timezone="^timezone)::command_line_arguments
  in
  (* numeric_TZ (something like "+02:00") *)
  let command_line_arguments =
    let numeric_TZ = Shell.date ~arg:"+%:z" () in
    ("numeric_TZ="^numeric_TZ)::command_line_arguments
  in
  (* console_no *)
  let command_line_arguments =
    ("console_no="^(string_of_int console_no))::command_line_arguments
  in
  (* Some examples:
     "con=none"; "con6=port:9000"; "ssl1=port:9001"; "ssl2=tty:/dev/tty42"; "ssl3=pts"; *)
  let console_related_arguments =
    match kernel_console_arguments with
    | Some args ->
        let () = Log.printf2 "uml_process: creating %s: using specific console arguments: %s\n" umid args in
        [args]
    | None ->
        (* Undesirable situation: the couple (kernel, filesystem) is not well
           configured. We try however to deduce the good arguments simply looking
           the kernel version: *)
        match StrExtra.First.matchingp (Str.regexp "linux-2[.]6[.]") kernel_file_name with
        | true  ->
            let () = Log.printf1 "uml_process: creating %s: using default console arguments for old pairs filesystem/kernels\n" umid in
            [ "con=none"; "ssl="^console; "console=ttyS0" ]
        | false ->
            let () = Log.printf1 "uml_process: creating %s: using default console arguments for new pairs filesystem/kernels\n" umid in
            [ "con0="^console; ]
  in
  let command_line_arguments =
    command_line_arguments @ console_related_arguments
  in
  object(self)
  inherit process
      kernel_file_name
      command_line_arguments
      ~stdin:dev_null_in
      ~stdout:dev_null_out
      ~stderr:dev_null_out
      ~unexpected_death_callback
      () as super

  method swap_file_name =
    swap_file_name

  val swap_file_size =
    1024 * 1024 (* 1 Gb *)

  method create_swap_file =
    try
      let dd_command_line =
        Printf.sprintf
          "dd if=/dev/zero bs=1024 seek=%i count=1 of='%s'"
          swap_file_size
          swap_file_name
      in
      Log.system_or_fail dd_command_line;
      Log.printf2 "%s#create_swap_file: created the swap file %s.\n" umid swap_file_name;
      let mkswap_command_line =
        Printf.sprintf "export PATH=$PATH:/sbin:/usr/sbin:/usr/local/sbin; mkswap '%s'" swap_file_name
      in
      Log.system_or_fail mkswap_command_line;
      Log.printf2 "%s#create_swap_file: executed mkswap on the swap file %s.\n" umid swap_file_name;
    with e -> begin
      Log.printf3 "%s#create_swap_file: WARNING: swap file %s creation failed (this might be serious): %s\n" umid swap_file_name (Printexc.to_string e);
    end

  method delete_swap_file =
    try
      Log.system_or_fail (Printf.sprintf "rm -f '%s'" swap_file_name);
      Log.printf2 "%s#delete_swap_file: deleted the swap file '%s'\n" umid swap_file_name;
    with e -> begin
      Log.printf2 ~v:2 "%s#delete_swap_file: WARNING: removing the swap file '%s' failed.\n" umid swap_file_name;
    end

(*   (\** There is a specific and better way to stop a UML processes, using *)
(*       mconsole. We (transparently, thanks to late binding) support it *\) *)
(*   method stop = *)
(*     ignore (Unix.system ("uml_mconsole " ^ umid ^ " stop 1>/dev/null 2>/dev/null")); *)

(*   (\** There is a specific and better way to continue a UML processes, using *)
(*       mconsole. We (transparently, thanks to late binding) support it *\) *)
(*   method continue = *)
(*     ignore (Unix.system ("uml_mconsole " ^ umid ^ " go 1>/dev/null 2>/dev/null")); *)

  method private gracefully_terminate_with_mconsole ?(command="cad") ?(tries=1) ?(delay=1.) () : bool =
    (* let redirection = Global_options.Debug_level.redirection () in *)
    let redirection = "1>/dev/null 2>/dev/null" in (* anyway silently *)
    let cmdline = Printf.sprintf "uml_mconsole %s %s %s" umid command redirection in
    let rec loop i =
      if i > tries then false (* abandon *) else (* retry *)
      let status = Unix.system cmdline in
      if status = (Unix.WEXITED 0)
        then
          begin
            Log.printf3
              "%s#gracefully_terminate: uml_mconsole succeeded in sending a '%s' to %s. Ok.\n"
              umid command umid;
            true (* success *)
          end
        else
          begin
            Log.printf5
              "%s#gracefully_terminate: uml_mconsole failed in sending a '%s' to %s. Trying again (loop no. %d/%d)...\n"
              umid command umid i tries;
            Thread.delay delay;
            loop (i+1)
          end
    in
    loop 1

  method private kill_descendants_then_myself ~pid =
   try
     Linux.Process.kill_descendants
       ~signal_sequence:[Sys.sigkill] ~wait_delay:0. ~node_max_retries:2 ~root_max_retries:2 ~pid ();
     Unix.kill pid Sys.sigkill;
   with _ -> ()

  (** There is a specific and better way to terminate a UML processes, using
      mconsole. Note that `#terminate' (not overridden here) remains useful as a
      more drastic solution *)
  method gracefully_terminate =
   match !pid with
   | None -> raise (ProcessIsntInTheRightState "gracefully_terminate")
   | Some current_pid ->
       Log.printf2 "%s#gracefully_terminate: about to terminate !!! the UML process with pid %d...\n" umid current_pid;
       let descendants : int list = Linux.Process.get_descendants ~pid:current_pid () in
       (* We set here a sort of timeout: we will wait no more than 30 seconds to kill the whole hierarchy.
          This is very ugly, but needed: sometimes uml_console succeeds when sending a 'cad'
          message, but the UML process is just in an early stage of boot, and ignores the
          message. *)
       let _ =
         Thread.create
           begin fun delay ->
              (Thread.delay delay);
              (if UnixExtra.is_process_alive (current_pid)
                then self#kill_descendants_then_myself ~pid:current_pid
                else ());
              (* Kill anyway remaining descendants which may be now orphan: *)
              (List.iter (fun pid -> try Unix.kill pid Sys.sigkill with _ -> ()) descendants);
           end
           (30.) (* timeout: no more than 30 seconds *)
       in
       (* Action 1: release some resources (in a distinct thread): *)
       Log.printf2 "%s#gracefully_terminate: about to stop monitoring pid %d...\n" umid current_pid;
       self#stop_monitoring ~current_pid ();
       (* Action 2: tell UML to terminate, cleanly with `cad': *)
       let uml_console_succeeded =
         self#gracefully_terminate_with_mconsole ~command:"cad" ~tries:5 ~delay:1. ()
       in
       (* Action 3: else, tell UML to terminate, quite brutally with `halt': *)
       let uml_console_succeeded =
         if uml_console_succeeded then true else (* try with command "halt": *)
         self#gracefully_terminate_with_mconsole ~command:"halt" ~tries:3 ~delay:2. ()
       in
       (* Action 4: else, kill all descendants and the process brutally: *)
       let () =
         if uml_console_succeeded then () else
           begin
             (* try without mconsole, directly killing all descendants then `current_pid': *)
             Log.printf2 "%s#gracefully_terminate: killing whole hierarchy of pid %d with SIGKILL...\n" umid current_pid;
             self#kill_descendants_then_myself ~pid:current_pid;
           end
       in
       (* Wait for the process to die: *)
       let () =
	 try begin
	  Log.printf2 "%s#gracefully_terminate: waiting pid %d...\n" umid current_pid;
	  ignore (UnixExtra.Process.waitpid_non_intr current_pid);
	  Log.printf2 "%s#gracefully_terminate: pid %d correctly waited. Fine.\n" umid current_pid;
	 end with e ->
	  begin
	    Log.printf3 "%s#gracefully_terminate: pid %d uncorrectly waited: %s\n"
	      umid current_pid (Printexc.to_string e);
	  end
       in
       (* Remove other resources: *)
       begin
	Log.printf2 "%s#gracefully_terminate: removing swap file allocated for %d\n" umid current_pid;
	self#delete_swap_file;
	Log.printf2 "%s#gracefully_terminate: asking to remove tap allocated for %d\n" umid current_pid;
	let _ = Daemon_client.ask_the_server (Destroy (Tap tap_name)) in
	(* Remember that now there's no process any more: *)
	pid := None;
	Log.printf2 "%s#gracefully_terminate: UML process with pid %d successfully terminated.\n" umid current_pid;
       end


  (** UML processes are not always very willing to die, and sometimes react to signals by
      going into infinite loops keeping a CPU 100% busy. But this should always work: *)
  method terminate =
   match !pid with
   | None -> raise (ProcessIsntInTheRightState "terminate")
   | Some current_pid ->
       let _ = self#stop_monitoring ~current_pid () in
       let _ = self#gracefully_terminate_with_mconsole ~command:"sysrq e" () in
       let _ = self#gracefully_terminate_with_mconsole ~command:"sysrq i" () in
       let uml_console_succeeded = self#gracefully_terminate_with_mconsole ~command:"halt" ~tries:1 () in
       let () =
         if uml_console_succeeded then () else
           begin
             (* try without mconsole, directly killing all descendants then `current_pid': *)
             Log.printf2 "%s#terminate: killing whole hierarchy of pid %d with SIGKILL...\n" umid current_pid;
             self#kill_descendants_then_myself ~pid:current_pid;
           end
       in
       let () = begin
         self#allow_for_some_seconds_to_die 2.0;
         (* The process has not complied yet after interval seconds. Kill it the hard way: *)
         (try
           while self#is_alive do
             self#allow_for_some_seconds_to_die 2.0;
             self#kill_with_signal ~current_pid Sys.sigint;
             self#allow_for_some_seconds_to_die 2.0;
             self#kill_with_signal ~tries:5 ~delay:0.1 ~current_pid Sys.sigkill;
           done;
         with _ -> ());
        end
       in
       (* Wait for the process to die: *)
       let () =
	 try begin
	  Log.printf2 "%s#terminate: waiting pid %d...\n" umid current_pid;
	  ignore (UnixExtra.Process.waitpid_non_intr current_pid);
	  Log.printf2 "%s#terminate: pid %d correctly waited. Fine.\n" umid current_pid;
	 end with e ->
	  begin
	    Log.printf3 "%s#terminate: pid %d uncorrectly waited: %s\n"
	      umid current_pid (Printexc.to_string e);
	  end
       in
       (* Remove other resources: *)
       begin
	Log.printf2 "%s#terminate: removing swap file allocated for %d\n" umid current_pid;
	self#delete_swap_file;
	Log.printf2 "%s#terminate: asking to remove tap allocated for %d\n" umid current_pid;
	let _ = Daemon_client.ask_the_server (Destroy (Tap tap_name)) in
	(* Remember that now there's no process any more: *)
	pid := None;
	Log.printf2 "%s#terminate: UML process with pid %d successfully terminated.\n" umid current_pid;
       end

  method hostfs_directory_pathname =
    hostfs_pathname

  (** Create the host directory shared by hostfs and its content, if they don't already
      exist; otherwise do nothing. *)
  method private make_hostfs_stuff_if_not_already_present =
    (* Make the directory: *)
    (try
      Unix.mkdir
        self#hostfs_directory_pathname
        0o777 (* a+rwx; To do: this should be made slightly more restrictive... *);
    with _ -> ());
    (* Now fill this directory: *)
    (* Copy the `filesystem_relay_script' if exists: *)
    let () =
      Option.iter
        (fun relay ->
           let dest = Filename.concat (self#hostfs_directory_pathname) (Filename.basename relay) in
           UnixExtra.file_copy relay dest)
        (filesystem_relay_script)
    in
    (* Create the file `boot_parameters_pathname': *)
    let descriptor =
      Unix.openfile boot_parameters_pathname [Unix.O_WRONLY; Unix.O_CREAT] 0o777
    in
    let out_channel = Unix.out_channel_of_descr descriptor in
    let write (name, value) = Printf.fprintf out_channel "%s='%s'\n" name value in
    (* --- *)
    Option.iter (fun x -> write ("mit_magic_cookie_1",x)) X.mit_magic_cookie_1;
    (* --- *)
    List.iter
      write
      (* Here we leave "ethernet_interfaces_no" instead of "ethernet_interface_no" *)
      (("ethernet_interfaces_no", (string_of_int ethernet_interface_no)) ::
       (List.append
          (List.flatten
             (List.map
                (fun (ei, h) -> ethernet_interface_to_boot_parameters_bindings umid ei h)
                (List.combine (ListExtra.range 0 (ethernet_interface_no - 1)) hublet_processes)))
          [(* A non-standard binding we use to identify the IP address of eth42 in the guest: *)
           "ip42", ip42;
           (* A non-standard binding we use to pass the virtual machine name to the guest: *)
           "hostname", umid;
         ]));
    flush_all ();
    (try
      close_out out_channel;
      Unix.close descriptor; (* To do: understand which one is really needed. *)
    with _ -> ())

  (** Destroy the host directory shared by hostfs. This should only be called at machine
     deletion time. *)
  method remove_hostfs_directory =
    ignore (Unix.system (Printf.sprintf "rm -rf '%s'" self#hostfs_directory_pathname));
    ()

  method private grant_host_x_server_access =
    let redirection = Global_options.Debug_level.redirection () in
    try
      ignore (Unix.system ("xhost +" ^ ip42 ^ " " ^ redirection))
    with _ -> begin
      Log.printf2 "%s#grant_host_x_server_access: WARNING: granting host X server access to %s failed.\n" umid ip42
    end

(*  method private revoke_host_x_server_access =
    let redirection = Global_options.Debug_level.redirection () in
    try
      ignore (Unix.system ("xhost -" ^ ip42 ^ " " ^redirection))
    with _ -> begin
      Log.printf "WARNING: revoking host X server access to %s failed.\n" ip42
    end*)

  method private copy_cow_file_if_needed =
    match dynamically_get_the_cow_file_name_source () with
    | None -> ()
    | Some source_pathname ->
        ignore
	  (Cow_files.duplicate_cow_file_into_states_directory
	    ~source_pathname
	    ~states_directory
	    ~cow_file_name
	    ())

  (** When spawning the UML machine we automatically grant it access to the host X
      server and make the swap file for it: *)
  method spawn =
    self#copy_cow_file_if_needed;
    self#grant_host_x_server_access;
    self#create_swap_file;
    super#spawn

  initializer
    self#make_hostfs_stuff_if_not_already_present
end;;

(** {2 Generic simulation infrastructure} *)

(** Make a device state printable *)
let device_state_to_string s =
  match s with
    Off -> "off" | On -> "on" | Sleeping -> "sleeping" | Destroyed -> "destroyed";;

(** What happens the user tries to follow a non-existing DFA transition *)
exception CantGoFromStateToState of device_state * device_state;;

(** The base class of simulated devices. Either one parameter or the other one must be
    supplied (note the ugly hack in which which both parameters are shadows to compute
    the one which was not passed).
    Note how here a device may also be a cable or a machine. This is different from the
    convention in [mariokit.ml].
    Note that hublets are created {e once and for all} at construction time, and
    destroyed {e only} when the method destroy is invoked.
    We always have to avoid destroying hublets connected to running cable processes. *)
class virtual ['parent] device
 ~(parent:'parent)
 ~hublet_no (* TODO: remove it, use instead parent#get_port_no *)
 ~(unexpected_death_callback: unit -> unit)
 ()
 =
 let make_hublet_process_array ~unexpected_death_callback ~size =
   Array.init
     size
     (fun index ->
        new hublet_process
              ~index
              ~unexpected_death_callback
              ())
 in
 object(self)
  (** The internal state, as a DFA state *)
  val mutable state = Off
  method get_state = state

  method virtual device_type : string

  (** Port associated hublets: *)
  val mutable hublet_process_array = [||]
(*  method get_hublet_process_array = hublet_process_array*)
  method get_hublet_no = Array.length hublet_process_array
  method get_hublet_process_list = Array.to_list hublet_process_array
  method get_hublet_process_of_port port_index (* 0-based *) =
    Array.get hublet_process_array port_index


  method private make_and_spawn_the_hublet_process_array =
    hublet_process_array <-
      make_hublet_process_array
        ~unexpected_death_callback:self#execute_the_unexpected_death_callback
        ~size:hublet_no (*parent#get_port_no*) ;
    Array.iter (fun sp -> sp#spawn) hublet_process_array; (* TODO: this doesn't seem necessary *)

  (* We have to use the inizializer instead of binding 'hublet_processes' before
     'object', just because we need to use 'self' for unexpected_death_callback: *)
  initializer
    self#make_and_spawn_the_hublet_process_array;

  method private terminate_hublets =
    let name = parent#get_name in
    Array.iter
      (fun sp ->
         let pid = try sp#get_pid with _ -> -1 in
         Log.printf2 "device#terminate_hublets: terminating a device hublet process (pid %i) of %s...\n" pid name;
         (try sp#continue with _ -> ());
         (try sp#terminate with _ -> ());
         Log.printf2  "device#terminate_hublets: ok, a hublet process (pid %i) of %s was terminated\n" pid name)
      hublet_process_array;

  (** This is just to allow some implicit type conversions... *)
  method hostfs_directory_pathname : string =
    failwith ("hostfs_directory_pathname is not available for a " ^ self#device_type)

  (** Transitions are implemented with a simple change of internal state
      (which may fail if the current state is not appropriate for the
      transition). The actual interaction with device-simulating processes
      is performed in subclasses. *)
  method startup =
    match state with
    | Off -> state <- On; self#spawn_processes
    | _   -> failwith "can't startup a non-off device"

  method shutdown =
    match state with
    | On -> state <- Off; (try self#terminate_processes with _ -> ())
    | _  -> failwith "can't shutdown a non-on device"

  method gracefully_shutdown =
    match state with
    | On -> state <- Off; self#gracefully_terminate_processes
    | _  -> failwith "can't gracefully_shutdown a non-on device"

  method suspend =
    match state with
    | On -> state <- Sleeping; self#stop_processes
    | _  -> failwith "can't suspend a non-on device"

  method resume =
    match state with
    | Sleeping -> state <- On; self#continue_processes
    | _        -> failwith "can't resume a non-sleeping device"

  (** Terminate all processes including hublets, and set the device state in an
      unescapable 'destroyed' state. This is useful when the device is modified in
      a way that alter connections with other devices, and a simple restart is not
      enough to boot the device again in a usable state *)
  method destroy =
    let name = parent#get_name in
    Log.printf1 "device#destroy: resuming %s before destruction...\n" name;
    (try self#resume with _ -> ());
    Log.printf1  "device#destroy: shutting down %s before destruction...\n" name;
    (try self#shutdown with _ -> ());
    Log.printf1  "device#destroy: about to terminate %s's hublets...\n" name;
    self#terminate_hublets;
    Log.printf1 "device#destroy: Ok, the hublets of %s were destroyed.\n" name;
    state <- Destroyed;

  method (* protected *) execute_the_unexpected_death_callback pid process_name =
    let process_name = Filename.basename process_name in
    let title = (s_ "A process died unexpectedly") in
    let message =
      Printf.sprintf
        (f_ "The process %s with pid %i allowing the simulation of %s %s died unexpectedly. It was necessary %s \"%s\" to maintain a consistent state.")
        process_name
        pid
        self#device_type
        parent#get_name
        (if self#device_type = "Ethernet cable" then (s_ "to restart") else (s_ "to stop"))
        parent#get_name
    in
    (* Run the actual callback, and warn the user: *)
    unexpected_death_callback ();
    Simple_dialogs.warning title message ();


  (** Work with the 'main' processes implementing the device. This may be
      complex for some devices, and sometimes more than a single process
      is involved; what should be done varies according to how the device
      is simulated with processes, and on how each process depends on each
      other.
      Note that hublets are *not* involved in this. *)
  method virtual spawn_processes : unit

  method virtual terminate_processes : unit
  method virtual stop_processes : unit
  method virtual continue_processes : unit

  (** This is just an alias of terminate_processes by default, but of course
      some subclasses may override it to do something different *)
  method gracefully_terminate_processes =
    self#terminate_processes
end;;


(** The common schema for user-level hubs, switches and gateways: *)
class virtual ['parent] main_process_with_n_hublets_and_cables
  ~(parent:'parent)
  ~hublet_no
  ?(last_user_visible_port_index=(hublet_no-1))
  ~unexpected_death_callback
  ()
  =
object(self)
  inherit ['parent] device
      ~parent
      ~hublet_no
      ~unexpected_death_callback
      ()
      as super

  val mutable main_process = None
  method private get_main_process =
    match main_process with
    | Some p -> p
    | None -> assert false

  val internal_cable_processes = ref []
  method get_internal_cable_processes = !internal_cable_processes

  (** Switches and hubs are stateless from the point of view of the user, and it makes no
      sense to "suspend" them. This also helps implementation :-) *)
  method spawn_processes =
    (* Spawn the main switch process, and wait to be sure it's started: *)
    self#get_main_process#spawn;
    (* Create internal cable processes from main switch to hublets, and spawn them: *)
    let () =
      internal_cable_processes :=
	let hublets = self#get_hublet_process_list in
	let name = parent#get_name in
	Log.printf3 "main_process_with_n_hublets_and_cables#spawn_processes: device=%s hublet_no=%d last_user_visible_port_index=%d\n" name hublet_no last_user_visible_port_index;
	List.map
	  (fun (i, hublet_process) ->
	    if i <= last_user_visible_port_index then
	      make_ethernet_cable_process
		~left_end:self#get_main_process
		~right_end:hublet_process
		~leftward_defects:(parent#ports_card#get_my_inward_defects_by_index i)
		~rightward_defects:(parent#ports_card#get_my_outward_defects_by_index i)
		~unexpected_death_callback:self#execute_the_unexpected_death_callback
		()
	      else (* Hidden ports have no defects: *)
	      begin
	      new ethernet_cable_process
		~left_end:self#get_main_process
		~right_end:hublet_process
		~unexpected_death_callback:self#execute_the_unexpected_death_callback
		()
	      end
	      )
	  (List.combine
	    (ListExtra.range 0 (hublet_no-1))
	    hublets)
    in
    (* Now we can spawn the internal cables: *)
    self#spawn_internal_cables

  (* WARNING: cables must be created sequentially in order to make the mapping
     between VDE and marionnet port numbering deterministic.
     So, do not perform Task_runner.do_in_parallel but simply iter.
     Note that this method *must be refined* for switches in order to have the same port
     numbering in the vde_switch internal state (relevant for the VLAN management). *)
  method spawn_internal_cables =
    List.iter (fun thunk -> thunk ())
      (List.map (* Here map returns a list of thunks *)
         (fun internal_cable_process () -> internal_cable_process#spawn)
         !internal_cable_processes)


  method terminate_processes =
    (* Terminate internal cables and the main switch process: *)
    Task_runner.do_in_parallel
      ((fun () -> self#get_main_process#terminate)
       ::
       (List.map (* here map returns a list of thunks *)
          (fun internal_cable_process () -> internal_cable_process#terminate)
          !internal_cable_processes));
    (* Unreference cable processes: *)
    internal_cable_processes := [];

 method stop_processes =
    self#get_main_process#stop

  method continue_processes =
    self#get_main_process#continue

end;; (* class main_process_with_n_hublets_and_cables *)


(** Add some accessory processes running together with the main process. *)
class virtual ['parent] main_process_with_n_hublets_and_cables_and_accessory_processes =
  fun ~(parent:'parent)
      ~hublet_no
      ?(last_user_visible_port_index:int option)
      ~unexpected_death_callback
      () ->
 object(self)

  inherit ['parent] main_process_with_n_hublets_and_cables
      ~parent
      ~hublet_no
      ?last_user_visible_port_index
      ~unexpected_death_callback
      ()
      as super

  val mutable accessory_processes = []
  method add_accessory_process (p:process) =
    accessory_processes <- p::accessory_processes

  method private terminate_accessory_processes =
    List.iter (fun p -> try p#terminate with _ -> ()) accessory_processes

  method private spawn_accessory_processes =
    List.iter (fun p -> p#spawn) (List.rev accessory_processes)

  method private stop_accessory_processes =
    List.iter (fun p -> p#stop) accessory_processes

  method private continue_accessory_processes =
    List.iter (fun p -> p#continue) (List.rev accessory_processes)

  (* Redefined: *)
  method destroy =
   self#terminate_accessory_processes;
   super#destroy

  (* Redefined: *)
  method gracefully_shutdown =
   self#terminate_accessory_processes;
   super#gracefully_shutdown

  method spawn_processes =
    super#spawn_processes;
    self#spawn_accessory_processes;

  method terminate_processes =
    self#terminate_accessory_processes;
    super#terminate_processes;

  method stop_processes =
    self#stop_accessory_processes;
    super#stop_processes;

  method continue_processes =
    super#continue_processes;
    self#continue_accessory_processes;

end;;


(** This class implements {e either} a hub or a switch; their implementation
    is nearly identical, so this is convenient. *)
class virtual ['parent] hub_or_switch =
  fun ~(parent:'parent)
      ~hublet_no
      ?(last_user_visible_port_index:int option)
      ~(hub:bool)
      ?management_socket
      ?fstp
      ?rcfile
      ~unexpected_death_callback
      () ->
 object(self)

  inherit ['parent] main_process_with_n_hublets_and_cables_and_accessory_processes
      ~parent
      ~hublet_no
      ?last_user_visible_port_index
      ~unexpected_death_callback
      ()
      as super

  initializer
    main_process <-
      Some (new vde_switch_process
              ~hub
              ~socket_name_prefix:
                 (Printf.sprintf "%s-%s-socket-"
                   (if hub then "hub" else "switch")
                   (parent#get_name))
              ~port_no:hublet_no
              ?management_socket
              ?fstp
              ?rcfile
              ~unexpected_death_callback:self#execute_the_unexpected_death_callback
              ())

  method get_management_socket_name =
    (Option.extract main_process)#get_management_socket_name

end;;


(** {2 machine and router implementation} *)

(** This class implements {e either} an machine or a router; their implementation
    is nearly identical, so this is convenient. *)
class virtual ['parent] machine_or_router =
  fun ~(parent:'parent)
      ~(router:bool)
      ~(kernel_file_name)
      ?(kernel_console_arguments)
      ?(filesystem_relay_script)
      ~(filesystem_file_name)
      ~dynamically_get_the_cow_file_name_source
      ~(cow_file_name)
      ~states_directory
      ~(ethernet_interface_no)
      ~(memory) (* in megabytes *)
      ~(console_no)
      ~(console)
      ~xnest
      ?umid:(umid="uml-" ^ (string_of_int (gensym ())))
      ~id
      ?show_unix_terminal
      ~unexpected_death_callback
      () ->
let half_hublet_no = ethernet_interface_no in
object(self)
  (* Outer hublets interface the device with the outer world, but we want to just user
     super#destroy to get rid of *all* hublets; so we declare *all* hublets as part of the
     external interface, even if only the first half will be used in this way. Hence it's
     important the the outer layer comes *first*: *)
  inherit ['parent] device
      ~parent
      ~hublet_no:(half_hublet_no * 2)
      ~unexpected_death_callback
      ()
      as super

  (* Inner hublets interface the UML process with the outer hublets; the cables in
     between simulate port defects in the user-level network: *)
  val inner_hublet_processes = ref []
  method private get_inner_hublet_processes = !inner_hublet_processes

  val outer_hublet_processes = ref []
  method private get_outer_hublet_processes = !outer_hublet_processes

  val uml_process = ref None
  method private get_uml_process =
    match !uml_process with
      None -> failwith "machine_or_router: get_uml_process was called when there's no process"
    | Some uml_process -> uml_process

  val xnest_process = ref None
  method private get_xnest_process =
    match !xnest_process with
      None -> failwith "machine_or_router: get_xnest_process was called when there's no process"
    | Some xnest_process -> xnest_process

  method hostfs_directory_pathname =
    match !uml_process with
      None ->
        failwith "machine_or_router: hostfs_directory_pathname was called when there's no process"
    | Some uml_process ->
        uml_process#hostfs_directory_pathname

  initializer
    let all_hublets = self#get_hublet_process_list in
    outer_hublet_processes :=
      ListExtra.select_from_to all_hublets 0 (half_hublet_no - 1);
    inner_hublet_processes :=
      ListExtra.select_from_to all_hublets (half_hublet_no) (2 * half_hublet_no - 1);
    (if xnest then
      xnest_process :=
        Some (new xnest_process
                ~title:(Printf.sprintf (f_ "Virtual X display of \"%s\"") umid)
                ~unexpected_death_callback:self#execute_the_unexpected_death_callback
                ()));
    uml_process :=
      Some (new uml_process
              ~kernel_file_name
              ?kernel_console_arguments
              ?filesystem_relay_script
              ~filesystem_file_name
              ~dynamically_get_the_cow_file_name_source
              ~cow_file_name
              ~states_directory
              ~ethernet_interface_no
              ~hublet_processes:self#get_inner_hublet_processes
              ~memory
              ~umid
              ~console_no
              ~console
              ~id
              ?show_unix_terminal
              ?xnest_display_number:(
                 if xnest then Some self#get_xnest_process#display_number_as_server
                          else None)
              ~unexpected_death_callback:self#execute_the_unexpected_death_callback
              (* The following parameter will be given to the uml process: *)
              ~guestkind:(if router then "router" else "machine")
              ());

  method terminate_processes = self#terminate_processes_private ~gracefully:false ()
  method gracefully_terminate_processes = self#terminate_processes_private ~gracefully:true ()
  method stop_processes = self#get_uml_process#stop
  method continue_processes = self#get_uml_process#continue

  val internal_cable_processes = ref []

  method spawn_processes =
    (* Create the Xnest, if we have to support it: *)
    (if xnest then
      Task_runner.the_task_runner#schedule
        (fun () -> self#get_xnest_process#spawn(* ; Thread.delay 0.5 *)));
    (* Create internal cable processes connecting the inner layer to the outer layer: *)
    let () =
      (internal_cable_processes :=
	List.map
	  (fun (i, inner_hublet_process, outer_hublet_process) ->
	    make_ethernet_cable_process
		~left_end:inner_hublet_process
		~right_end:outer_hublet_process
		~leftward_defects:(parent#ports_card#get_my_inward_defects_by_index i)
		~rightward_defects:(parent#ports_card#get_my_outward_defects_by_index i)
		~unexpected_death_callback:self#execute_the_unexpected_death_callback
		())
	  (ListExtra.combine3
	    (ListExtra.range 0 (half_hublet_no - 1))
	    self#get_inner_hublet_processes
	    self#get_outer_hublet_processes))
    in
    self#spawn_internal_cables

  method private spawn_internal_cables =
    (* Spawn internal cables processes and the UML process: *)
    Task_runner.do_in_parallel
      ((fun () -> self#get_uml_process#spawn)
       ::
       (List.map (* here map returns a list of thunks *)
          (fun internal_cable_process () -> internal_cable_process#spawn)
          !internal_cable_processes));

  method private terminate_processes_private ~gracefully  () =
    Log.printf1 "machine_or_router#terminate_processes_private: about to terminate the internal cable processes of %s...\n" parent#get_name;
    (* Terminate internal cables and unreference them: *)
    Task_runner.do_in_parallel
      ((fun () ->
        if gracefully then
          self#get_uml_process#gracefully_terminate
        else
          self#get_uml_process#terminate)
       ::
       (List.map (* here map returns a list of thunks *)
          (fun internal_cable_process () -> internal_cable_process#terminate)
          !internal_cable_processes));
    (* Kill the Xnest, if we had started it: *)
    (if xnest then
      self#get_xnest_process#terminate);
    internal_cable_processes := [];

  (** There's no need to override super#destroy. See the comment above. *)
end;;
