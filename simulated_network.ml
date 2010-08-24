(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007, 2008  Luca Saiu

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

open Defects_interface;;
open Daemon_language;;
open X;; (* Not really needed: this works around a problem with OCamlBuild 3.10.0 *)
open Gettext;;

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
  -> object(self)

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
        let cmdline = StringExtra.Fold.blankcat (program::arguments) in
        let basename = Filename.basename program in
        Log.printf "spawning process: the command line for %s is:\n---\n%s\n---\n" basename cmdline;
        let new_pid = (Unix.create_process
                         program
                         (Array.of_list (program :: arguments))
                         stdin
                         stdout
                         stderr) in
        pid := (Some new_pid);
        Death_monitor.start_monitoring new_pid program unexpected_death_callback;
        Log.printf
          "spawning process: a process (%s) was just spawned (pid %i).\n"
          basename
          new_pid

  method stop_monitoring =
    try
      Log.printf "simulated_network: stop_monitoring the process %s (pid %i): BEGIN\n"
        program self#get_pid;
      Death_monitor.stop_monitoring self#get_pid;
      Log.printf "simulated_network: stop_monitoring the process %s (pid %i): SUCCESS\n"
        program self#get_pid;
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
      Some p ->
        self#stop_monitoring;
        self#kill_with_signal Sys.sigint;
        pid := None
    | None ->
        raise (ProcessIsntInTheRightState "terminate")

  (** Note that this does *not* affect death monitoring. *)
  method private kill_with_signal signal =
    let p =
      match !pid with
        Some p -> p
      | None -> raise (ProcessIsntInTheRightState "kill_with_signal") in
    Log.print_string ("About to terminate the process with pid " ^ (string_of_int p) ^ "...\n");
    flush_all ();
    (try
      (* Send the signal: *)
      Unix.kill p signal;
      Log.printf "SUCCESS: terminating the process with pid %i succeeded.\n" p;
    with _ ->
      Log.printf "WARNING: terminating the process with pid %i failed.\n" p);
    (* Wait for the subprocess to die, so that no zombie processes remain: *)
    Log.print_string "OK-W 3\n"; flush_all ();
    (try
      Thread.delay 0.05;
      ignore (ThreadUnix.waitpid [Unix.WNOHANG] p);
    with _ ->
      Log.printf "simulated_network: waitpid %i failed. (9)\n" p);
    if self#is_alive then (* The process didn't die. Try again with something stronger :-) *)
      self#kill_with_signal Sys.sigkill;
    flush_all ();

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
(*        Log.print_string "GC'ing a process object. I hope it's not running :-)\n"; *)
        try process#terminate with _ -> ())
      self
end;;

(** Sometimes we aren't interested in the input or the output of some program *)
let dev_null_in = Unix.descr_of_in_channel (open_in "/dev/null");;
let dev_null_out = Unix.descr_of_out_channel (open_out "/dev/null");;

(** An utility function returning a byte as printed in hexadecimal notation, always
    two digits long. This is handy for MAC addresses *)
let int_byte_to_hex_byte n =
  if n < 0 or n > 255 then
    failwith "out of bounds (1)"
  else
    let string_possibly_of_length_1 = Printf.sprintf "%x" n in
    if String.length string_possibly_of_length_1 = 1 then
      "0" ^ string_possibly_of_length_1
    else
      string_possibly_of_length_1;;

(** An utility function converting a byte in hexadecimal notation into an int.
    This is handy for MAC addresses *)
let hex_byte_to_int_byte h =
  let n = Scanf.sscanf h "%x" (fun q -> q) in
  if n < 0 or n > 255 then
    failwith "out of bounds (2)"
  else
    n;;

(** {2 Example of low-level interaction} *)

(** Play with xeyes for ten seconds, then terminate it:
{[let _ =
    let p = new process "xeyes" [] () in
    p#spawn;
    print_int (p#get_pid); Log.print_string "\n";
    Thread.delay 10.0;
    p#terminate;;]} *)

(** {2 Implementation of stuff simulated with Unix processes} *)

(** Generate a unique identifier for a switch process *)
let gensym = Counter.make_int_generator ();;

class xnest_process =
  fun ?(host_name_as_client=X.host_name)
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

(** Process using a socket. Spawning and terminating methods are specific. *)
class virtual process_with_socket =
 fun program
    (arguments : string list)
    ?stdin
    ?stdout
    ?stderr
    ?(socket_name_prefix="socket-")
    ~unexpected_death_callback
    () ->

 (* Make a unique socket name: *)
 let socket_name =
  UnixExtra.temp_file
    ~parent:(Global_options.get_project_working_directory ())
    ~prefix:socket_name_prefix
    () in
 let _ =
  Log.print_string ("Making a socket at " ^ socket_name ^ "...\n");
  (try Unix.unlink socket_name with _ -> ()) in

 object(self)
  inherit process program arguments ?stdin ?stdout ?stderr ~unexpected_death_callback () as super

  (** Return the automatically-generated Unix socket name. The name is generated
      once and for all at initialization time, so this method can be safely used
      also before spawning the process. *)
  method get_socket_name =
    socket_name

  (** hub_or_switch_processes need to be up before we connect cables or UMLs to
      them, so they have to be spawned in a *synchronous* way: *)
  method spawn =
    Log.printf "Spawning a process with a socket\n"; flush_all ();
    super#spawn;
    let redirection = Global_options.debug_mode_redirection () in
    let command_line =
      Printf.sprintf "grep \"%s\" /proc/net/unix %s" socket_name redirection in
    (* We also check that the process is alive: if spawning it failed than the death
       monitor will take care of everything it's needed and destroy the device: in
       this case we just exit and let the death monitor clean up after us. *)
    while self#is_alive && not (Unix.system command_line = (Unix.WEXITED 0)) do
      (* The socket is not ready yet, but the process is up: let's wait and then
         check again: *)
      Thread.delay 0.05;
      Log.printf "The process has not created the socket yet.\n"; flush_all ();
    done;
    Log.printf "Ok, the socket now exists. Spawning succeeded.\n"; flush_all ();
    (* This should not be needed, but we want to play it super-safe for the first public
       release: *)
    Thread.delay 0.3;

  (** We want to be absolutely sure to remove the socket, so we also send a SIGKILL to the
      process and explicitly delete the file: *)
  method terminate =
    super#terminate;
    super#kill_with_signal Sys.sigkill;
    let command_line =
      Printf.sprintf "rm -f '%s'" self#get_socket_name in
    ignore (Unix.system command_line)

end;; (* class process_with_socket *)

(** This is used to implement Switch, Hub, Hublet and Gateway Hub processes.
    Only Unix socket is used as a transport if no tap_name is specified: *)
class hub_or_switch_process =
  fun ?hub:(hub:bool=false)
      ?ports_no:(ports_no:int=32)
      ?tap_name
      ~unexpected_death_callback
      () ->

 object(self)
  inherit process_with_socket
      (Initialization.vde_prefix ^ "vde_switch")
      (List.append
         (match tap_name with
           None ->
             []
         | Some tap_name ->
             ["-tap"; tap_name])
         (List.append
            (if hub then ["-x"] else [])
            ["-n"; (string_of_int (ports_no + 1));
             "-mod"; "777" (* To do: find a reasonable value for this *);
             ]))
      ~stdin:an_input_descriptor_never_sending_anything
      ~stdout:dev_null_out
      ~stderr:dev_null_out
      ~socket_name_prefix:(if hub then "hub-socket-" else "switch-socket-")
      ~unexpected_death_callback
      ()
  initializer
    self#append_arguments ["-unix"; self#get_socket_name]
end;; (* class hub_or_switch_process *)

(** A Swtich process is, well, a Switch or Hub process but not a hub: *)
class switch_process =
  fun ~(ports_no:int)
      ~unexpected_death_callback
      () ->
object(self)
  inherit hub_or_switch_process
      ~hub:false
      ~ports_no
      ~unexpected_death_callback
      ()
      as super
end;;

(** A Hub process is, well, a Switch or Hub process and also a hub *)
class hub_process =
  fun ~(ports_no:int)
      ~unexpected_death_callback
      () ->
object(self)
  inherit hub_or_switch_process
      ~hub:true
      ~ports_no
      ~unexpected_death_callback
      ()
      as super
end;;

(** A Hublet process is just a Hub process with exactly two ports *)
class hublet_process =
  fun ~unexpected_death_callback
      () ->
object(self)
  inherit hub_process
      ~ports_no:2
      ~unexpected_death_callback
      ()
      as super
end;;

(** A Bridge Socket Hub process is just a Hub process with exactly two ports,
    of which the first one is connected to the given host tun/tap interface: *)
class bridge_socket_hub_process =
  fun ~tap_name
      ~unexpected_death_callback
      () ->
object(self)
  inherit hub_or_switch_process
      ~ports_no:2
      ~hub:true
      ~tap_name
      ~unexpected_death_callback
      ()
      as super
end;;

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
      (Initialization.vde_prefix ^ "wirefilter")
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
           [ "-v"; ((left_end#get_socket_name) ^ ":" ^ (right_end#get_socket_name)) ]])
      ~stdin:an_input_descriptor_never_sending_anything
      ~stdout:dev_null_out
      ~stderr:dev_null_out
      ~unexpected_death_callback
      ()
      as super


  (** This ugly kludge is a workaround for a stability problem in the version of
      wirefitler we use, which may be related to our modifications. We just
      periodically restart wirefilter cables. *)
  val mutex = Recursive_mutex.create ()
  val automatic_reboot_thread = ref None
  method private get_automatic_reboot_thread =
    Recursive_mutex.with_mutex mutex
      (fun () -> !automatic_reboot_thread)
  method private set_automatic_reboot_thread t =
    Recursive_mutex.with_mutex mutex
      (fun () -> automatic_reboot_thread := Some t)
  method private unset_automatic_reboot_thread =
    Recursive_mutex.with_mutex mutex
      (fun () -> automatic_reboot_thread := None)

  method private stop_automatic_reboot_thread =
    (* Do nothing. This is not needed any more, as the reboot thread now checks
       whether it should re-install another thread like itself before dying. *)
    ()
  method private start_automatic_reboot_thread =
    Recursive_mutex.with_mutex mutex
      (fun () ->
        match self#get_automatic_reboot_thread with
          Some thread ->
            (* Do nothing, we already have a thread for this *)
            ()
        | None ->
            (* Log.printf "\n\n(Re-)installing an automatic reboot thread\n\n"; flush_all (); *)
            self#set_automatic_reboot_thread
              (Thread.create
                 (fun () ->
                   (* Sleep for a randomized amount of time; we don't want all automatic
                      reboot threads to wake up together: *)
                   Thread.delay (Random.float (Global_options.automatic_reboot_thread_interval *. 2.0));
                   Recursive_mutex.with_mutex mutex
                     (fun () ->
                       (* I want to avoid the loop because this makes exiting the
                          thread easier.
                          First unconditionally uninstall the thread... *)
                       self#unset_automatic_reboot_thread;
                       (* Now check whether we should re-install it: *)
                       (match self#get_pid_option with
                         Some pid -> begin
                           (if Global_options.get_workaround_wirefilter_problem () then begin
                             Log.printf "*** Rebooting the wirefilter with pid %i\n" pid; flush_all ();
                             (* Restart the cable: *)
                             super#terminate;
                             super#spawn;
                           end);
                           (* Make a new thread for the next check: *)
                           self#start_automatic_reboot_thread;
                         end
                       | None ->
                           ())))
                 ()))

  (** We have to override this in order to be able to manage automatic restarts: *)
  method spawn =
    Recursive_mutex.with_mutex mutex
      (fun () ->
        super#spawn;
        self#start_automatic_reboot_thread)

  (** We have to override this in order to be able to manage automatic restarts: *)
  method terminate =
    Recursive_mutex.with_mutex mutex
      (fun () ->
        super#terminate;
        self#stop_automatic_reboot_thread)

  (** We have to override this in order to be able to manage automatic restarts: *)
  method gracefully_terminate =
    Recursive_mutex.with_mutex mutex
      (fun () ->
        super#gracefully_terminate;
        self#stop_automatic_reboot_thread)

  (** We have to override these just to add synchronization: *)
  method get_pid_option =
    Recursive_mutex.with_mutex mutex
      (fun () ->
        super#get_pid_option)
  method stop =
    Recursive_mutex.with_mutex mutex
      (fun () ->
        super#stop;
        self#stop_automatic_reboot_thread)
  method continue =
    Recursive_mutex.with_mutex mutex
      (fun () ->
        super#continue;
        self#stop_automatic_reboot_thread)
end;;

let ethernet_interface_to_boot_parameters_bindings umid port_index hublet =
(*   let name = Printf.sprintf "eth_%s_eth%i" umid index in *)
  let port_index_as_string = string_of_int port_index in
  let details = Network_details_interface.get_network_details_interface () in
  [
   "mac_address_eth"^port_index_as_string,
   details#get_port_attribute_by_index umid port_index "MAC address";
   "mtu_eth"^port_index_as_string,
   details#get_port_attribute_by_index umid port_index "MTU";
   "ipv4_address_eth"^port_index_as_string,
   details#get_port_attribute_by_index umid port_index "IPv4 address";
   "ipv4_broadcast_eth"^port_index_as_string,
   details#get_port_attribute_by_index umid port_index "IPv4 broadcast";
   "ipv4_netmask_eth"^port_index_as_string,
   details#get_port_attribute_by_index umid port_index "IPv4 netmask";
   "ipv6_address_eth"^port_index_as_string,
   details#get_port_attribute_by_index umid port_index "IPv6 address";
(*   "ipv6_broadcast_eth"^port_index_as_string,
     details#get_port_attribute_by_index umid port_index "IPv6 broadcast";
     "ipv6_netmask_eth"^port_index_as_string,
     details#get_port_attribute_by_index umid port_index "IPv6 netmask"; *)
  ];;

(** Convert the tuple we use to represent information about an ethernet interface
    into a command line argument for UML *)
let ethernet_interface_to_uml_command_line_argument umid port_index hublet =
  let details = Network_details_interface.get_network_details_interface () in
  "eth" ^ (string_of_int port_index) ^ "=daemon," ^
  (details#get_port_attribute_by_index umid port_index "MAC address") ^
  ",unix," ^ (hublet#get_socket_name) ^ "/ctl";;

let zip xs ys =
  let rec zip' xs ys acc =
    match xs, ys with
      [], [] -> List.rev acc
    | (_, []) | ([], _) -> failwith "lists have different length"
    | (x :: rest_of_xs), (y :: rest_of_ys) -> zip' rest_of_xs rest_of_ys ((x, y) :: acc)
  in
  zip' xs ys [];;

let zip3 xs ys zs =
  let rec zip3' xs ys zs acc =
    match xs, ys, zs with
      [], [], [] ->
        List.rev acc
    | ([], _, _) | (_, [], _) | (_, _, []) ->
        failwith "lists have different length"
    | (x :: rest_of_xs), (y :: rest_of_ys), (z :: rest_of_zs) ->
        zip3' rest_of_xs rest_of_ys rest_of_zs ((x, y, z) :: acc)
  in
  zip3' xs ys zs [];;

let random_ghost_mac_address () =
  let octet0 = "42" in
  let octet1 = "42" in
  let octet2 = (int_byte_to_hex_byte (Random.int 256)) in
  let octet3 = (int_byte_to_hex_byte (Random.int 256)) in
  let octet4 = (int_byte_to_hex_byte (Random.int 256)) in
  let octet5 = (int_byte_to_hex_byte (Random.int 256)) in
  Printf.sprintf "%s:%s:%s:%s:%s:%s" octet0 octet1 octet2 octet3 octet4 octet5;;

(** Create a fresh sparse file name for swap and return it: *)
let create_swap_file_name () =
  UnixExtra.temp_file
    ~parent:(Global_options.get_project_working_directory ())
    ~prefix:"sparse-swap-"
    ();;

(** The UML process used to implement machines and routers: *)
class uml_process =
  fun ~(kernel_file_name)
      ~(filesystem_file_name)
      ~(cow_file_name)
      ?(swap_file_name=create_swap_file_name ())
      ~(ethernet_interfaces_no)
      ~(hublet_processes)
      ~(memory) (* in megabytes *)
      ~(console)
      ?umid:(umid="uml-" ^ (string_of_int (gensym ())))
      ~id
      ?xnest_display_number
      ~unexpected_death_callback
      () ->
  let debug_mode =
    Global_options.get_debug_mode () in
  let console =
    (* Always use an xterm in debug mode: *)
    if debug_mode then
      "xterm"
    else
      (* Don't show the xterm console if we're using an Xnest, in non-debug mode. *)
      match xnest_display_number with
        Some xnest_display_number -> "none"
      | None -> console in
  let hostfs_pathname =
    Printf.sprintf "%s/hostfs/%i" (Filename.dirname (Filename.dirname cow_file_name)) id in
  let boot_parameters_pathname =
    Printf.sprintf "%s/boot_parameters" hostfs_pathname in
  let truncated_id = id mod 65535 in
  let octet2 = truncated_id / 255 in
  let octet3 = truncated_id mod 254 in
  let ip42 = Printf.sprintf "172.23.%i.%i" octet2 octet3 in
  let _ = Log.printf "eth42 has IP %s\n" ip42 in
  let tap_name =
    match Daemon_client.ask_the_server
            (Make (AnyTap((Unix.getuid ()),
                          (* "172.23.0.254" *) ip42))) with
    | Created (Tap tap_name) ->
        tap_name
    | _ ->
        "wrong-tap-name" in
  let command_line_arguments =
    List.append
      (List.map
         (fun (ei, h) -> ethernet_interface_to_uml_command_line_argument umid ei h)
         (zip (ListExtra.range 0 (ethernet_interfaces_no - 1)) hublet_processes))
      [
       "ubd0s=" ^ cow_file_name ^ "," ^ filesystem_file_name;
(*        "ubd0s=" ^ filesystem_file_name;  *)
       "ubdb=" ^ swap_file_name;
       "umid=" ^ umid;
       "mem=" ^ (string_of_int memory) ^ "M";
       "root=98:0";
       "hostfs="^hostfs_pathname;
       "hostname="^umid;
       "xterm="^Initialization.marionnet_terminal;
       (* Ghost interface configuration. The IP address is relative to a *host* tap: *)
       (* "eth42=tuntap,,"^(random_ghost_mac_address ())^",172.23.0.254"; *)
       "eth42=tuntap,"^tap_name^","^(random_ghost_mac_address ())^",172.23.0.254";
     ] in
  let command_line_arguments =
    if Command_line.are_we_in_exam_mode then
      "exam=1" :: command_line_arguments
    else
      command_line_arguments in
  let command_line_arguments =
    match xnest_display_number with
      Some xnest_display_number ->
        ("xnest_display_number=" ^ xnest_display_number) :: command_line_arguments
    | None ->
        command_line_arguments in
  let command_line_arguments =
    match Global_options.keyboard_layout with
      None ->
        command_line_arguments
    | Some keyboard_layout ->
        ("keyboard_layout="^keyboard_layout) :: command_line_arguments in
  let command_line_arguments =
    command_line_arguments @
    ["con0=none"; "con1=none"; "con2=none"; "con3=none"; "con4=none"; "con5=none"; "con6=none";
     "ssl0="^console; "ssl1=none"; "ssl2=none"; "ssl3=none"; "ssl4=none"; "ssl5=none"; "ssl6=none";
     "console=ttyS0"] in
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
          "dd if=/dev/zero bs=1024 seek=%i count=1 of=%s"
          swap_file_size
          swap_file_name
      in
      Log.system_or_fail dd_command_line;
      Log.printf "Created the swap file %s.\n" swap_file_name;
      let mkswap_command_line =
        Printf.sprintf "export PATH=$PATH:/sbin:/usr/sbin:/usr/local/sbin; mkswap %s" swap_file_name
      in
      Log.system_or_fail mkswap_command_line;
      Log.printf "Executed mkswap on the swap file %s.\n" swap_file_name;
    with e -> begin
      Log.printf "WARNING: creating the swap file %s failed (this might be serious).\n" swap_file_name;
    end

  method delete_swap_file =
    try
      Log.system_or_fail (Printf.sprintf "rm -f %s" swap_file_name);
      Log.printf "Deleted the swap file %s.\n" swap_file_name;
    with e -> begin
      Log.printf "WARNING: removing the swap file %s failed.\n" swap_file_name;
    end

(*   (\** There is a specific and better way to stop a UML processes, using *)
(*       mconsole. We (transparently, thanks to late binding) support it *\) *)
(*   method stop = *)
(*     ignore (Unix.system ("uml_mconsole " ^ umid ^ " stop 1>/dev/null 2>/dev/null")); *)

(*   (\** There is a specific and better way to continue a UML processes, using *)
(*       mconsole. We (transparently, thanks to late binding) support it *\) *)
(*   method continue = *)
(*     ignore (Unix.system ("uml_mconsole " ^ umid ^ " go 1>/dev/null 2>/dev/null")); *)

  method private gracefully_terminate_with_mconsole =
    self#stop_monitoring;
    ignore (Daemon_client.ask_the_server (Destroy (Tap tap_name)));
    let redirection = Global_options.debug_mode_redirection () in
    Unix.system ("uml_mconsole " ^ umid ^ " cad "^redirection)

  (** There is a specific and better way to terminate a UML processes, using
      mconsole. Note that terminate (not overridden here) remains useful as a
      more drastic solution *)
  method gracefully_terminate =
    self#revoke_host_x_server_access;
    self#delete_swap_file;
    Log.printf "About to gracefully terminate the UML process with pid %i...\n" (self#get_pid);
    (* Tell UML to terminate, cleanly: *)
    let did_we_succeed_immediately = ref true in
    while not (self#gracefully_terminate_with_mconsole = (Unix.WEXITED 0)) do
      Log.print_string ("uml_mconsole failed in sending a 'cad' message to "^ umid ^". Trying again...\n");
      Thread.delay 1.0;
      did_we_succeed_immediately := false;
    done;
    Log.print_string ("Ok, uml_mconsole succeeded in sending a 'cad' message to "^ umid ^".\n");
    if not !did_we_succeed_immediately then begin
      Log.print_string ("Just to make sure (since it didn't work the first time), terminate again with uml_mconsole after ten seconds:\n");
      Thread.delay 10.0;
      ignore (self#gracefully_terminate_with_mconsole);
      Log.print_string ("Ok, done.\n");
    end else begin
      (* This is very ugly, but needed: sometimes uml_console succeeds when sending a 'cad'
         message, but the UML process is just in an early stage of boot, and ignores the
         message. Let's send *another* message after some seconds, just to be on the safe(r)
         side: *)
      let _ =
        Thread.create
          (fun () ->
            Thread.delay 10.0;
            try
              ignore self#gracefully_terminate_with_mconsole
            with _ ->
              ())
          () in
      ();
    end;
    (* Wait for the process to die: *)
(*       Log.print_string "OK-W 7\n"; flush_all (); *)
    (try
      ignore (ThreadUnix.waitpid [] self#get_pid);
    with _ -> begin
      Log.printf "simulated_network: waitpid %i failed. This might be irrelevant. (8)\n" self#get_pid;
      flush_all ()
    end);
(*       Log.print_string "OK-W 8\n"; flush_all (); *)
    flush_all ();
    (* Remember that now there's no process any more: *)
    pid := None

  (** UML processes are not always very willing to die, and sometimes react to signals by
      going into infinite loops keeping a CPU 100% busy. But this should always work: *)
  method terminate =
    self#revoke_host_x_server_access;
    self#delete_swap_file;
    let redirection = Global_options.debug_mode_redirection () in
    match !pid with
      Some p -> begin
        self#stop_monitoring;
        (* Tell the process to halt immediately, without cleanup: *)
        (try ignore (Unix.system ("uml_mconsole " ^ umid ^ " halt "^redirection)) with _ -> ());
        self#allow_for_some_seconds_to_die 2.0;
        (* The process has not complied yet after interval seconds. Kill it the hard way: *)
        (try
          while self#is_alive do
            self#allow_for_some_seconds_to_die 2.0;
            (try self#kill_with_signal Sys.sigint with _ -> ());
            self#allow_for_some_seconds_to_die 2.0;
            (try self#kill_with_signal Sys.sigkill with _ -> ());
          done;
        with _ -> ());
        pid := None;
        ignore (Daemon_client.ask_the_server (Destroy (Tap tap_name)));
      end
    | None ->
        raise (ProcessIsntInTheRightState "terminate")

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
    (* Fill it: *)
    let descriptor =
      Unix.openfile boot_parameters_pathname [Unix.O_WRONLY; Unix.O_CREAT] 0o777 in
    let out_channel =
      Unix.out_channel_of_descr descriptor in
    List.iter
      (fun (name, value) -> Printf.fprintf out_channel "%s='%s'\n" name value)
      (("ethernet_interfaces_no", (string_of_int ethernet_interfaces_no)) ::
       (List.append
          (List.flatten
             (List.map
                (fun (ei, h) -> ethernet_interface_to_boot_parameters_bindings umid ei h)
                (zip (ListExtra.range 0 (ethernet_interfaces_no - 1)) hublet_processes)))
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
    ignore (Unix.system (Printf.sprintf "rm -rf %s" self#hostfs_directory_pathname));
    ()

  method private grant_host_x_server_access =
    let redirection = Global_options.debug_mode_redirection () in
    try
      ignore (Unix.system ("xhost +" ^ ip42 ^ " " ^ redirection))
    with _ -> begin
      Log.printf "WARNING: granting host X server access to %s failed.\n" ip42
    end

  method private revoke_host_x_server_access =
    let redirection = Global_options.debug_mode_redirection () in
    try
      ignore (Unix.system ("xhost -" ^ ip42 ^ " " ^redirection))
    with _ -> begin
      Log.printf "WARNING: revoking host X server access to %s failed.\n" ip42
    end

  (** When spawning the UML machine we automatically grant it access to the host X
      server and make the swap file for it: *)
  method spawn =
    self#grant_host_x_server_access;
    self#create_swap_file;
    super#spawn

  initializer
    self#make_hostfs_stuff_if_not_already_present
end;;

(** {2 Generic simulation infrastructure} *)

(** A device state can be represented as a DFA state *)
type device_state =
    Off
  | On
  | Sleeping
  | Destroyed;;

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
    destroyed {e only} when the method destroy is invoked. *)
class virtual device =
  fun ~name
      ~hublets_no
      ~(unexpected_death_callback: unit -> unit)
      () ->
object(self)
  (** The internal state, as a DFA state *)
  val state = ref Off

  (** Hublet processes are mutable: *)
  val current_hublet_processes =
    ref []

  method virtual device_type : string

  method private make_hublet_processes ~unexpected_death_callback n =
    let hublet_processes =
      List.map
        (fun _ -> new hublet_process ~unexpected_death_callback ())
        (ListExtra.range 1 n) in
(*     Thread.delay 2.0; *)
    hublet_processes

  (* We have to use the inizializer instead of binding 'hublet_processes' before
     'object', just because we need to use 'self' for unexpected_death_callback: *)
  initializer
    let hublet_processes =
      self#make_hublet_processes
        ~unexpected_death_callback:self#execute_the_unexpected_death_callback
        hublets_no in
    List.iter (fun sp -> sp#spawn) hublet_processes;
    current_hublet_processes := hublet_processes

  (** Return the current state *)
  method get_state = !state

  (** This is just to allow some implicit type conversions... *)
  method hostfs_directory_pathname : string =
    failwith ("hostfs_directory_pathname is not available for a " ^ self#device_type)

  (** Transitions are implemented with a simple change of internal state
      (which may fail if the current state is not appropriate for the
      transition). The actual interaction with device-simulating processes
      is performed in subclasses. *)
  method startup =
    match !state with
      Off ->
        state := On;
        self#spawn_processes
    | _ ->
        failwith "can't startup a non-off device"
  method shutdown =
    match !state with
      On ->
        state := Off;
        (try self#terminate_processes with _ -> ())
    | _ ->
        failwith "can't shutdown a non-on device"
  method gracefully_shutdown =
    match !state with
      On ->
        state := Off;
        self#gracefully_terminate_processes
    | _ ->
        failwith "can't gracefully_shutdown a non-on device"
  method suspend =
    match !state with
      On ->
        state := Sleeping;
        self#stop_processes
    | _ ->
        failwith "can't suspend a non-on device"
  method resume =
    match !state with
      Sleeping ->
        state := On;
        self#continue_processes
    | _ ->
       failwith "can't resume a non-sleeping device"

  (** Terminate all processes including hublets, and set the device state in an
      unescapable 'destroyed' state. This is useful when the device is modified in
      a way that alter connections with other devices, and a simple restart is not
      enough to boot the device again in a usable state *)
  method destroy =
    Log.printf "The method destroy has been called on the device %s: begin\n" name;
    Log.printf "Resuming %s before destruction (it might be needed)...\n" name;
    (try self#resume with _ -> ());
    Log.printf  "Shutting down %s before destruction...\n" name;
    (try self#shutdown with _ -> ());
    Log.printf  "Now terminate %s's hublets...\n" name;
    List.iter
      (fun sp ->
        let pid = try sp#get_pid with _ -> -1 in
        Log.printf
          "Terminating a device hublet process (pid %i) of %s to destroy its device...\n"
          pid
          name;
        (try sp#continue with _ -> ());
        (try sp#terminate with _ -> ());
        Log.printf  "...ok, a hublet process (pid %i) of %s was terminated\n" pid name)
      self#get_hublet_processes;
    Log.printf "Ok, the hublets of %s were destroyed...\n" name;
    state := Destroyed;
    Log.printf "The method destroy has been called on the device %s: end\n" name

  method private execute_the_unexpected_death_callback pid process_name =
    let process_name = Filename.basename process_name in
    let title =
      Printf.sprintf
        "Un processus (%s) permettant la simulation de\n%s est mort de façon inattendue"
        process_name
        name in
    let message =
      Printf.sprintf
        (f_ "The process %s with pid %i allowing the simulation of %s %s died unexpectedly. It was necessary %s \"%s\" to maintain a consistent state.")
        process_name
        pid
        self#device_type
        name
        (if self#device_type = "Ethernet cable" then (s_ "to restart") else (s_ "to stop"))
        name
    in
    (* Run the actual callback, and warn the user: *)
    unexpected_death_callback ();
    Simple_dialogs.warning title message ();

  (** A hublet process represents an ethernet port, so this serves as an accessor for
      a connection endpoint *)
  method get_hublet_process n =
    List.nth (self#get_hublet_processes) n

  (** Just an alias for get_hublet_process *)
  method get_ethernet_port n = self#get_hublet_process n

  (** Return a hublet process given a port string. To do: this is very ugly and based on
      the weak assumption that ports are always named eth* and port*, and that * is an integer
      starting at 0 for the first port and respecting the ports order *)
  method get_ethernet_port_by_name name =
    let n =
      try
        Scanf.sscanf name "eth%i" (fun q -> q)
      with _ ->
        Scanf.sscanf name "port%i" (fun q -> q)
    in
    self#get_hublet_process n

  (** Return the current number of hublets *)
  method get_hublets_no =
    List.length (self#get_hublet_processes)

  (** Return all the device hublet processes *)
  method get_hublet_processes =
    ! current_hublet_processes

  (** Update the hublets number. This can only be done in the 'off' state *)
  method set_hublet_processes_no new_hublets_no =
    match !state with
      Off ->
        let old_hublets_no = self#get_hublets_no in
        Log.print_string ("Simulated_network.device: changing the hublets no from " ^
                      (string_of_int old_hublets_no) ^ " to " ^
                      (string_of_int new_hublets_no) ^ "\n");
        if new_hublets_no = old_hublets_no then (* the hublets no isn't changing *)
          () (* do nothing *)
        else if new_hublets_no > old_hublets_no then begin (* hublets are being created *)
          (* Append the new hublets: *)
          current_hublet_processes :=
            (!current_hublet_processes) @
            (self#make_hublet_processes
               ~unexpected_death_callback:self#execute_the_unexpected_death_callback
               (new_hublets_no - old_hublets_no))
        end
        else begin (* hublets are being destroyed *)
          (* Remove the last (old_hublets_no - new_hublets_no) hublets: *)
          let hublet_processes_to_remove =
            ListExtra.tail ~i:new_hublets_no !current_hublet_processes in
          ignore (List.map
                    (fun hp -> try hp#terminate with _ -> ())
                    hublet_processes_to_remove);
          current_hublet_processes :=
            ListExtra.head ~n:new_hublets_no !current_hublet_processes
        end;
        Log.print_string ("Simulated_network.device: changed the hublets no from " ^
                      (string_of_int old_hublets_no) ^ " to " ^
                      (string_of_int new_hublets_no) ^ ": success\n");
    | _ ->
        failwith "can't update the hublets number for a non-off device"

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

(** {2 hub and switch implementation}
    Note that these classes implement hubs and switches {e only as specified
    in the user network}: this implies that Hublets are not implemented
    this way. *)

(** Implementation of a user-network cable or of a link to a switch/hub to a hublet.
    It's meant to always link two switch/hub processes, and as an interesting particular
    case two hublets.
    The 'straight' and 'cross-over' cases are not distinguished. *)
class ethernet_cable =
  fun ~name
      ~(left_end)
      ~(right_end)
      ?blinker_thread_socket_file_name:(blinker_thread_socket_file_name=None)
      ?left_blink_command:(left_blink_command=None)
      ?right_blink_command:(right_blink_command=None)
      ~(unexpected_death_callback : unit -> unit)
      () ->
object(self)
  inherit device
      ~name
      ~hublets_no:0
      ~unexpected_death_callback
      ()
      as super
  val ethernet_cable_process = ref None
  method private get_ethernet_cable_process =
    match !ethernet_cable_process with
      Some ethernet_cable_process -> ethernet_cable_process
    | None -> failwith "ethernet_cable: get_ethernet_cable_process was called when there is no such process"
  initializer
    let defects = get_defects_interface () in
    ethernet_cable_process :=
      Some(new ethernet_cable_process
             ~left_end
             ~right_end
             ~blinker_thread_socket_file_name
             ~left_blink_command
             ~right_blink_command
             ~rightward_loss:(defects#get_cable_attribute name LeftToRight "Loss %")
             ~rightward_duplication:(defects#get_cable_attribute name LeftToRight "Duplication %")
             ~rightward_flip:(defects#get_cable_attribute name LeftToRight "Flipped bits %")
             ~rightward_min_delay:(defects#get_cable_attribute name LeftToRight "Minimum delay (ms)")
             ~rightward_max_delay:(defects#get_cable_attribute name LeftToRight "Maximum delay (ms)")
             ~leftward_loss:(defects#get_cable_attribute name RightToLeft "Loss %")
             ~leftward_duplication:(defects#get_cable_attribute name RightToLeft "Duplication %")
             ~leftward_flip:(defects#get_cable_attribute name RightToLeft "Flipped bits %")
             ~leftward_min_delay:(defects#get_cable_attribute name RightToLeft "Minimum delay (ms)")
             ~leftward_max_delay:(defects#get_cable_attribute name RightToLeft "Maximum delay (ms)")
             ~unexpected_death_callback:self#execute_the_unexpected_death_callback
             ())
  method device_type = "Ethernet cable"

  method spawn_processes = self#get_ethernet_cable_process#spawn
  method terminate_processes =
    (try self#get_ethernet_cable_process#terminate with _ -> ())
  method stop_processes = self#get_ethernet_cable_process#stop
  method continue_processes = self#get_ethernet_cable_process#continue
end;;

(** This class implements {e either} a hub or a switch; their implementation
    is nearly identical, so this is convenient. *)
class virtual hub_or_switch =
  fun ~name
      ~hublets_no
      ~(hub:bool)
      ~unexpected_death_callback
      () ->
object(self)
  inherit device
      ~name
      ~hublets_no
      ~unexpected_death_callback
      ()
      as super

  val main_switch_process = ref None
  method private get_main_switch_process =
    match !main_switch_process with
      Some main_switch_process -> main_switch_process
    | None -> failwith "hub_or_switch: get_main_switch_process was called when there is no such process"

  initializer
    main_switch_process :=
      Some (new hub_or_switch_process
              ~hub
              ~ports_no:hublets_no
              ~unexpected_death_callback:self#execute_the_unexpected_death_callback
              ())

  val internal_cable_processes = ref []

  (** Switches and hubs are stateless from the point of view of the user, and it makes no
      sense to "suspend" them. This also helps implementation :-) *)
  method spawn_processes =
    (* Spawn the main switch process, and wait to be sure it's started: *)
    self#get_main_switch_process#spawn;
(*     Thread.delay 2.0; *)
    (* Create internal cable processes from main switch to hublets, and spawn them: *)
    (internal_cable_processes :=
      let hublets = self#get_hublet_processes in
      let defects = get_defects_interface () in
      List.map
        (fun (i, hublet_process) ->
          new ethernet_cable_process
            ~left_end:self#get_main_switch_process
            ~right_end:hublet_process
            ~rightward_loss:(defects#get_port_attribute_by_index name i InToOut "Loss %")
            ~rightward_duplication:(defects#get_port_attribute_by_index name i InToOut "Duplication %")
            ~rightward_flip:(defects#get_port_attribute_by_index name i InToOut "Flipped bits %")
            ~rightward_min_delay:(defects#get_port_attribute_by_index name i InToOut "Minimum delay (ms)")
            ~rightward_max_delay:(defects#get_port_attribute_by_index name i InToOut "Maximum delay (ms)")
            ~leftward_loss:(defects#get_port_attribute_by_index name i OutToIn "Loss %")
            ~leftward_duplication:(defects#get_port_attribute_by_index name i OutToIn "Duplication %")
            ~leftward_flip:(defects#get_port_attribute_by_index name i OutToIn "Flipped bits %")
            ~leftward_min_delay:(defects#get_port_attribute_by_index name i OutToIn "Minimum delay (ms)")
            ~leftward_max_delay:(defects#get_port_attribute_by_index name i OutToIn "Maximum delay (ms)")
            ~unexpected_death_callback:self#execute_the_unexpected_death_callback
            ())
        (zip
           (ListExtra.range 0 ((List.length hublets) - 1))
           hublets));
    Task_runner.do_in_parallel
      (List.map (* Here map returns a list of thunks *)
         (fun internal_cable_process () -> internal_cable_process#spawn)
         !internal_cable_processes)

  method terminate_processes =
    (* Terminate internal cables and the main switch process: *)
    Task_runner.do_in_parallel
      ((fun () -> self#get_main_switch_process#terminate)
       ::
       (List.map (* here map returns a list of thunks *)
          (fun internal_cable_process () -> internal_cable_process#terminate)
          !internal_cable_processes));
    (* Unreference cable processes: *)
    internal_cable_processes := [];

  (** Stop/continue aren't distinguishable from terminate/spawn from the user's point
      of view: *)
  method stop_processes = self#terminate_processes

  (** Stop/continue aren't distinguishable from terminate/spawn from the user's point
      of view: *)
  method continue_processes = self#spawn_processes
end;;

(** A hub: just a [hub_or_switch] with [hub = true] *)
class hub =
  fun ~name
      ~hublets_no
      ~unexpected_death_callback
      () ->
object(self)
  inherit hub_or_switch
      ~name
      ~hublets_no
      ~hub:true
      ~unexpected_death_callback
      ()
      as super
  method device_type = "hub"
end;;

(** A switch: just a [hub_or_switch] with [hub = false] *)
class switch =
  fun ~name
      ~hublets_no
      ~unexpected_death_callback
      () ->
object(self)
  inherit hub_or_switch
      ~name
      ~hublets_no
      ~hub:false
      ~unexpected_death_callback
      ()
      as super
  method device_type = "switch"
end;;

(** {2 machine and router implementation} *)


(** To do: move this into ListExtra.
    Return a sublist of the given list containing all the elements with indices
    between first_index and last_index, both included: *)
let rec from_to first_index last_index list =
  match first_index, list with
    _, [] ->
      failwith "from_to: out of bounds"
  | 0, (x::xs) ->
      if last_index < 0 then
        failwith "from_to: out of bounds"
      else if last_index = 0 then
        [x]
      else
        x :: (from_to 0 (last_index - 1) xs)
  | n, (_::xs) ->
      from_to (first_index - 1) (last_index - 1) xs;;

(** This class implements {e either} an machine or a router; their implementation
    is nearly identical, so this is convenient. *)
class virtual machine_or_router =
  fun ~name
      ~router:bool
      ~(kernel_file_name)
      ~(filesystem_file_name)
      ~(cow_file_name)
      ~(ethernet_interfaces_no)
      ~(memory) (* in megabytes *)
      ~(console)
      ~xnest
      ?umid:(umid="uml-" ^ (string_of_int (gensym ())))
      ~id
      ~unexpected_death_callback
      () ->
let half_hublets_no = ethernet_interfaces_no in
object(self)
  (* Outer hublets interface the device with the outer world, but we want to just user
     super#destroy to get rid of *all* hublets; so we declare *all* hublets as part of the
     external interface, even if only the first half will be used in this way. Hence it's
     important the the outer layer comes *first*: *)
  inherit device
      ~name
      ~hublets_no:(half_hublets_no * 2)
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
    let all_hublets = self#get_hublet_processes in
    outer_hublet_processes :=
      from_to 0 (half_hublets_no - 1) all_hublets;
    inner_hublet_processes :=
      from_to half_hublets_no (2 * half_hublets_no - 1) all_hublets;
    (if xnest then
      xnest_process :=
        Some (new xnest_process
                ~title:(Printf.sprintf (f_ "Virtual X display of \"%s\"") umid)
                ~unexpected_death_callback:self#execute_the_unexpected_death_callback
                ()));
    uml_process :=
      Some (new uml_process
              ~kernel_file_name
              ~filesystem_file_name
              ~cow_file_name
              ~ethernet_interfaces_no
              ~hublet_processes:self#get_inner_hublet_processes
              ~memory
              ~umid
              ~console
              ~id
              ?xnest_display_number:(if xnest then
                                       Some self#get_xnest_process#display_number_as_server
                                     else
                                       None)
              ~unexpected_death_callback:self#execute_the_unexpected_death_callback
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
(*     Log.printf "OK-Q 1\n"; flush_all (); *)
    (internal_cable_processes :=
      let defects = get_defects_interface () in
      List.map
        (fun (i, inner_hublet_process, outer_hublet_process) ->
          new ethernet_cable_process
            ~left_end:inner_hublet_process
            ~right_end:outer_hublet_process
            ~rightward_loss:(defects#get_port_attribute_by_index name i InToOut "Loss %")
            ~rightward_duplication:(defects#get_port_attribute_by_index name i InToOut "Duplication %")
            ~rightward_flip:(defects#get_port_attribute_by_index name i InToOut "Flipped bits %")
            ~rightward_min_delay:(defects#get_port_attribute_by_index name i InToOut "Minimum delay (ms)")
            ~rightward_max_delay:(defects#get_port_attribute_by_index name i InToOut "Maximum delay (ms)")
            ~leftward_loss:(defects#get_port_attribute_by_index name i OutToIn "Loss %")
            ~leftward_duplication:(defects#get_port_attribute_by_index name i OutToIn "Duplication %")
            ~leftward_flip:(defects#get_port_attribute_by_index name i OutToIn "Flipped bits %")
            ~leftward_min_delay:(defects#get_port_attribute_by_index name i OutToIn "Minimum delay (ms)")
            ~leftward_max_delay:(defects#get_port_attribute_by_index name i OutToIn "Maximum delay (ms)")
            ~unexpected_death_callback:self#execute_the_unexpected_death_callback
            ())
        (zip3
           (ListExtra.range 0 (half_hublets_no - 1))
           self#get_inner_hublet_processes
           self#get_outer_hublet_processes));
(*     Log.printf "OK-Q 3\n"; flush_all (); *)
    (* Spawn internal cables processes and the UML process: *)
    Task_runner.do_in_parallel
      ((fun () -> self#get_uml_process#spawn)
       ::
       (List.map (* here map returns a list of thunks *)
          (fun internal_cable_process () -> internal_cable_process#spawn)
          !internal_cable_processes));
(*     Log.printf "OK-Q 4\n"; flush_all (); *)

  method private terminate_processes_private ~gracefully  () =
    Log.printf "** Terminating the internal cable processes of %s...\n" name; flush_all ();
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

(** A machine: just a [machine_or_router] with [router = false] *)
class machine =
  fun ~name
      ~(filesystem_file_name)
      ~(kernel_file_name)
      ~(cow_file_name)
      ~(ethernet_interfaces_no)
      ?memory:(memory=40) (* in megabytes *)
      ?umid:(umid="uml-" ^ (string_of_int (gensym ())))
      ?(xnest=false)
      ~id
      ~unexpected_death_callback
      () ->
object(self)
  inherit machine_or_router
      ~name
      ~router:false
      ~filesystem_file_name
      ~cow_file_name
      ~kernel_file_name
      ~ethernet_interfaces_no
      ~memory
      ~umid
      ~console:"xterm"
      ~id
      ~xnest
      ~unexpected_death_callback
      ()
      as super
  method device_type = "computer"
end;;

(** A router: just a [machine_or_router] with [router = true] *)
class router =
  fun ~name
      ~(cow_file_name)
      ~(kernel_file_name)
      ~(filesystem_file_name)
      ~(ethernet_interfaces_no)
      ?umid:(umid="uml-" ^ (string_of_int (gensym ())))
      ~id
      ~unexpected_death_callback
      () ->
object(self)
  inherit machine_or_router
      ~name
      ~router:true
      ~filesystem_file_name(* :"/usr/marionnet/filesystems/router.debian.lenny.sid.fs" *)
      ~kernel_file_name
      ~cow_file_name
      ~ethernet_interfaces_no
      ~memory:40(*32*)
      ~umid
      (* Change this when debugging the router device *)
      ~console:"none" (* To do: this should be "none" for releases and "xterm" for debugging *)
      ~id
      ~xnest:false
      ~unexpected_death_callback
      ()
      as super
  method device_type = "router"
end;;

class bridge_socket =
  fun (* ~id *)
      ~name
      ~bridge_name
      ~unexpected_death_callback
      () ->
object(self)
  inherit device
      ~name
      ~hublets_no:1
      ~unexpected_death_callback
      ()
      as super
  method device_type = "bridge_socket"

  val the_hublet_process = ref None
  method private get_the_hublet_process = (* the method get_hublet_process exists and is different... *)
    match !the_hublet_process with
      Some the_hublet_process -> the_hublet_process
    | None -> failwith "bridge_socket: get_the_hublet_process was called when there is no such process"

  val bridge_socket_hub_process = ref None
  method private get_bridge_socket_hub_process =
    match !bridge_socket_hub_process with
    | Some p -> p
    | None -> failwith "bridge_socket: get_bridge_socket_hub_process was called when there is no such process"

  val bridge_socket_tap_name = ref None
  method private get_bridge_socket_tap_name =
    match !bridge_socket_tap_name with
    | Some t -> t
    | None -> failwith "bridge_socket_tap_name: non existing tap"

  (** Create the tap via the daemon, and return its name.
      Fail if a the tap already exists: *)
  method private make_bridge_socket_tap =
    match !bridge_socket_tap_name with
      None ->
        let tap_name =
          let server_response =
            Daemon_client.ask_the_server
              (Make (AnySocketTap((Unix.getuid ()), bridge_name))) in
          (match server_response with
          | Created (SocketTap(tap_name, _, _)) -> tap_name
          | _ -> "non-existing-tap") in
        bridge_socket_tap_name := Some tap_name;
        tap_name
    | Some _ ->
        failwith "a bridge socket tap already exists"

  method private destroy_bridge_socket_tap =
    (try
      ignore (Daemon_client.ask_the_server
                (Destroy (SocketTap(self#get_bridge_socket_tap_name,
                                     (Unix.getuid ()),
                                     bridge_name))));
    with e -> begin
      Log.printf
        "WARNING: Failed in destroying a host tap for a bridge socket : %s\n"
        (Printexc.to_string e);
    end);
    bridge_socket_tap_name := None

  val internal_cable_process = ref None

  initializer
    assert ((List.length self#get_hublet_processes) = 1);
    the_hublet_process :=
      Some (self#get_hublet_process 0);
    bridge_socket_hub_process :=
      Some self#make_bridge_socket_hub_process

  method private make_bridge_socket_hub_process =
    new bridge_socket_hub_process
      ~tap_name:self#make_bridge_socket_tap
      ~unexpected_death_callback:self#execute_the_unexpected_death_callback
      ()

  method spawn_processes =
    (match !bridge_socket_hub_process with
    | None ->
        bridge_socket_hub_process := Some self#make_bridge_socket_hub_process
    | Some the_bridge_socket_hub_process ->
        ());
    (* Spawn the hub process, and wait to be sure it's started: *)
    self#get_bridge_socket_hub_process#spawn;
    (* Create the internal cable process from the single hublet to the hub,
       and spawn it: *)
    let the_internal_cable_process =
      let defects = get_defects_interface () in
      new ethernet_cable_process
        ~rightward_loss:(defects#get_port_attribute_by_index name 0 InToOut "Loss %")
        ~rightward_duplication:(defects#get_port_attribute_by_index name 0 InToOut "Duplication %")
        ~rightward_flip:(defects#get_port_attribute_by_index name 0 InToOut "Flipped bits %")
        ~rightward_min_delay:(defects#get_port_attribute_by_index name 0 InToOut "Minimum delay (ms)")
        ~rightward_max_delay:(defects#get_port_attribute_by_index name 0 InToOut "Maximum delay (ms)")
        ~leftward_loss:(defects#get_port_attribute_by_index name 0 OutToIn "Loss %")
        ~leftward_duplication:(defects#get_port_attribute_by_index name 0 OutToIn "Duplication %")
        ~leftward_flip:(defects#get_port_attribute_by_index name 0 OutToIn "Flipped bits %")
        ~leftward_min_delay:(defects#get_port_attribute_by_index name 0 OutToIn "Minimum delay (ms)")
        ~leftward_max_delay:(defects#get_port_attribute_by_index name 0 OutToIn "Maximum delay (ms)")
        ~left_end:self#get_bridge_socket_hub_process
        ~right_end:self#get_the_hublet_process
        ~unexpected_death_callback:self#execute_the_unexpected_death_callback
        () in
    internal_cable_process := Some the_internal_cable_process;
    the_internal_cable_process#spawn

  method terminate_processes =
    (* Terminate the internal cable process and the hub process: *)
    (match !internal_cable_process with
      Some the_internal_cable_process ->
        Task_runner.do_in_parallel
          [ (fun () -> the_internal_cable_process#terminate);
            (fun () -> self#get_bridge_socket_hub_process#terminate) ]
    | None ->
        assert false);
    (* Destroy the tap, via the daemon: *)
    self#destroy_bridge_socket_tap;
    (* Unreference everything: *)
    internal_cable_process := None;
    bridge_socket_hub_process := None;

  (** As bridge sockets are stateless from the point of view of the user, stop/continue
      aren't distinguishable from terminate/spawn: *)
  method stop_processes = self#terminate_processes
  method continue_processes = self#spawn_processes
end;;

class cloud =
  fun (* ~id *)
      ~name
      ~unexpected_death_callback
      () ->
object(self)
  inherit device
      ~name
      ~hublets_no:2
      ~unexpected_death_callback
      ()
      as super
  method device_type = "cloud"

  val internal_cable_process = ref None
  method private get_internal_cable_process =
    match !internal_cable_process with
      Some internal_cable_process -> internal_cable_process
    | None -> failwith "cloud: get_the_internal_cable_process was called when there is no such process"

  initializer
    ()

  method spawn_processes =
    (* Create the internal cable process and spawn it: *)
    let defects = get_defects_interface () in
    let the_internal_cable_process =
      new ethernet_cable_process
        ~rightward_loss:(defects#get_port_attribute_by_index name 0 InToOut "Loss %")
        ~rightward_duplication:(defects#get_port_attribute_by_index name 0 InToOut "Duplication %")
        ~rightward_flip:(defects#get_port_attribute_by_index name 0 InToOut "Flipped bits %")
        ~rightward_min_delay:(defects#get_port_attribute_by_index name 0 InToOut "Minimum delay (ms)")
        ~rightward_max_delay:(defects#get_port_attribute_by_index name 0 InToOut "Maximum delay (ms)")
        ~leftward_loss:(defects#get_port_attribute_by_index name 0 OutToIn "Loss %")
        ~leftward_duplication:(defects#get_port_attribute_by_index name 0 OutToIn "Duplication %")
        ~leftward_flip:(defects#get_port_attribute_by_index name 0 OutToIn "Flipped bits %")
        ~leftward_min_delay:(defects#get_port_attribute_by_index name 0 OutToIn "Minimum delay (ms)")
        ~leftward_max_delay:(defects#get_port_attribute_by_index name 0 OutToIn "Maximum delay (ms)")
        ~left_end:(self#get_hublet_process 0)
        ~right_end:(self#get_hublet_process 1)
        ~unexpected_death_callback:self#execute_the_unexpected_death_callback
        () in
    internal_cable_process := Some the_internal_cable_process;
    the_internal_cable_process#spawn

  method terminate_processes =
    (* Terminate the internal cable process: *)
    (try self#get_internal_cable_process#terminate with _ -> ());
    (* Unreference it: *)
    internal_cable_process := None;

  (** As clouds are stateless from the point of view of the user, stop/continue
      aren't distinguishable from terminate/spawn: *)
  method stop_processes = self#terminate_processes
  method continue_processes = self#spawn_processes
end;;
