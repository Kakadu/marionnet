(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007, 2008, 2009  Luca Saiu
   Copyright (C) 2007, 2009, 2010, 2013  Jean-Vincent Loddo
   Copyright (C) 2007, 2008, 2009, 2010, 2013  Université Paris 13

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


(** Some modules for managing the virtual network *)

open Gettext;;
module Recursive_mutex = MutexExtra.Recursive ;;

(** A thunk allowing to invoke the sketch refresh method, accessible from many
    modules: *)
module Refresh_sketch_thunk = Stateful_modules.Variable (struct
  type t = unit->unit
  let name = Some "Refresh_sketch_thunk"
  end)
let refresh_sketch () = Refresh_sketch_thunk.extract () ()


(* *************************** *
        Module Dot_tuning
 * *************************** *)

module Dot_tuning = struct


(* *************************
    class Dot_tuning.network
   ************************* *)

type index = int;; (* 0..(length-1) *)
type shuffler = index list ;; (* represents a permutation of indexes of a list*)

(* This part of the state will be filled loading Gui_toolbar_DOT_TUNING. *)
class type dot_tuning_high_level_toolbar_driver =
 object
  method get_iconsize              : string
  method set_iconsize              : string -> unit
  method get_nodesep               : float
  method set_nodesep               : float -> unit
  method get_labeldistance         : float
  method set_labeldistance         : float -> unit
  method get_extrasize             : float
  method set_extrasize             : float -> unit
  method get_image                 : GdkPixbuf.pixbuf
  method get_image_current_width   : int
  method get_image_current_height  : int
  method reset_image_size          : unit -> unit
  method get_image_original_width  : int
  method get_image_original_height : int
end (* class type high_level_toolbar_driver *)

(** Dot options for a network *)
let network_marshaller = new Oomarshal.marshaller;;

(* TODO: rename is in network_dot_tuning_zone *)
class network =

  fun ?(iconsize="large") ?(shuffler=[]) ?(rankdir="TB") ?(nodesep=0.5) ?(labeldistance=1.6) ?(extrasize=0.) ?(curved_lines=false)

      (* The handler for the real network *)
      (network:( < reversed_cables:(string list); reversed_cable_set:(bool->string->unit); .. > ))  ->

  object (self)
  inherit Xforest.interpreter ()

  method direct_cable_color    = "#949494"
  method crossover_cable_color = "#6d8dc0"

  val iconsize = Chip.wref ~name:"iconsize" iconsize
  method iconsize = iconsize

  val rankdir  = Chip.wref ~name:"rankdir" rankdir
  method rankdir = rankdir

  val curved_lines = Chip.wswitch ~name:"curved_lines" curved_lines
  method curved_lines = curved_lines

  val shuffler = Chip.wref ~name:"shuffler" shuffler
  method shuffler = shuffler

  val nodesep = Chip.wref ~name:"nodesep" nodesep
  method nodesep = nodesep

  val labeldistance = Chip.wref ~name:"labeldistance" labeldistance
  method labeldistance = labeldistance

  val extrasize = Chip.wref ~name:"extrasize" extrasize
  method extrasize = extrasize

  method iconsize_for_dot  = iconsize#get
  method shuffler_as_function = ListExtra.asFunction shuffler#get (* returns the permutation function *)
  method rankdir_for_dot   = "rankdir="^(rankdir#get)^";"
  method nodesep_for_dot   = let s=(string_of_float nodesep#get) in ("nodesep="^s^"; ranksep="^s)
  method labeldistance_for_dot = "labeldistance="^(string_of_float labeldistance#get)

  (** This is the method used in user gui callbacks (reactions) *)
  val mutable gui_callbacks_disable : bool   = false
  method gui_callbacks_disable   = gui_callbacks_disable
  method set_gui_callbacks_disable x = gui_callbacks_disable <- x
  method disable_gui_callbacks    () = gui_callbacks_disable <- true
  method enable_gui_callbacks     () =
   ignore (GMain.Timeout.add ~ms:500 ~callback:(fun () -> gui_callbacks_disable <- false; false))

  method reset_shuffler () = shuffler#set []

  method reset_extrasize () =
    begin
    self#toolbar_driver#reset_image_size ();
    extrasize#set 0.;
    end

  (* Delete _alone here:  *)
  method reset_defaults () =
    begin
      iconsize#set "large";
      shuffler#set [];
      rankdir#set "TB";
      curved_lines#reset ();
      nodesep#set 0.5;
      labeldistance#set 1.6 ;
      ListExtra.foreach network#reversed_cables (network#reversed_cable_set false) ;
      self#reset_extrasize () ;
      self#set_toolbar_widgets ()
    end

  method ratio : string =
   let extrasize = extrasize#get in
   if (extrasize = 0.) then "ratio=compress;" else
   begin
    let x = Widget.Image.inch_of_pixels self#toolbar_driver#get_image_original_width in
    let y = Widget.Image.inch_of_pixels self#toolbar_driver#get_image_original_height in
    let area  = x *. y in
    let delta_area = extrasize *. area /. 100. in
    let delta = sqrt( (x+.y)**2. +. 4.*. delta_area  )  -.  (x+.y)  in
    let x = string_of_float (x +. delta) in
    let y = string_of_float (y +. delta) in
    "size=\""^x^","^y^
    "\";\nratio=fill;"
   end

  (** Accessor the dot tuning toolbar. This part of the state will be filled
      loading Gui_toolbar_DOT_TUNING.
      Inverted cables corresponds to dynamic menus, so they not need to be reactualized
      (the dynamic menus are recalculated each time from network#reversed_cables. *)

  val mutable toolbar_driver : dot_tuning_high_level_toolbar_driver option = None
  method set_toolbar_driver t = toolbar_driver <- Some t
  method toolbar_driver = match toolbar_driver with Some t -> t | None -> assert false

  (** The dotoption gui reactualization *)

  method set_toolbar_widgets () : unit =
    begin
      self#disable_gui_callbacks   () ;
      self#toolbar_driver#set_iconsize iconsize#get ;
      self#toolbar_driver#set_nodesep nodesep#get ;
      self#toolbar_driver#set_labeldistance labeldistance#get ;
      self#toolbar_driver#set_extrasize extrasize#get ;
      self#enable_gui_callbacks    () ;
      ()
    end

  (** Marshalling is performed in this ugly way because directly dumping the whole [self] object
      would involve resolving references to Gtk callbacks, which are outside the OCaml heap and
      hence (understandably) not supported by the marshaller. *)

  (** Dump the current state of [self] into the given file. *)
  method save_to_file (file_name : string) =
    (* we are manually setting the verbosity 3 *)
    (if (Global_options.Debug_level.get ()) >= 3 then Xforest.print_xforest ~channel:stderr network#to_forest);
    network_marshaller#to_file self#to_forest file_name

  (** This method is used just for undumping dotoptions, so is not strict.
      For instance, exceptions provoked by bad cable names are simply ignored. *)
  method set_reversed_cables names =
    ListExtra.foreach names (fun n -> try (network#reversed_cable_set true n) with _ -> ())

  (** Undump the state of [self] from the given file. *)
  method load_from_file ~(project_version: [`v0|`v1|`v2]) (fname : string) =
    let (forest:Xforest.t) = 
      match project_version with
      | `v2 | `v1 -> network_marshaller#from_file (fname)
      | `v0       -> Forest_backward_compatibility.load_from_old_file (fname)
    in
   (* we are manually setting the verbosity 3 *)
   (if (Global_options.Debug_level.get ()) >= 3 then Xforest.print_xforest ~channel:stderr forest);
   match Forest.to_tree forest with
   | (("dotoptions", attrs), children) -> self#from_tree ("dotoptions", attrs) children
   | _ -> assert false
   
 (** Dot_tuning to forest encoding. *)
  method to_tree : (string * (string * string) list) Forest.tree =
   Forest.tree_of_leaf ("dotoptions", [
     ("iconsize"      , iconsize#get                   ) ;
     ("shuffler"      , (Xforest.encode shuffler#get)  ) ;
     ("rankdir"       , rankdir#get                    ) ;
     ("curved_lines"  , (string_of_bool curved_lines#get)) ;
     ("nodesep"       , (string_of_float nodesep#get)      ) ;
     ("labeldistance" , (string_of_float labeldistance#get)) ;
     ("extrasize"     , (string_of_float extrasize#get)    ) ;
     ("gui_callbacks_disable", (string_of_bool gui_callbacks_disable)) ;
     ("invertedCables", (Xforest.encode network#reversed_cables)) ;
     ])

 (** A Dotoption.network has just attributes (no children) in this version.
     The Dotoption.network must be undumped AFTER the Netmodel.network in
     order to have significant cable names (reversed_cables). *)
 method eval_forest_attribute = function
  | ("iconsize"             , x ) -> self#iconsize#set x
  | ("shuffler"             , x ) -> self#shuffler#set (Xforest.decode x)
  | ("rankdir"              , x ) -> self#rankdir#set x
  | ("curved_lines"         , x ) -> self#curved_lines#set_to (bool_of_string x)
  | ("nodesep"              , x ) -> self#nodesep#set (float_of_string x)
  | ("labeldistance"        , x ) -> self#labeldistance#set (float_of_string x)
  | ("extrasize"            , x ) -> self#extrasize#set (float_of_string x)
  | ("gui_callbacks_disable", x ) -> self#set_gui_callbacks_disable (bool_of_string x)
  | ("invertedCables"       , x ) -> self#set_reversed_cables (Xforest.decode x)
  | _ -> () (* Forward-comp. *)

end;; (* class Dot_tuning.network *)

end;; (* module Dot_tuning *)

type devkind = [ `Machine | `Hub | `Switch | `Router | `World_gateway | `World_bridge | `Cloud ] ;;

type nodename   = string ;;

(** Examples: "eth0", "port3" *)
type receptname = string ;;

type name   = string ;;
type label  = string ;;
let nolabel = "";;

(** iconsize may be "small", "med", "large" or "xxl". *)
type iconsize = string ;;

(** {2 Classes} *)

type simulated_device_automaton_state =
   NoDevice         (** *)
 | DeviceOff        (** *)
 | DeviceOn         (** *)
 | DeviceSleeping   (** *)
;;

let string_of_simulated_device_automaton_state = function
  | DeviceOff      -> "DeviceOff"
  | DeviceOn       -> "DeviceOn"
  | DeviceSleeping -> "DeviceSleeping"
  | NoDevice       -> "NoDevice"
;;

exception ForbiddenTransition;;
let raise_forbidden_transition msg =
 Log.printf1 "ForbiddenTransition raised in %s\n" msg;
 raise ForbiddenTransition
;;

(** This represents the current state of a simulated device (as per
    network_simulation.ml) and enables easy high-level state transitions which
    conveniently hide the complexity of managing switches and cables; when the
    user tries to invoke any forbidden state transition an exception is
    raised. *)
class virtual ['parent] simulated_device () = object(self)

  initializer
    self#add_destroy_callback (lazy self#destroy_my_simulated_device);

  (** We have critical sections here: *)
  val mutex = Recursive_mutex.create ()

  (** The current automaton state, and its access method: *)
  val automaton_state = ref NoDevice

  (** Get the state of simulated device. *)
  method simulated_device_state =
    !automaton_state

  (** This string will be used to select the good icon for the dot sketch. *)
  method string_of_simulated_device_state = match !automaton_state with
  | DeviceOff      -> "off"
  | DeviceOn       -> "on"
  | DeviceSleeping -> "pause"
  | _              -> "off" (* Sometimes the sketch is builded in this state, so... *)

  (** For debugging. Failthful translation of constructors: *)
  method automaton_state_as_string = string_of_simulated_device_automaton_state !automaton_state

  (** The automaton state this device is going to. This is only used for the GUI and
      is not guaranteed to be accurate in case of concurrent access. It's only
      guaranteed to always hold some value of the correct type.
      If no transition is occurring then the ref should hold None. *)
  val next_automaton_state = ref (Some NoDevice)

  method next_simulated_device_state =
    !next_automaton_state

  method set_next_simulated_device_state state =
    next_automaton_state := state;
    refresh_sketch (); (* show our transient simulation state icon *)

  method virtual get_name : string

  (** The device implementing the object in the simulated network, if any (this is
      ref None when the device has not been started yet, or some state modification
      happened) *)
  val simulated_device : 'parent Simulation_level.device option ref =
    ref None

  method get_hublet_process_of_port index =
    match !simulated_device with
    | Some (sd) -> sd#get_hublet_process_of_port index
    | None      -> failwith "looking for a hublet when its device is non-existing"

  (** Create a new simulated device according to the current status *)
  method virtual make_simulated_device : 'parent Simulation_level.device

  (** Return the list of cables directly linked to a port of self as an endpoint.
      This is needed so that simulated cables can be automatically started/destroyed
      as soon as both their endpoints are created/destroyed *)
  method private get_involved_cables = []

  (** Return true iff hublet processes are currently existing. This is only meaningful
      for devices which can actually have hublets *)
  method has_hublet_processes =
    match !simulated_device with
      Some(_) -> true
    | None -> false

  method private enqueue_task_with_progress_bar verb thunk =
    let text = verb ^ " " ^ self#get_name in
    let progress_bar = ref None in
    Task_runner.the_task_runner#schedule
      ~name:text
      (fun () ->
        (try
          progress_bar := Some (Simple_dialogs.make_progress_bar_dialog ~title:text ());
          thunk ();
        with e -> begin
          Log.printf "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n";
          let message =
            Printf.sprintf "enqueue_task_with_progress_bar: %s %s failed (%s)"
              verb self#get_name (Printexc.to_string e) in
          Log.printf1 "%s\n" message;
          Simple_dialogs.warning message message ();
          Log.printf "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n";
        end));
    Task_runner.the_task_runner#schedule
      ~name:("Destroy the progress bar for \"" ^ text ^ "\"")
      (fun () ->
        match !progress_bar with
          Some progress_bar ->
            Simple_dialogs.destroy_progress_bar_dialog progress_bar
        | None ->
            assert false)

  method create =
    (* This is invisible for the user: don't set the next state *)
    Task_runner.the_task_runner#schedule ~name:("create "^self#get_name) (fun () -> self#create_right_now)

  method (*private*) destroy_my_simulated_device =
    Log.printf1 "component \"%s\": destroying my simulated device.\n" self#get_name;
    (* This is invisible for the user: don't set the next state *)
    Task_runner.the_task_runner#schedule ~name:("destroy "^self#get_name)(fun () -> self#destroy_right_now)

  method startup =
    self#set_next_simulated_device_state (Some DeviceOn);
    self#enqueue_task_with_progress_bar (s_ "Starting") (fun () -> if self#can_startup then    self#startup_right_now)

  method suspend =
    self#set_next_simulated_device_state (Some DeviceSleeping);
    self#enqueue_task_with_progress_bar (s_ "Suspending") (fun () -> if self#can_suspend then self#suspend_right_now)

  method resume =
    self#set_next_simulated_device_state (Some DeviceOn);
    self#enqueue_task_with_progress_bar (s_ "Resuming") (fun () -> if self#can_resume then self#resume_right_now)

  method gracefully_shutdown =
    self#set_next_simulated_device_state (Some DeviceOff);
    self#enqueue_task_with_progress_bar (s_ "Stopping") (fun () -> if self#can_gracefully_shutdown then self#gracefully_shutdown_right_now)

  method gracefully_restart =
    if not self#can_gracefully_shutdown then () else (* continue *)
    self#gracefully_shutdown;
    self#set_next_simulated_device_state (Some DeviceOn);
    self#enqueue_task_with_progress_bar
      (s_ "Restarting")
      (fun () ->
         Thread.delay 7.; (* Ugly: to prevent a killer signal (all this part must be rewritten with Cortex_lib as soon as possible!!) *)
         if self#can_startup then self#startup_right_now)

  method poweroff =
    self#set_next_simulated_device_state (Some DeviceOff);
    self#enqueue_task_with_progress_bar (s_ "Shutting down") (fun () -> if self#can_poweroff then self#poweroff_right_now)

  method (*private*) create_right_now =
    Recursive_mutex.with_mutex mutex
      (fun () ->
        Log.printf2 "About to create the simulated device %s: it's connected to %d cables.\n"
          self#get_name
          (List.length (self#get_involved_cables));

        match !automaton_state, !simulated_device with
        | NoDevice, None ->
	    ( simulated_device := (Some self#make_simulated_device);
              automaton_state := DeviceOff;
	      self#set_next_simulated_device_state None;
	      (* An endpoint for cables linked to self was just added; we need to start some cables. *)
	      ignore (List.map
			(fun cable ->
			   Log.printf1 "Working on cable %s\n" (cable#show "");
			   cable#increment_alive_endpoint_no)
			(self#get_involved_cables)))

        | _ -> raise_forbidden_transition "create_right_now")

  (** The unit parameter is needed: see how it's used in simulated_network: *)
  method private destroy_because_of_unexpected_death () =
    Log.printf1 "You don't deadlock here %s, do you? -1\n" self#get_name;
    Recursive_mutex.with_mutex mutex
      (fun () ->
        Log.printf1 "You don't deadlock here %s, do you? 0\n" self#get_name;
        (try
          self#destroy_right_now
        with e -> begin
          Log.printf1 "WARNING: destroy_because_of_unexpected_death: failed (%s)\n"
            (Printexc.to_string e);
        end;
          self#set_next_simulated_device_state None)); (* don't show next-state icons for this *)

  method (*private*) destroy_right_now =
    Recursive_mutex.with_mutex mutex
      (fun () ->
        Log.printf1 "About to destroy the simulated device %s \n" self#get_name;
        match !automaton_state, !simulated_device with
        | (DeviceOn | DeviceSleeping), Some(d) ->
             Log.printf1
               "  (destroying the on/sleeping device %s. Powering it off first...)\n"
               self#get_name;
             self#poweroff_right_now; (* non-gracefully *)
             self#destroy_right_now
        | NoDevice, None ->
            Log.printf1
             "  (destroying the already 'no-device' device %s. Doing nothing...)\n"
             self#get_name;
            () (* Do nothing, but don't fail. *)
        | DeviceOff, Some(d) ->
            ((* An endpoint for cables linked to self was just added; we
                may need to start some cables. *)
             Log.printf1
               "  (destroying the off device %s: decrementing its cables rc...)\n"
               self#get_name;
             List.iter
               (fun cable ->
                 Log.printf1 "Unpinning the cable %s " (cable#show "");
                 cable#decrement_alive_endpoint_no;
                 Log.printf1 ("The cable %s was unpinned with success\n") (cable#show "");
                 )
               self#get_involved_cables;
             Log.printf1 "  (destroying the simulated device implementing %s...)\n" self#get_name;
             d#destroy; (* This is the a method from some object in Simulation_level *)
             simulated_device := None;
             automaton_state := NoDevice;
             self#set_next_simulated_device_state None;
             Log.printf1 "We're not deadlocked yet (%s). Great.\n" self#get_name);
        | _ ->
            raise_forbidden_transition "destroy_right_now"
        );
    Log.printf1 "The simulated device %s was destroyed with success\n" self#get_name


  method (*private*) startup_right_now =
    Recursive_mutex.with_mutex mutex
      (fun () ->
        (* Don't startup ``incorrect'' devices. This is currently limited to cables of the
           wrong crossoverness which the user has defined by mistake: *)
        if self#is_correct then begin
          Log.printf1 "Starting up the device %s...\n" self#get_name;
          match !automaton_state, !simulated_device with
          | NoDevice, None ->
             (Log.printf1 "Creating processes for %s first...\n" self#get_name;
              self#create_right_now;
              Log.printf1 "Processes for %s were created...\n" self#get_name;
              self#startup_right_now
              )

          | DeviceOff, Some(d) ->
             (d#startup;  (* This is the a method from some object in Simulation_level *)
              automaton_state := DeviceOn;
              self#set_next_simulated_device_state None;
              Log.printf1 "The device %s was started up\n" self#get_name
              )

          | DeviceOn,  _ ->
              Log.printf1 "startup_right_now: called in state %s: nothing to do.\n" (self#automaton_state_as_string)

          | _ -> raise_forbidden_transition "startup_right_now"
        end else begin
          Log.printf1 "REFUSING TO START UP the ``incorrect'' device %s!!!\n" self#get_name
        end)

  method (*private*) suspend_right_now =
    Recursive_mutex.with_mutex mutex
      (fun () ->
        Log.printf1 "Suspending up the device %s...\n" self#get_name;
        match !automaton_state, !simulated_device with
          DeviceOn, Some(d) ->
           (d#suspend; (* This is the a method from some object in Simulation_level *)
            automaton_state := DeviceSleeping;
            self#set_next_simulated_device_state None)
        | _ -> raise_forbidden_transition "suspend_right_now")

  method (*private*) resume_right_now =
    Recursive_mutex.with_mutex mutex
      (fun () ->
        Log.printf1 "Resuming the device %s...\n" self#get_name;
        match !automaton_state, !simulated_device with
        | DeviceSleeping, Some(d) ->
           (d#resume; (* This is the a method from some object in Simulation_level *)
            automaton_state := DeviceOn;
            self#set_next_simulated_device_state None)

        | _ -> raise_forbidden_transition "resume_right_now")

  method (*private*) gracefully_shutdown_right_now =
    Recursive_mutex.with_mutex mutex
      (fun () ->
        let current_state = self#automaton_state_as_string in
        (Log.printf2 "* Gracefully shutting down the device %s (from state: %s)...\n"
          self#get_name
          current_state);
        match !automaton_state, !simulated_device with
        | DeviceOn, Some(d) ->
           (d#gracefully_shutdown; (* This is the a method from some object in Simulation_level *)
            automaton_state := DeviceOff;
            self#set_next_simulated_device_state None)

        | DeviceSleeping, Some(d) ->
           (self#resume_right_now;
            self#gracefully_shutdown_right_now)

        | NoDevice,  _ | DeviceOff, _ ->
            Log.printf1 "gracefully_shutdown_right_now: called in state %s: nothing to do.\n" (self#automaton_state_as_string)

        | _ -> raise_forbidden_transition "gracefully_shutdown_right_now")

  method (*private*) poweroff_right_now =
    Recursive_mutex.with_mutex mutex
      (fun () ->
        Log.printf1 "Powering off the device %s...\n" self#get_name;
        match !automaton_state, !simulated_device with
        | DeviceOn, Some(d) ->
           (d#shutdown; (* non-gracefully *)
            automaton_state := DeviceOff;
            self#set_next_simulated_device_state None)

        | DeviceSleeping, Some(d) ->
            (self#resume_right_now;
             self#poweroff_right_now)

        | NoDevice,  _ | DeviceOff, _ ->
            Log.printf1 "poweroff_right_now: called in state %s: nothing to do.\n" (self#automaton_state_as_string)

        | _ -> raise_forbidden_transition "poweroff_right_now")

  (** Return true iff the current state allows the user to 'startup' the device from the GUI. *)
  method can_startup =
    Recursive_mutex.with_mutex mutex
      (fun () ->
        match !automaton_state with NoDevice | DeviceOff -> true | _ -> false)

  (** Return true iff the current state allows the user to 'shutdown' a device from the GUI. *)
  method can_gracefully_shutdown =
    Recursive_mutex.with_mutex mutex
      (fun () ->
        match !automaton_state with DeviceOn | DeviceSleeping -> true | _ -> false)

  (** Return true iff the current state allows the user to 'power off' a device from the GUI. *)
  method can_poweroff =
    Recursive_mutex.with_mutex mutex
      (fun () ->
        match !automaton_state with NoDevice | DeviceOff -> false | _ -> true)

  (** Return true iff the current state allows the user to 'suspend' a device from the GUI. *)
  method can_suspend =
    Recursive_mutex.with_mutex mutex
      (fun () ->
        match !automaton_state with DeviceOn -> true | _ -> false)

  (** Return true iff the current state allows the user to 'resume' a device from the GUI. *)
  method can_resume =
    Recursive_mutex.with_mutex mutex
      (fun () ->
        match !automaton_state with DeviceSleeping -> true | _ -> false)

  (** 'Correctness' support: this is needed so that we can refuse to start incorrectly
      placed components such as Ethernet cables of the wrong crossoverness, which the user
      may have created by mistake: *)
  method is_correct = true (* redefined in cables *)

end;;


(* *************************** *
      class common
 * *************************** *)

(** The shared generator for all unique ids: *)
let generator = Counter.make_int_generator ();;

(** General-purpose class with common fields as
    - an automatically generated identifier
    - a (mutable) name; i.e. valid string identifier
    - a (mutable) label (string) not containing '<' and '>' (for prevent conflict with dot)
*)
class id_name_label = fun ?(name="noname") ?(label="") () ->

  (* Some checks over used name and label *)
  let wellFormedLabel x = not (StrExtra.First.matchingp (Str.regexp ".*[><].*") x) in

  let check_name  x =
  	if not (StrExtra.Class.identifierp x)
       	then failwith ("Setting component "^name^": invalid name")
        else x in

  let check_label x =
  	if not (wellFormedLabel x)
        then failwith ("Setting component "^name^": invalid label")
        else (StringExtra.strip x) in

  object (self)

  (** A component has an immutable identifier. *)
  val id : int = generator ()
  method id = id

  (** A component has a mutable name. *)
  val mutable name : string = check_name name

  method get_name   = name
  method set_name x = name <- check_name x

  (* A shortcut for get_name *)
  method      name  = name

  (** A component has a label, usually used by dot drawing the network graph. *)
  val mutable label : string = check_label label

  method get_label : string = label
  method set_label x = label <- check_label x

end;;

(* *************************** *
        class component
 * *************************** *)

(** A component may be a node (machine or device) or a cable (direct, crossover or nullmodem).
    It's simply a thing with a name and an associated (mutable) label. *)
class virtual component =
fun ~(network:< .. >)
    ?(name="noname")
    ?(label="") () ->

  object (self)
  inherit id_name_label ~name ~label ()
  inherit Xforest.interpreter ()

  (** The global network. It's a pain to have to access this via a global variable
      define in Marionnet *)
  val network = network

  method virtual can_suspend : bool
  method virtual can_resume  : bool

  method virtual suspend : unit
  method virtual resume  : unit
end;;


(* *************************** *
          class port
 * *************************** *)

(** Essentially a triple (user_name, user_index, internal_index) *)
class port
  ~port_prefix      (* ex: "eth" or "port" *)
  ~internal_index   (* 0-based numbering *)
  ~user_port_offset
  ()
  =
  let user_index = (internal_index + user_port_offset) in
  let user_name = Printf.sprintf "%s%d" port_prefix user_index
  in
  object
    method user_name      = user_name       (* ex: port1 *)
    method user_index     = user_index      (* ex: 1 *)
    method internal_index = internal_index  (* ex: 0 *)
end;;

(** Just a container of ports: *)
class ['parent] ports_card
  ~network
  ~(parent:'parent)
  ~port_no
  ~port_prefix
  ?(user_port_offset=0)
  () =
 let () = assert (port_no >= 0) in
 let port_array =
   Array.init
     port_no
     (fun i -> new port ~port_prefix ~internal_index:i ~user_port_offset ())
 in
 let port_list = Array.to_list port_array
 in
 object (self)
  method port_no = port_no
  method port_prefix = port_prefix
  method user_port_offset = user_port_offset

  method internal_index_of_user_port_name x =
    (List.find (fun p->p#user_name = x) port_list)#internal_index

  method user_port_index_of_user_port_name x =
    (List.find (fun p->p#user_name = x) port_list)#user_index

  method user_port_name_of_internal_index i =
    (Array.get port_array i)#user_name

  method user_port_index_of_internal_index i =
    (Array.get port_array i)#user_index

  method user_port_name_list = List.map (fun x->x#user_name) port_list

  method private get_my_defects_by_index
   (port_index:int)
   (port_direction:Treeview_defects.port_direction)
   =
    let get column_header = network#defects#get_port_attribute_of
      ~device_name:((parent#get_name):string)
      ~port_prefix
      ~port_index
      ~user_port_offset
      ~port_direction
      ~column_header
      ()
    in
    object
      method loss        : float = get "Loss %"
      method duplication : float = get "Duplication %"
      method flip        : float = get "Flipped bits %"
      method min_delay   : float = get "Minimum delay (ms)"
      method max_delay   : float = get "Maximum delay (ms)"
    end

  method get_my_inward_defects_by_index (port_index:int) =
    self#get_my_defects_by_index port_index Treeview_defects.OutToIn

  method get_my_outward_defects_by_index (port_index:int) =
    self#get_my_defects_by_index port_index Treeview_defects.InToOut

end (** class ports_card *)

(* *************************** *
          class node
 * *************************** *)

(** Machines and routers have MDI ports, switches and hubs have MDI_X a priori.
    Currently, devices are sold with "intelligent" ports, i.e. MDI/MDI-X. *)
type polarity = MDI | MDI_X | Intelligent ;;

(** A node of the network is essentially a container of ports.
    Defects may be added after the creation, using the related method. *)
class virtual node_with_ports_card = fun
   ~network
   ~name
   ?label
   ~(devkind:devkind)
   ~port_no
   ~port_prefix
   ~port_no_min
   ~port_no_max
   ?(user_port_offset=0)
   ?(has_ledgrid=false)
   () ->
   let make_ports_card ~parent ~port_no =
     new ports_card ~network ~parent ~port_no ~port_prefix ~user_port_offset ()
   in
   object (self)
   inherit component ~network ~name ?label ()
   inherit (*the parent:*) [node_with_ports_card] simulated_device ()

   (* Building constant parameters: *)
   method user_port_offset = user_port_offset
   method port_prefix = port_prefix

   val mutable ports_card = None
   initializer ports_card <- Some (make_ports_card ~parent:self ~port_no)
   method ports_card = Option.extract ports_card
   method get_port_no = self#ports_card#port_no
   method set_port_no new_port_no =
     if (new_port_no >= port_no_min) && (new_port_no <= port_no_max)
     then
       ports_card <- Some (make_ports_card ~parent:self ~port_no:new_port_no)
     else invalid_arg "node_with_ports_card#set_port_no"

   method port_no_min = port_no_min
   method port_no_max = port_no_max

   method has_ledgrid = has_ledgrid

   method virtual destroy : unit

  (** 'Static' methods (in the sense of C++/Java). Polarity is used to decide the correct
      kind of Ethernet cable needed to connect a pair of devices: the cable should be
      crossover iff both endpoints have the same polarity: *)
   method virtual polarity : polarity

  (** The kind of the device (if the node is a device). *)
   method devkind = devkind

   method virtual string_of_devkind : string

   (* This is a default, but could be redefined: *)
   method leds_relative_subdir = self#string_of_devkind

   (** Returns an image representig the node with the given iconsize. *)
   method virtual dotImg : iconsize -> string

   (** Returns the label to use for cable representation.
       This method may be redefined (for instance in [world_bridge]). *)
   method dotLabelForEdges (receptname:string) = self#get_label

   (** Returns the port to use for cable representation.
       This method may be redefined (for instance in [world_bridge]). *)
   method dotPortForEdges (receptname:string)  = receptname

  (** A node is represented in dot with an HTML label which is a table
      with a first line containing the name, with a second line containing the node associated image (method [dotImg]),
      and, if the node has a label, a third line containing the label. With the [nodeoptions] parameter one can force,
      for example, the fontsize or fontname for both name and label :
      [ dotTrad ~nodeoptions="fontsize=8" "large" ] *)
   method dotTrad ?(nodeoptions="") (z:iconsize) =
    let label = self#label_for_dot in
    let label_line =
      if label=""
       then ""
       else "<TR><TD><FONT COLOR=\"#3a3936\">"^label^"</FONT></TD></TR>"
    in
    let fontsize   = self#dot_fontsize_statement in
    let nodeoptions = if nodeoptions = "" then "" else (nodeoptions^",") in
    begin
    self#get_name^" ["^fontsize^nodeoptions^"shape=plaintext,label=<
<TABLE BORDER=\"0\" CELLBORDER=\"0\" CELLSPACING=\"0\" CELLPADDING=\"0\">
  <TR><TD>"^self#get_name^"</TD></TR>
  <TR><TD PORT=\"img\"><IMG SRC=\""^(self#dotImg z)^"\"></IMG></TD></TR>
"^label_line^"
</TABLE>>];"
    end

   (* Redefined in User_level_machine as "": *)
   method dot_fontsize_statement = "fontsize=8,"

   (** Could be redefined. *)
   method label_for_dot = self#get_label

   (** make_simulated_device is defined in subclasses, not here  *)

   (* TODO: move it in the network class
     Return the list of cables of which a port of self is an endpoint: *)
   method private get_involved_cables =
     List.filter (fun c->c#is_node_involved self#get_name) network#cables#get

end;;

(* Justa an alias: *)
class type virtual node = node_with_ports_card


class virtual node_with_defects_zone ~network () =
object (self)

  method virtual defects_device_type : string
  method virtual get_name : string
  method virtual get_port_no : int
  method virtual port_prefix : string
  method virtual user_port_offset : int
  method virtual add_destroy_callback : unit Lazy.t -> unit

  method private add_my_defects =
   match
     (network#defects:Treeview_defects.t)#unique_row_exists_with_binding
        "Name"
        self#get_name
   with
   | true ->
       Log.printf2 "The %s %s has already defects defined...\n"
         self#defects_device_type
         self#get_name
   | false ->
       network#defects#add_device
         ~device_name:self#get_name
         ~device_type:self#defects_device_type
         ~port_no:self#get_port_no
         ~port_prefix:self#port_prefix
         ~user_port_offset:self#user_port_offset
         ()

  method private destroy_my_defects =
    Log.printf1 "component \"%s\": destroying my defects.\n" self#get_name;
    network#defects#remove_subtree_by_name self#get_name;

  method private defects_update_port_no new_port_no =
    network#defects#update_port_no
      ~device_name:self#get_name
      ~port_no:new_port_no
      ~port_prefix:self#port_prefix
      ~user_port_offset:self#user_port_offset
      ()

  initializer
    self#add_my_defects;
    self#add_destroy_callback (lazy self#destroy_my_defects);

end

class virtual node_with_defects
  ~network
  ~name
  ?(label="")
  ~devkind
  ~port_no
  ~port_no_min
  ~port_no_max
  ?user_port_offset
  ~port_prefix
  ()
  =
  let network_alias = network in
  object (self)

  inherit node_with_ports_card
    ~network
    ~name
    ~label
    ~devkind
    ~port_no
    ~port_no_min
    ~port_no_max
    ~port_prefix
    ?user_port_offset
    ()
  as self_as_node_with_ports_card

  initializer
    (* TODO: the following line must be moved the a node initializer: *)
    network#add_node (self :> node);
    self#add_destroy_callback (lazy (network#del_node_by_name self#get_name));

  inherit node_with_defects_zone ~network:network_alias () as node_with_defects_zone

  method virtual dotImg : iconsize -> string

  (** Returns the label to use for cable representation.
      For devices, the port X is represented by the string "[X]". *)
  method dotLabelForEdges (receptname:string) =
    let user_index = self#ports_card#user_port_index_of_user_port_name receptname in
    ("["^string_of_int user_index^"]")

  (** Return the string representing the port in cable representation. *
      Ignore the receptname and returns the empty string. *)
  method dotPortForEdges (receptname:string) = ""

  (* REDEFINED: *)
  (* TODO: duplicated code *)
  method set_name new_name =
    let old_name = self#get_name in
    if old_name <> new_name then begin
      network#defects#rename old_name new_name;
      self_as_node_with_ports_card#set_name new_name;
    end;

  (* REDEFINED: *)
  (* TODO: duplicated code *)
  method set_port_no new_port_no =
    let old_port_no = self#get_port_no in
    if new_port_no <> old_port_no then begin
      node_with_defects_zone#defects_update_port_no new_port_no;
      self_as_node_with_ports_card#set_port_no new_port_no;
    end;

  method update_with ~name ~label ~port_no =
  (* No: force because the simulated device may be rebuilded with new values of other parameters *)
  (* if self#update_really_needed ~name ~label ~port_no then *)
    begin
      self#destroy_my_simulated_device;
      self#set_name name;
      self#set_port_no port_no;
      self#set_label label;
    end

end;; (* class node_with_defects *)


(** Common class for hubs, switches and world_gateways
   (routers have a more specialized class): *)
class virtual node_with_ledgrid_and_defects
  ~network
  ~name
  ?(label="")
  ~devkind
  ~port_no
  ~port_no_min
  ~port_no_max
  ?user_port_offset
  ~(port_prefix:string) (* "port" or "eth" *)
  ()
  =
  let network_alias = network in
  object (self)

  inherit node_with_ports_card
    ~network
    ~name
    ~label
    ~devkind
    ~port_no
    ~port_no_min
    ~port_no_max
    ~port_prefix
    ~has_ledgrid:true
    ?user_port_offset
    ()
  as self_as_node_with_ports_card

  initializer
    (* TODO: the following line must be moved the a node initializer: *)
    network#add_node (self :> node);
    self#add_destroy_callback (lazy (network#del_node_by_name self#get_name));
    (* this is correct here: *)
    self#add_my_ledgrid;
    self#add_destroy_callback (lazy self#destroy_my_ledgrid);

  inherit node_with_defects_zone ~network:network_alias () as node_with_defects_zone

  (** Dot adjustments *)

  (** Returns an image representig the node with the given iconsize. *)
  method virtual dotImg : iconsize -> string

  (** Returns the label to use for cable representation.
      For nodes, the port X is represented by the string "[X]". *)
  method dotLabelForEdges (receptname:string) =
    let user_index = self#ports_card#user_port_index_of_user_port_name receptname in
    ("["^string_of_int user_index^"]")

  (** Return the string representing the port in cable representation. *
      Ignore the receptname and returns the empty string. *)
  method dotPortForEdges (receptname:string) = ""

  (** Here we also have to manage LED grids: *)
  method private startup_right_now =
    (* Do as usual... *)
    self_as_node_with_ports_card#startup_right_now;
    (* ...and also show the LED grid: *)
    network#ledgrid_manager#show_device_ledgrid ~id:(self#id) ()


  method private gracefully_shutdown_right_now =
    (* Do as usual... *)
    self_as_node_with_ports_card#gracefully_shutdown_right_now;
    (* ...and also hide the LED grid... *)
    network#ledgrid_manager#hide_device_ledgrid ~id:(self#id) ();


  (** Here we also have to manage LED grids: *)
  method private poweroff_right_now =
    (* Do as usual... *)
    self_as_node_with_ports_card#poweroff_right_now;
    (* ...and also hide the LED grid... *)
    network#ledgrid_manager#hide_device_ledgrid ~id:(self#id) ();

  method ledgrid_image_directory =
   let leds_relative_subdir = self#leds_relative_subdir in
   (Initialization.Path.leds ^ leds_relative_subdir)

  (* may be redefined *)
  method ledgrid_title = self#get_name
  method virtual ledgrid_label : string

  method add_my_ledgrid =
     (* Make a new device LEDgrid: *)
     (network#ledgrid_manager:Ledgrid_manager.ledgrid_manager)#make_device_ledgrid
       ~id:(self#id)
       ~title:(self#get_name)
       ~label:(self#ledgrid_label)
       ~port_no:(self#get_port_no)
       ?port_labelling_offset:user_port_offset
       ~image_directory:self#ledgrid_image_directory
       ();
     (* Set port connection state: *)
     let busy_ports_indexes =
       network#busy_port_indexes_of_node (self :> node_with_ports_card)
     in
     ignore (List.map
               (fun port_index ->
                  (network#ledgrid_manager#set_port_connection_state
                     ~id:self#id
                     ~port:port_index
                     ~value:true
                     ()))
               busy_ports_indexes)

  method destroy_my_ledgrid : unit =
    Log.printf1 "component \"%s\": destroying my ledgrid.\n" self#get_name;
    (network#ledgrid_manager:Ledgrid_manager.ledgrid_manager)#destroy_device_ledgrid
      ~id:(self#id)
      ()

  (* REDEFINED: *)
  method set_name new_name =
    let old_name = self#get_name in
    if old_name <> new_name then begin
      network#defects#rename old_name new_name;
      self_as_node_with_ports_card#set_name new_name;
    end;

  (* REDEFINED: *)
  method set_port_no new_port_no =
    let old_port_no = self#get_port_no in
    if new_port_no <> old_port_no then begin
      node_with_defects_zone#defects_update_port_no new_port_no;
      self_as_node_with_ports_card#set_port_no new_port_no;
    end;

(*  method private update_really_needed ~(name:string) ~(label:string) ~(port_no:int) : bool =
   ((name    <> self#get_name)  ||
    (label   <> self#get_label) ||
    (port_no <> self#get_port_no))*)

  method update_with ~name ~label ~port_no =
  (* No: force because the simulated device may be rebuilded with new values of other parameters *)
  (* if self#update_really_needed ~name ~label ~port_no then *)
    begin
      self#destroy_my_simulated_device;
      self#destroy_my_ledgrid;
      self#set_name name;
      self#set_port_no port_no;
      self#set_label label;
      self#add_my_ledgrid; (* may use all previous properties (including the label) *)
    end


end;;


(* ************************************* *
          class virtual_machine
   (common class for machine and router)
 * ************************************* *)

class virtual virtual_machine_with_history_and_ifconfig
  ~network
  ?epithet   (* An epithet, for instance "debian-lenny-42178" *)
  ?variant
  ?kernel    (* Again an epithet, for instance "2.6.18-ghost" *)
  ?terminal
  ~(history_icon:string)
  ~(ifconfig_device_type:string)
  ?(ifconfig_port_row_completions:Treeview_ifconfig.port_row_completions option)
  ~(vm_installations:Disk.virtual_machine_installations)
  ()
  =
  let epithet = match epithet with
   | Some x -> x
   | None   -> Option.extract vm_installations#filesystems#get_default_epithet
  in
  let kernel = match kernel with
   | Some x -> x
   | None   -> Option.extract vm_installations#kernels#get_default_epithet
  in
  let terminal = match terminal with
   | Some x -> x
   | None   -> (vm_installations#terminal_manager_of epithet)#get_default
  in

  object (self)

  initializer
    self#add_my_ifconfig ?port_row_completions:ifconfig_port_row_completions self#get_port_no;
    self#add_destroy_callback (lazy self#destroy_my_ifconfig);
    self#add_my_history;
    self#add_destroy_callback (lazy self#destroy_my_history);

  (* Paramters *)
  method history_icon = history_icon
  method ifconfig_device_type = ifconfig_device_type

  method private banner =
    (Printf.sprintf "User_level.virtual_machine: setting %s: " self#get_name)

  method sprintf : 'a. ('a, unit, string, string) format4 -> 'a =
    Printf.ksprintf (fun x->self#banner^x)

  method failwith : 'a 'b. ('a, unit, string, string) format4 -> 'b =
    Obj.magic
      (Printf.ksprintf
        (fun x-> let msg = self#banner^x in
                 let () = Log.printf1 "%s\n" msg in
                 failwith msg))

  (** A machine has a Linux filesystem *)
  val mutable epithet : string = epithet
  initializer ignore (self#check_epithet epithet)
  method get_epithet = epithet
  method set_epithet x = epithet <- self#check_epithet x
  method private check_epithet x =
    match (vm_installations#filesystems#epithet_exists x) with
    | true  -> x
    | false -> self#failwith "unknown filesystem %s" x

  (** A machine may have an associated initial variant: *)
  val mutable variant : string option = variant
  initializer ignore (Option.map (self#check_variant) variant)
  method get_variant = variant
  method get_variant_as_string = match variant with None -> "" | Some x -> x
  method set_variant (x:string option) = variant <- (Option.map (self#check_variant) x)
  method private check_variant x =
   let v = vm_installations#variants_of epithet in
   match v#epithet_exists x with
   | true -> x
   | false -> self#failwith "the variant \"%s\" is not available" x

 method get_variant_realpath : string option =
   Option.map (vm_installations#variants_of self#get_epithet)#realpath_of_epithet self#get_variant

  (** A machine has an associated linux kernel, expressed by an epithet: *)
  val mutable kernel : string = kernel
  initializer ignore (self#check_kernel kernel)
  method get_kernel   = kernel
  method set_kernel x = kernel <- self#check_kernel x
  method private check_kernel x =
    match (vm_installations#kernels#epithet_exists kernel) with
    | true -> x
    | false -> self#failwith "unknown kernel \"%s\"" x

  (** A machine can be used accessed in a specific terminal mode. *)
  val mutable terminal : string = terminal
  initializer ignore (self#check_terminal terminal)
  method get_terminal   = terminal
  method set_terminal x = terminal <- self#check_terminal x
  method private check_terminal x =
    match (vm_installations#terminal_manager_of epithet)#is_valid_choice x with
    | true  -> x
    | false -> self#failwith "invalid terminal choice \"%s\"" x

  method get_filesystem_file_name =
      vm_installations#filesystems#realpath_of_epithet (self#get_epithet)

  method get_kernel_file_name =
      vm_installations#kernels#realpath_of_epithet (self#get_kernel)

  method get_kernel_console_arguments : string option =
      vm_installations#get_kernel_console_arguments (self#get_epithet) (self#get_kernel)

  method get_filesystem_relay_script : string option =
      vm_installations#relay_script_of (self#get_epithet)

  method is_xnest_enabled =
      (vm_installations#terminal_manager_of self#get_epithet)#is_xnest (self#get_terminal)

  (* Used only to add a filesystem history device: *)
  method private prefixed_epithet = (vm_installations#prefix ^ self#get_epithet)

  method add_my_history =
   let icon = self#history_icon in
   let name = self#get_name in
   match ((network#history:Treeview_history.t)#number_of_states_with_name name) > 0 with
   | true -> Log.printf1 "The virtual machine %s has already history defined...\n" name
   | false ->
      network#history#add_device
          ~name
          ~prefixed_filesystem:self#prefixed_epithet
          ?variant:self#get_variant
          ~icon
          ()

  method add_my_ifconfig
    ?(port_row_completions:Treeview_ifconfig.port_row_completions option)
    (port_no:int) : unit
   =
   match
     (network#ifconfig:Treeview_ifconfig.t)#unique_row_exists_with_binding
        "Name"
        self#get_name
   with
   | true  -> Log.printf2 "The %s %s has already ifconfig defined...\n" self#ifconfig_device_type self#get_name
   | false ->
      begin
      network#ifconfig#add_device
        ?port_row_completions
        self#get_name
        ifconfig_device_type
        self#get_port_no
      end

  method destroy_my_ifconfig =
    Log.printf1 "component \"%s\": destroying my ifconfig.\n" self#get_name;
    network#ifconfig#remove_subtree_by_name self#get_name;

  method destroy_my_history =
    Log.printf1 "component \"%s\": destroying my history.\n" self#get_name;
    network#history#remove_device_tree self#get_name;

  method update_virtual_machine_with ~name ~port_no kernel =
    network#ifconfig#update_port_no self#get_name port_no;
    network#ifconfig#rename self#get_name name;
    network#history#rename  self#get_name name;
    self#set_kernel kernel;

  method get_states_directory =
    let history = (network#history:Treeview_history.t) in
    (Option.extract history#directory#get)

  method create_cow_file_name_and_thunk_to_get_the_source =
    let history = (network#history:Treeview_history.t) in
    let cow_file_name =
      Printf.sprintf "%s%s"
        (Option.extract history#directory#get)
        (history#add_state_for_device self#get_name)
    in
    (* Thunk that will be used by the simulation level to retreive
       the source cow file to be copied (if needed). The procedure
       looks backward in the tree searching the first ancestor with
       a cow_file_name corresponding to an existing file. If there
       are no existing files, it looks for the optional variant_realpath.*)
    let dynamically_get_the_cow_file_name_source =
      let rec find_first_existing_ancestor cow_file_name =
        match history#get_parent_cow_file_name ~cow_file_name () with
          (* The state hasn't a parent with an existing cow_file_name: its a root.
             The are now two subcases according to the presence of a variant:
             if there is no variant, there is nothing to copy; otherwise the
             file to copy is precisely the variant (its realpath): *)
	  | None ->
	      self#get_variant_realpath

	   (* The state has a parent, but its cow_file_name could be fictive,
	      so we have to distinguish two subcases: *)
	  | Some cow_file_name_parent ->
	      if Cow_files.cow_file_exists
		  ~states_directory:(self#get_states_directory)
		  ~cow_file_name:cow_file_name_parent
		  ()
		then (Some cow_file_name_parent)
		else find_first_existing_ancestor cow_file_name_parent
      in
      fun () -> find_first_existing_ancestor cow_file_name
    in
    (cow_file_name, dynamically_get_the_cow_file_name_source)

end;; (* class virtual_machine_with_history_and_ifconfig *)


(* *************************** *
        class network
* *************************** *)

class type endpoint = object
  method node : node
  method port_index : int
  method user_port_name : string
  method user_port_index : int
  method involved_node_and_port_index : node * int
end

class type virtual cable = object
(*  inherit OoExtra.destroy_methods *)
 inherit component
 inherit [component] simulated_device
 method destroy : unit
 method get_left  : endpoint
 method get_right : endpoint
 method involved_node_and_port_index_list : (node * int) list
 method is_node_involved : string -> bool
 method crossover : bool
 method is_reversed : bool
 method set_reversed : bool -> unit
 method show : string -> string
 method dot_traduction : curved_lines:bool -> labeldistance:float -> string
 method decrement_alive_endpoint_no : unit
 method increment_alive_endpoint_no : unit
 method is_connected : bool
end

(** Class modelling the user-level network *)
class network () =
 let ledgrid_manager = Ledgrid_manager.the_one_and_only_ledgrid_manager in
 object (self)
 inherit Xforest.interpreter ()

 (* TODO: remove these pointers, we have access to these informations
    by ports_card and endpoint: *)
 method defects  = Treeview_defects.extract ()
 method ifconfig = Treeview_ifconfig.extract ()
 method history  = Treeview_history.extract ()

 method motherboard = Motherboard.extract ()

 (* Note that the default equality is not suitable because the inner value is a Queue.t, 
    that is to say an immutable reference. Thus, we have to redefine it. 
    Important note: in order to define the equality correctly, we exploit the partial application.
    Actually, when the Cortex will be solicited for an evaluation, it will call this function on
    its current value to obtain a predicate for committed values. *)
 val nodes : (node Queue.t) Cortex.t = 
   let equality xs = (* just an argument here! we use it to define a predicate *)
     let xs' = QueueExtra.to_list xs in 
     fun ys -> (QueueExtra.to_list ys) = xs'
   in  
   Cortex.return ~equality (Queue.create ())
   
 method nodes = nodes
 method get_node_list       = Cortex.apply nodes (QueueExtra.to_list)
 method set_node_list xs    = Cortex.set   nodes (QueueExtra.of_list xs)
 method is_node_queue_empty = Cortex.apply nodes (Queue.is_empty) 
 
 val cables : cable Chip.wlist = Chip.wlist ~name:"network#cables" []
 method cables = cables

 (** Buffers to backup/restore data. *)
 val mutable nodes_buffer  : (node list) = []
 val mutable cables_buffer : (cable list) = []

 (** Accessors *)
 method ledgrid_manager = ledgrid_manager

 (** Related dot options fro drawing this virtual network.
     This pointer is shared with the project instance. *)
 val mutable dotoptions : (Dot_tuning.network option) = None
 method      dotoptions   = match dotoptions with Some x -> x | None -> raise (Failure "network#dotoptions")
 method  set_dotoptions x = dotoptions <- Some x

 method components : (component list) =
   List.append
     (self#get_node_list :> component list)
     (cables#get :> component list) (* CABLES MUST BE AT THE FINAL POSITION for marshaling !!!! *)

 method components_of_kind ?(kind:[`Node | `Cable] option) () =
   match kind with
   | None        -> self#components
   | Some `Node  -> (self#get_node_list :> (component list))
   | Some `Cable -> (cables#get :> (component list))

 method disjoint_union_of_nodes_and_cables : ((component * [`Node | `Cable]) list) =
   let xs = List.map (fun x -> x,`Node ) (self#get_node_list :> component list)  in
   let ys = List.map (fun x -> x,`Cable) (cables#get :> component list)  in
   List.append xs ys

 (** Setter *)

 (* The optional parameter [scheduled=true] means that this method is called
    in a task managed by the Task_runner. In this case, we have not to call
    the task runner method [wait_for_all_currently_scheduled_tasks]. *)
 method reset ?(scheduled=false) () =
  begin
   Log.printf "---\n";
   Log.printf "network#reset: begin\n\tDestroying all cables...\n";
   (List.iter
      (fun cable -> try cable#destroy with _ -> ())
      cables#get);
   Log.printf "\tDestroying all nodes (machines, switchs, hubs, routers, etc)...\n";
   (List.iter
      (fun node -> try node#destroy with _ -> ())
      (self#get_node_list));
   Log.printf "\tSynchronously wait that everything terminates...\n";
   (if not scheduled then Task_runner.the_task_runner#wait_for_all_currently_scheduled_tasks);
   Log.printf "\tMaking the network graph empty...\n";
   (self#set_node_list []);
   cables#set  [] ;
   Log.printf "\tWait for all devices to terminate...\n";
   (** Make sure that all devices have actually been terminated before going
       on: we don't want them to lose filesystem access: *)
   Log.printf "\tAll devices did terminate.\nnetwork#reset: end (success)\n---\n";
  end

 method destroy_process_before_quitting () =
  begin
   Log.printf "destroy_process_before_quitting: BEGIN\n";
   (List.iter (fun cable -> try cable#destroy_right_now with _ -> ())   cables#get);
   (List.iter (fun device -> try device#destroy_right_now with _ -> ()) (self#get_node_list));
   Log.printf "destroy_process_before_quitting: END (success)\n";
  end

 method restore_from_buffers =
  begin
   self#reset ();
   (self#set_node_list nodes_buffer);
   cables#set cables_buffer;
  end

 method save_to_buffers =
  begin
   nodes_buffer  <- self#get_node_list;
   cables_buffer <- cables#get;
  end

 method to_tree =
   let l = List.map (fun x->x#to_tree) self#components in
   let root = ("network",[]) in
   let children = Forest.of_treelist l in
   (root, children)

 method to_forest =
   Forest.of_tree self#to_tree

 val try_to_add_procedure_list= ref []
 method subscribe_a_try_to_add_procedure p =
   try_to_add_procedure_list := p::(!try_to_add_procedure_list)

 (** We redefine just the interpretation of a children.
     We ignore (in this version) network attributes. *)
 method eval_forest_child (f:Xforest.tree) : unit =
  let xs = List.rev !try_to_add_procedure_list in
  let result = List.exists (fun p -> p self f) xs in
  match result with
  | true -> ()
  | false ->
      let ((nodename, attrs), _) = f in
        let name  = List.assoc "name" attrs in
        (Log.printf2 "network#eval_forest_child: I can't interpret this \"%s\" name \"%s\".\n" nodename name)
        (* Forward-compatibility *)

 method names = (List.map (fun x->x#get_name) self#components)

 method suggestedName prefix =
   let rec tip prefix k =
     begin
     let prop = (prefix^(string_of_int k)) in
     if self#name_exists prop then tip prefix (k+1) else prop
     end in tip prefix 1

 method get_node_by_name n =
   try List.find (fun x->x#get_name=n) (self#get_node_list)  with _ -> failwith ("get_node_by_name "^n)

 method get_cable_by_name n =
   try List.find (fun x->x#get_name=n) cables#get with _ -> failwith ("get_cable_by_name "^n)

 method get_component_by_name ?kind n =
   let components = self#components_of_kind ?kind () in
   try List.find (fun x->x#get_name=n) components with _ -> failwith ("get_component_by_name "^n)

 method involved_node_and_port_index_list =
   List.flatten (List.map (fun c->c#involved_node_and_port_index_list) cables#get)

 method busy_port_indexes_of_node (node:node) =
   let node_name = node#get_name in
   let related_busy_pairs =
     List.filter
       (fun (node, port_index) -> node#get_name = node_name)
        self#involved_node_and_port_index_list
   in
   List.map snd related_busy_pairs

 method free_port_indexes_of_node ?(force_to_be_included:(int list)=[]) (node:node) =
   let node_port_indexes = ListExtra.range 0 (node#get_port_no-1) in
   let busy_port_indexes =
     ListExtra.substract (self#busy_port_indexes_of_node node) force_to_be_included
   in
   ListExtra.substract node_port_indexes busy_port_indexes

 method free_user_port_names_of_node ?(force_to_be_included=[]) node =
   (* force_to_be_included expressed now by indexes: *)
   let force_to_be_included =
      List.map (node#ports_card#internal_index_of_user_port_name) force_to_be_included
   in
   List.map
     (node#ports_card#user_port_name_of_internal_index)
     (self#free_port_indexes_of_node ~force_to_be_included node)

 method free_endpoint_list_humanly_speaking
  ?(force_to_be_included:((string*string) list)=[])
  : (string * string) list
  =
  let npss =
    List.map
      (fun node ->
	  let n = node#get_name in
	  let force_to_be_included =
	    List.map snd (List.filter (fun (n0,p0) -> n0=n) force_to_be_included)
	  in
	  (List.map (fun p -> (n,p)) (self#free_user_port_names_of_node ~force_to_be_included node))
	)
	(self#get_node_list)
  in List.concat npss

 (* Unused...*)
(* method is_endpoint_free endpoint =
   let busy_pairs = self#involved_node_and_port_index_list in
   List.iter (function (n,p) -> Log.printf "Involved: (%s,%d)\n" n#get_name p) busy_pairs;
   not (List.mem (endpoint#involved_node_and_port_index) busy_pairs)*)

 (* The total number of endpoints in the network: *)
 method private endpoint_no =
   let sum xs = List.fold_left (+) 0 xs in
   sum (List.map (fun node -> node#get_port_no) (self#get_node_list))

 method are_there_almost_2_free_endpoints : bool =
    let busy_no = List.length (self#involved_node_and_port_index_list) in
    ((self#endpoint_no - busy_no) >= 2)

 (** The max index among busy receptacles of a given kind of a given node.
     The user cannot change the number of receptacle of the given node to a number less than this index+1.
     For instance, if the (max_busy_receptacle_index "rome" Eth) = 2 then the user can change
     the number of receptacle of rome but only with a number >= 3.  *)
 method max_busy_port_index_of_node node =
   let indexes = self#busy_port_indexes_of_node node in
   if indexes=[] then -1 else ListExtra.max indexes

  (** Useful updating a device: *)
 method port_no_lower_of node =
  let port_no_lower = node#port_no_min in
  let min_port_no = (self#max_busy_port_index_of_node node + 1) in
  let k = float_of_int port_no_lower in
  (* minimum multiple of k containing min_port_no: *)
  let min_multiple = (ceil ((float_of_int min_port_no) /. k)) *. k in
  int_of_float (max min_multiple k)

 method node_exists  n = let f=(fun x->x#get_name=n) in (List.exists f (self#get_node_list))
 method cable_exists n = let f=(fun x->x#get_name=n) in (List.exists f cables#get)
 method name_exists  n = List.mem n self#names

 (** Adding components *)

 (** Nodes must have a unique name in the network *)
 method add_node (node:node) =
    if (self#name_exists node#get_name) then
      failwith "User_level.network#add_node: name already used in the network"
    else
      (Cortex.apply nodes (Queue.push node))

 (** Remove a node from the network. Remove it from the node list
     and remove all related cables. TODO: change this behaviour! *)
 method del_node_by_name (node_name:string) =
     let node = self#get_node_by_name (node_name) in
     (* Destroy cables first: they refer what we're removing... *)
     let cables_to_destroy = List.filter (fun c->c#is_node_involved node_name) cables#get in
     (* The cable#destroy will call itself the network#del_cable_by_name: *)
     let () = List.iter (fun cable -> cable#destroy) cables_to_destroy in
     (* nodes#filter (fun x->not (x=node)) *)
     (Cortex.apply nodes (QueueExtra.filter ((<>)node)))

 (** Cable must connect free ports: *)
 (* TODO: manage ledgrid with a reactive system!!!*)
 method add_cable (c:cable) =
    if (self#name_exists c#get_name)
    then failwith "User_level.network#add_cable: name already used in the network"
    else cables#append c

 (** Remove a cable from network. Called by cable#destroy. *)
 method del_cable_by_name (cable_name) =
     let c = self#get_cable_by_name (cable_name) in
     cables#filter (fun x->not (x=c))

 method change_node_name (old_name) (new_name) =
   if old_name = new_name then () else
   let node = self#get_node_by_name (old_name) in
   node#set_name (new_name)

 (** Facilities *)

 (** List of node names in the network *)
 method get_node_names  =
   List.map (fun x->x#get_name) (self#get_node_list)

 method private predicate_of_optional_devkind ?devkind () =
  match devkind with
  | Some devkind -> (fun x -> x#devkind = devkind)
  | None         -> (fun x -> true)

 method get_nodes_such_that ?devkind (predicate) =
  let devkindp = self#predicate_of_optional_devkind ?devkind () in
  List.filter (fun x -> (devkindp x) && (predicate x)) (self#get_node_list)

 (* --- can_startup --- *)

 method get_nodes_that_can_startup ?devkind () =
  self#get_nodes_such_that ?devkind (fun x -> x#can_startup)

 method get_node_names_that_can_startup ?devkind () =
  List.map (fun x -> x#get_name) (self#get_nodes_that_can_startup ?devkind ())

 (* --- can_gracefully_shutdown --- *)

 method get_nodes_that_can_gracefully_shutdown ?devkind () =
  self#get_nodes_such_that ?devkind (fun x -> x#can_gracefully_shutdown)

 method get_node_names_that_can_gracefully_shutdown ?devkind () =
  List.map (fun x -> x#get_name) (self#get_nodes_that_can_gracefully_shutdown ?devkind ())

 (* --- can_suspend --- *)

 method get_nodes_that_can_suspend ?devkind () =
  self#get_nodes_such_that ?devkind (fun x -> x#can_suspend)

 method get_node_names_that_can_suspend ?devkind () =
  List.map (fun x -> x#get_name) (self#get_nodes_that_can_suspend ?devkind ())

 (* --- can_resume --- *)

 method get_nodes_that_can_resume ?devkind () =
  self#get_nodes_such_that ?devkind (fun x -> x#can_resume)

 method get_node_names_that_can_resume ?devkind () =
  List.map (fun x -> x#get_name) (self#get_nodes_that_can_resume ?devkind ())

 (* Including cables (suspend=disconnect, resume=reconnect). The boolean in the result
    indicates if the component is suspended (sleeping): *)
 method get_component_names_that_can_suspend_or_resume () : (string * [`Node|`Cable] * bool) list =
  ListExtra.filter_map
    (fun (x, node_or_cable) ->
       let can_suspend = x#can_suspend in
       let can_resume  = lazy x#can_resume in
       if  can_suspend || (Lazy.force can_resume)
         then Some (x#get_name, node_or_cable, (Lazy.force can_resume))
         else None)
    self#disjoint_union_of_nodes_and_cables

 (** List of direct cable names in the network *)
 method get_direct_cable_names  =
   let clist = List.filter (fun x->x#crossover=false) cables#get in
   List.map (fun x->x#get_name) clist

 (** List of crossover cable names in the network *)
 method get_crossover_cable_names =
   let clist= List.filter (fun x->x#crossover=true) cables#get in
   List.map (fun x->x#get_name) clist

 method get_direct_cables =
   List.filter (fun x->x#crossover=false) cables#get

 method get_crossover_cables  =
   List.filter (fun x->x#crossover=true) cables#get

 (** Starting and showing the network *)

 (** List of reversed cables (used only for drawing network) *)
 method reversed_cables : (string list) =
   let clist= List.filter (fun x->x#is_reversed) cables#get in
   List.map (fun x->x#get_name) clist

 (** Set the reversed dotoptions field of a cable of the network (identified by name) *)
 method reversed_cable_set (x:bool) (cname:string) =
   (self#get_cable_by_name cname)#set_reversed x

 (** Show network topology *)
 method show =
   Log.printf "========== NETWORK STATUS ===========\n";
   (* show nodes *)
   let msg= try
        (String.concat " , "
        (List.map (fun d->d#get_name^" ("^(d#string_of_devkind)^")") (self#get_node_list)))
        with _ -> ""
   in Log.printf1 "Nodes \r\t\t: %s\n" msg;
  (* show links *)
   let msg=try
        (String.concat "\n" (List.map (fun c->(c#show "\r\t\t  ")) cables#get))
        with _ -> ""
   in Log.printf1 "Cables \r\t\t: %s\n" msg


 (** {b Consider cable as Edge.edges} *)

 (** Network translation into the dot language *)
 method dotTrad () =
 let opt = self#dotoptions in
 let labeldistance = opt#labeldistance#get in
 let curved_lines = opt#curved_lines#get in
 begin
"digraph plan {

"^opt#ratio^"
"^opt#rankdir_for_dot^"
"^opt#nodesep_for_dot^";"^"

/* ***************
        NODES
   *************** */

"^
(StringExtra.Text.to_string
   (List.map
     (fun (n:node)->n#dotTrad opt#iconsize_for_dot)
     (ListExtra.permute opt#shuffler_as_function (self#get_node_list))
   ))
^"
/* ***********************
      DIRECT CABLE EDGES
   *********************** */


edge [dir=none,color=\""^self#dotoptions#direct_cable_color^"\",fontsize=8,labelfontsize=8,minlen=1.6,"^
opt#labeldistance_for_dot^",tailclip=true];

"^
(StringExtra.Text.to_string
   (List.map (fun c->c#dot_traduction ~curved_lines ~labeldistance) self#get_direct_cables))

^"
/* *********************************
      CROSSOVER/SERIAL CABLE EDGES
   ********************************* */


edge [headclip=true,minlen=1.6,color=\""^self#dotoptions#crossover_cable_color^"\",weight=1];

"^
(StringExtra.Text.to_string
   (List.map (fun c->c#dot_traduction ~curved_lines ~labeldistance) self#get_crossover_cables))

^"} //END of digraph\n"

 end (* method dotTrad *)

initializer

 self#set_dotoptions (new Dot_tuning.network self);

end


(** {2 Saving and loading a Netmodel.network } *)

(** Pseudo XML now! (using xforest instead of ocamlduce) *)
module Xml = struct

 let network_marshaller = new Oomarshal.marshaller ;;

 (** Parse the file containing an xforest representation of the network.
     The given network is updated during the parsing. *)
 let load_network ~(project_version: [`v0|`v1|`v2]) (net:network) (fname:string) =
  let (forest:Xforest.t) = 
    match project_version with
    | `v2 | `v1 -> network_marshaller#from_file (fname)
    | `v0       -> Forest_backward_compatibility.load_from_old_file (fname)
  in
  (* we are manually setting the verbosity 3 *)
  (if (Global_options.Debug_level.get ()) >= 3 then Xforest.print_xforest ~channel:stderr forest);
  match Forest.to_tree forest with
  | (("network", attrs), children) -> net#from_tree ("network", attrs) children
  | _ -> assert false
 ;;

(** Save the xforest representation of the network. *)
let save_network (net:network) (fname:string) =
 Log.printf "Netmodel.Xml.save_network: begin\n";
 (* we are manually setting the verbosity 3 *)
 (if (Global_options.Debug_level.get ()) >= 3 then Xforest.print_xforest ~channel:stderr net#to_forest);
 network_marshaller#to_file net#to_forest fname;
 Log.printf "Netmodel.Xml.save_network: end (success)\n";;

end;; (* module Netmodel.Xml *)
