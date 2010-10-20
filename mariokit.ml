(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007  Jean-Vincent Loddo
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


(** Some modules for managing the virtual network *)

(*open Sugar;;*)
open UnixExtra;;
open Environment;;
open Oomarshal;;
open Task_runner;;
open Simple_dialogs;;
open Gettext;;

module Recursive_mutex = MutexExtra.Recursive ;;

(** Some constants for drawing with colors. *)
module Color = struct
 let direct_cable    = "#949494" ;;
 let crossover_cable = "#6d8dc0" ;;
end;;

(** A thunk allowing to invoke the sketch refresh method, accessible from many
    modules: *)
module Refresh_sketch_thunk = Stateful_modules.Variable (struct type t = unit->unit end)
let refresh_sketch () = Refresh_sketch_thunk.get () ()


(* *************************** *
        Module Dotoptions
 * *************************** *)

module Dotoptions = struct


(* *************************
    class Dotoptions.network
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

class network =

  fun ?(iconsize="large") ?(shuffler=[]) ?(rankdir="TB") ?(nodesep=0.5) ?(labeldistance=1.6) ?(extrasize=0.)

      (* The handler for the real network *)
      (network:( < reverted_cables:(string list); reverted_cable_set:(bool->string->unit); .. > ))  ->

  object (self)
  inherit Xforest.interpreter ()

  val iconsize = Chip.wref ~name:"iconsize" iconsize
  method iconsize = iconsize

  val rankdir  = Chip.wref ~name:"rankdir" rankdir
  method rankdir = rankdir

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
      nodesep#set 0.5;
      labeldistance#set 1.6 ;
      ListExtra.foreach network#reverted_cables (network#reverted_cable_set false) ;
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
      (the dynamic menus are recalculated each time from network#reverted_cables. *)

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
    Xforest.print_forest network#to_forest;
    network_marshaller#to_file self#to_forest file_name

  (** This method is used just for undumping dotoptions, so is not strict.
      For instance, exceptions provoked by bad cable names are simply ignored. *)
  method set_reverted_cables names =
    ListExtra.foreach names (fun n -> try (network#reverted_cable_set true n) with _ -> ())

  (** Undump the state of [self] from the given file. *)
  method load_from_file (file_name : string) =
   let forest = network_marshaller#from_file file_name in
   Xforest.print_forest forest;
   match forest with
   | Forest.NonEmpty (("dotoptions", attrs) , childs , Forest.Empty) ->
      self#from_forest ("dotoptions", attrs) childs
   | _ -> assert false

 (** Dotoptions to forest encoding. *)
  method to_forest =
   Forest.leaf ("dotoptions", [
    		   ("iconsize"      , iconsize#get                   ) ;
    		   ("shuffler"      , (Xforest.encode shuffler#get)  ) ;
                   ("rankdir"       , rankdir#get                    ) ;
                   ("nodesep"       , (string_of_float nodesep#get)      ) ;
                   ("labeldistance" , (string_of_float labeldistance#get)) ;
                   ("extrasize"     , (string_of_float extrasize#get)    ) ;
                   ("gui_callbacks_disable", (string_of_bool gui_callbacks_disable)) ;
                   ("invertedCables", (Xforest.encode network#reverted_cables)) ;
	           ])

 (** A Dotoption.network has just attributes (no childs) in this version.
     The Dotoption.network must be undumped AFTER the Netmodel.network in
     order to have significant cable names (reverted_cables). *)
 method eval_forest_attribute = function
  | ("iconsize"             , x ) -> self#iconsize#set       x
  | ("shuffler"             , x ) -> self#shuffler#set      (Xforest.decode x)
  | ("rankdir"              , x ) -> self#rankdir#set        x
  | ("nodesep"              , x ) -> self#nodesep#set       (float_of_string x)
  | ("labeldistance"        , x ) -> self#labeldistance#set (float_of_string x)
  | ("extrasize"            , x ) -> self#extrasize#set     (float_of_string x)
  | ("gui_callbacks_disable", x ) -> self#set_gui_callbacks_disable (bool_of_string x)
  | ("invertedCables"       , x ) -> self#set_reverted_cables (Xforest.decode x)
  | _ -> () (* Forward-comp. *)

end;; (* class Dotoptions.network *)


(** Dot options for a cable *)
class cable ?(reverted=false) ~(motherboard:State_types.motherboard) ~name () =
 object (self)
(*   val reverted = motherboard#reverted_rj45cables_cable#create_wref ~name:(name^"_reverted") reverted*)
   val reverted = Chip.wref_in_cable ~name:(name^"_reverted") ~cable:motherboard#reverted_rj45cables_cable reverted
   method reverted = reverted
   method destroy =
    reverted#destroy
 end;; (* class Dotoptions.cable *)


end;; (* module Dotoptions *)


(* **************************************** *
              Module Netmodel
 * **************************************** *)


(** Model for managing the virtual network defined step by step by user *)
module Netmodel = struct

(** {2 Basic functions and types } *)

(** A device may be a Hub, a Switch or a Router. *)
type devkind   = Hub | Switch | Router | World_gateway | NotADevice ;;

(** A cable may be Direct or Crossover. *)
type cablekind = Direct | Crossover ;;

(** A port (of machine or device) may Eth (ethernet). *)
type portkind  = Eth ;;

(** A node is a Machine, a Device, a Cloud or a World_bridge.
    Cable are edge in the network, which is a graph of these nodes. *)
type nodekind  = Machine | Device | Cloud | World_bridge  ;;

(** String conversion for a devkind. *)
let string_of_devkind = function
  | Hub        -> "hub"
  | Switch     -> "switch"
  | Router     -> "router"
  | World_gateway -> "world_gateway"
  | NotADevice -> raise (Failure "string_of_devkind: NotADevice")
;;

(** String conversion for a cablekind. *)
let string_of_cablekind = function
  | Direct     -> "direct"
  | Crossover  -> "crossover"
;;

(** String conversion for a portkind. *)
let string_of_portkind = function
  | Eth   -> "eth"
;;

(** The portkind interpretation of the given string. *)
let devkind_of_string x = match x with
  | "hub"     -> Hub
  | "switch"  -> Switch
  | "router"  -> Router
  | "world_gateway"  -> World_gateway
  | _         -> raise (Failure ("devkind_of_string"^x))
;;

(** The cablekind interpretation of the given string. *)
let cablekind_of_string x = match x with
  | "direct"    -> Direct
  | "crossover" -> Crossover
  | _           -> failwith ("cablekind_of_string: "^x)
;;


(** Examples: pc1, pc2 if the node is a machines, A,B,C if the node is a device *)
type nodename   = string ;;

(** Examples: eth0..eth4 *)
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
 Log.printf "ForbiddenTransition raised in %s\n" msg;
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
  val simulated_device : 'parent Simulated_network.device option ref =
    ref None

  method get_hublet_process_of_port index =
    match !simulated_device with
    | Some (sd) -> sd#get_hublet_process_of_port index
    | None      -> failwith "looking for a hublet when its device is non-existing"

  (** Create a new simulated device according to the current status *)
  method virtual make_simulated_device : 'parent Simulated_network.device

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
    the_task_runner#schedule
      ~name:text
      (fun () ->
        (try
          progress_bar := Some (make_progress_bar_dialog ~title:text ());
          thunk ();
        with e -> begin
          Log.printf "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n";
          let message =
            Printf.sprintf "enqueue_task_with_progress_bar: %s %s failed (%s)"
              verb self#get_name (Printexc.to_string e) in
          Log.printf "%s\n" message;
          Simple_dialogs.warning message message ();
          Log.printf "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n";
          flush_all ();
        end));
    the_task_runner#schedule
      ~name:("Destroy the progress bar for \"" ^ text ^ "\"")
      (fun () ->
        match !progress_bar with
          Some progress_bar ->
            destroy_progress_bar_dialog progress_bar
        | None ->
            assert false)

(*  method reboot_if_possible =
    try
      self#suspend;
      self#startup;
    with _ ->
      () *)

  method create =
    (* This is invisible for the user: don't set the next state *)
    the_task_runner#schedule ~name:("create "^self#get_name) (fun () -> self#create_right_now)

  method (*private*) destroy_my_simulated_device =
    Log.printf "component \"%s\": destroying my simulated device.\n" self#get_name;
    (* This is invisible for the user: don't set the next state *)
    the_task_runner#schedule ~name:("destroy "^self#get_name)(fun () -> self#destroy_right_now)

  method startup =
    self#set_next_simulated_device_state (Some DeviceOn);
    self#enqueue_task_with_progress_bar (s_ "Starting") (fun () -> if self#can_startup then self#startup_right_now)

  method suspend =
    self#set_next_simulated_device_state (Some DeviceSleeping);
    self#enqueue_task_with_progress_bar (s_ "Suspending") (fun () -> if self#can_suspend then self#suspend_right_now)

  method resume =
    self#set_next_simulated_device_state (Some DeviceOn);
    self#enqueue_task_with_progress_bar (s_ "Resuming") (fun () -> if self#can_resume then self#resume_right_now)

  method gracefully_shutdown =
    self#set_next_simulated_device_state (Some DeviceOff);
    self#enqueue_task_with_progress_bar (s_ "Stopping") (fun () -> if self#can_gracefully_shutdown then self#gracefully_shutdown_right_now)

  method poweroff =
    self#set_next_simulated_device_state (Some DeviceOff);
    self#enqueue_task_with_progress_bar (s_ "Shutting down") (fun () -> if self#can_poweroff then self#poweroff_right_now)

  method (*private*) create_right_now =
    Recursive_mutex.with_mutex mutex
      (fun () ->
        Log.printf "About to create the simulated device %s: it's connected to %d cables.\n"
          self#get_name
          (List.length (self#get_involved_cables));
          
        match !automaton_state, !simulated_device with
        | NoDevice, None ->
	    (simulated_device := (Some self#make_simulated_device);
	      automaton_state := DeviceOff;
	      self#set_next_simulated_device_state None;
	      (* An endpoint for cables linked to self was just added; we need to start some cables. *)
	      ignore (List.map
			(fun cable ->
			   Log.printf "Working on cable %s\n" (cable#show "");
			   cable#increment_alive_endpoint_no)
			(self#get_involved_cables)))

        | _ -> raise_forbidden_transition "create_right_now")

  (** The unit parameter is needed: see how it's used in simulated_network: *)
  method private destroy_because_of_unexpected_death () =
    Log.printf "You don't deadlock here %s, do you? -1\n" self#get_name;
    Recursive_mutex.with_mutex mutex
      (fun () ->
        Log.printf "You don't deadlock here %s, do you? 0\n" self#get_name;
        (try
          self#destroy_right_now
        with e -> begin
          Log.printf "WARNING: destroy_because_of_unexpected_death: failed (%s)\n"
            (Printexc.to_string e);
        end;
          self#set_next_simulated_device_state None)); (* don't show next-state icons for this *)

  method (*private*) destroy_right_now =
    Recursive_mutex.with_mutex mutex
      (fun () ->
        Log.printf "About to destroy the simulated device %s \n" self#get_name;
        match !automaton_state, !simulated_device with
        | (DeviceOn | DeviceSleeping), Some(d) ->
             Log.printf
               "  (destroying the on/sleeping device %s. Powering it off first...)\n"
               self#get_name;
             self#poweroff_right_now; (* non-gracefully *)
             self#destroy_right_now
        | NoDevice, None ->
            Log.printf
             "  (destroying the already 'no-device' device %s. Doing nothing...)\n"
             self#get_name;
            () (* Do nothing, but don't fail. *)
        | DeviceOff, Some(d) ->
            ((* An endpoint for cables linked to self was just added; we
                may need to start some cables. *)
             Log.printf
               "  (destroying the off device %s: decrementing its cables rc...)\n"
               self#get_name;
             List.iter
               (fun cable ->
                 Log.printf "Unpinning the cable %s " (cable#show "");
                 cable#decrement_alive_endpoint_no;
                 Log.printf ("The cable %s was unpinned with success\n") (cable#show "");
                 )
               self#get_involved_cables;
             Log.printf "  (destroying the simulated device implementing %s...)\n" self#get_name;
             d#destroy; (* This is the a method from some object in Simulated_network *)
             simulated_device := None;
             automaton_state := NoDevice;
             self#set_next_simulated_device_state None;
             Log.printf "We're not deadlocked yet (%s). Great.\n" self#get_name);
        | _ ->
            raise_forbidden_transition "destroy_right_now"
        );
    Log.printf "The simulated device %s was destroyed with success\n" self#get_name


  method (*private*) startup_right_now =
    Recursive_mutex.with_mutex mutex
      (fun () ->
        (* Don't startup ``incorrect'' devices. This is currently limited to cables of the
           wrong crossoverness which the user has defined by mistake: *)
        if self#is_correct then begin
          Log.printf "Starting up the device %s...\n" self#get_name;
          match !automaton_state, !simulated_device with
          | NoDevice, None ->
             (Log.printf "  (creating processes for %s first...)\n" self#get_name;
              self#create_right_now;
              Log.printf "  (processes for %s were created...)\n" self#get_name;
              self#startup_right_now
              )

          | DeviceOff, Some(d) ->
             (d#startup;  (* This is the a method from some object in Simulated_network *)
              automaton_state := DeviceOn;
              self#set_next_simulated_device_state None;
              Log.printf "The device %s was started up\n" self#get_name
              )

          | DeviceOn,  _ ->
              Log.printf "startup_right_now: called in state %s: nothing to do.\n" (self#automaton_state_as_string)

          | _ -> raise_forbidden_transition "startup_right_now"
        end else begin
          Log.printf "REFUSING TO START UP the ``incorrect'' device %s!!!\n" self#get_name
        end)

  method (*private*) suspend_right_now =
    Recursive_mutex.with_mutex mutex
      (fun () ->
        Log.printf "Suspending up the device %s...\n" self#get_name;
        match !automaton_state, !simulated_device with
          DeviceOn, Some(d) ->
           (d#suspend; (* This is the a method from some object in Simulated_network *)
            automaton_state := DeviceSleeping;
            self#set_next_simulated_device_state None)
        | _ -> raise_forbidden_transition "suspend_right_now")

  method (*private*) resume_right_now =
    Recursive_mutex.with_mutex mutex
      (fun () ->
        Log.printf "Resuming the device %s...\n" self#get_name;
        match !automaton_state, !simulated_device with
        | DeviceSleeping, Some(d) ->
           (d#resume; (* This is the a method from some object in Simulated_network *)
            automaton_state := DeviceOn;
            self#set_next_simulated_device_state None)

        | _ -> raise_forbidden_transition "resume_right_now")

  method (*private*) gracefully_shutdown_right_now =
    Recursive_mutex.with_mutex mutex
      (fun () ->
        let current_state = self#automaton_state_as_string in
        (Log.printf "* Gracefully shutting down the device %s (from state: %s)...\n"
          self#get_name
          current_state);
        match !automaton_state, !simulated_device with
        | DeviceOn, Some(d) ->
           (d#gracefully_shutdown; (* This is the a method from some object in Simulated_network *)
            automaton_state := DeviceOff;
            self#set_next_simulated_device_state None)

        | DeviceSleeping, Some(d) ->
           (self#resume_right_now;
            self#gracefully_shutdown_right_now)

        | NoDevice,  _ | DeviceOff, _ ->
            Log.printf "gracefully_shutdown_right_now: called in state %s: nothing to do.\n" (self#automaton_state_as_string)

        | _ -> raise_forbidden_transition "gracefully_shutdown_right_now")

  method (*private*) poweroff_right_now =
    Recursive_mutex.with_mutex mutex
      (fun () ->
        Log.printf "Powering off the device %s...\n" self#get_name;
        match !automaton_state, !simulated_device with
        | DeviceOn, Some(d) ->
           (d#shutdown; (* non-gracefully *)
            automaton_state := DeviceOff;
            self#set_next_simulated_device_state None)

        | DeviceSleeping, Some(d) ->
            (self#resume_right_now;
             self#poweroff_right_now)

        | NoDevice,  _ | DeviceOff, _ ->
            Log.printf "poweroff_right_now: called in state %s: nothing to do.\n" (self#automaton_state_as_string)

        | _ -> raise_forbidden_transition "poweroff_right_now")

  (** Return true iff the current state allows to 'startup' the device from the GUI. *)
  method can_startup =
    Recursive_mutex.with_mutex mutex
      (fun () ->
        match !automaton_state with NoDevice | DeviceOff -> true | _ -> false)

  (** Return true iff the current state allows to 'shutdown' a device from the GUI. *)
  method can_gracefully_shutdown =
    Recursive_mutex.with_mutex mutex
      (fun () ->
        match !automaton_state with DeviceOn | DeviceSleeping -> true | _ -> false)

  (** Return true iff the current state allows to 'power off' a device from the GUI. *)
  method can_poweroff =
    Recursive_mutex.with_mutex mutex
      (fun () ->
        match !automaton_state with NoDevice | DeviceOff -> false | _ -> true)

  (** Return true iff the current state allows to 'suspend' a device from the GUI. *)
  method can_suspend =
    Recursive_mutex.with_mutex mutex
      (fun () ->
        match !automaton_state with DeviceOn -> true | _ -> false)

  (** Return true iff the current state allows to 'resume' a device from the GUI. *)
  method can_resume =
    Recursive_mutex.with_mutex mutex
      (fun () ->
        match !automaton_state with DeviceSleeping -> true | _ -> false)

  (** 'Correctness' support: this is needed so that we can refuse to start incorrectly
      placed components such as Ethernet cables of the wrong crossoverness, which the user
      may have created by mistake: *)
  val is_correct =
    ref true (* devices are ``correct'' by default *)
  method is_correct =
    Recursive_mutex.with_mutex mutex
      (fun () ->
        !is_correct)
  method set_correctness correctness =
    Recursive_mutex.with_mutex mutex
      (fun () ->
        is_correct := correctness)
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
  let wellFormedLabel x = not (StrExtra.Bool.match_string ".*[><].*" x) in

  let check_name  x =
  	if not (StrExtra.wellFormedName  x)
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
 object
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

  method get_port_defect_by_index
   (port_index:int)
   (port_direction:Defects_interface.port_direction)
   (column_header:string) : float
   =
    network#defects#get_port_attribute_of
      ~device_name:((parent#get_name):string)
      ~port_prefix
      ~port_index
      ~user_port_offset
      ~port_direction
      ~column_header
      ()

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
   ~(nodekind:nodekind)
   ~(devkind:devkind)
   ~port_no
   ~port_prefix
   ?(user_port_offset=0)
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
     ports_card <- Some (make_ports_card ~parent:self ~port_no:new_port_no)

   method virtual destroy : unit
     
  (** 'Static' methods (in the sense of C++/Java). Polarity is used to decide the correct
      kind of Ethernet cable needed to connect a pair of devices: the cable should be
      crossover iff both endpoints have the same polarity: *)
   method virtual polarity : polarity

   val mutable devkind = devkind
   method set_devkind x = devkind <- x

  (** The kind of the node (Machine or Device). *)
  method nodekind = nodekind

  (** The kind of the device (if the node is a device). *)
  method devkind = devkind

  (** The method which give access to the object describing defects of this component. *)
  (* method defect = defect *)

  (** Node dot traduction *)

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
    let fontsize   = if self#nodekind=Machine then "" else "fontsize=8," in
    let nodeoptions = if nodeoptions = "" then "" else (nodeoptions^",") in
    begin
    self#name^" ["^fontsize^nodeoptions^"shape=plaintext,label=<
<TABLE BORDER=\"0\" CELLBORDER=\"0\" CELLSPACING=\"0\" CELLPADDING=\"0\">
  <TR><TD>"^self#name^"</TD></TR>
  <TR><TD PORT=\"img\"><IMG SRC=\""^(self#dotImg z)^"\"></IMG></TD></TR>
"^label_line^"
</TABLE>>];"
    end

  (** Could be redefined. *)
  method label_for_dot = self#get_label

  (** make_simulated_device is defined in subclasses, not here  *)

  (* TODO: move it in the network class
     Return the list of cables of which a port of self is an endpoint: *)
  method private get_involved_cables =
    List.filter (fun c->c#is_node_involved self#get_name) network#cables

end;;

(* Justa an alias: *)
class type virtual node = node_with_ports_card

(** Essentially a pair:  (node, internal_port_index) *)
(** TODO: remove it and use the class port! *)
class endpoint ~(node:node) ~(port_index:int) =
 object
  method node = node
  method port_index = port_index

  method user_port_name =
    node#ports_card#user_port_name_of_internal_index port_index

  method user_port_index =
    node#ports_card#user_port_index_of_internal_index port_index

  (* Just a type conversion, as a pair: *)
  method involved_node_and_port_index = (node, port_index)

 end;;
 
(* *************************** *
        class cable
 * *************************** *)

(** A cable defines an edge in the network graph.
    Defects may be added after creation. *)
class cable =
   fun
   ~network
   ~motherboard
   ~name
   ?label
   ~cablekind
   ~(left : endpoint)
   ~(right: endpoint)
   () ->
  let dotoptions = new Dotoptions.cable ~motherboard ~name () in
  object (self)
  inherit OoExtra.destroy_methods ()
  inherit component ~network ~name ?label ()
  inherit [cable] simulated_device () as self_as_simulated_device

  (* Redefinition: *)
  method destroy =
    self#destroy_my_simulated_device;
    self#dotoptions#destroy ()

  val cablekind = cablekind
  method cablekind = cablekind

  method dotoptions = dotoptions

  (** A cable has two connected endpoints: *)
  val mutable left  : endpoint = left
  val mutable right : endpoint = right

  (** Accessors *)
  method get_left  = left
  method get_right = right
  method set_left x = left  <- x
  method set_right x = right <- x


  (** The li st of two names of nodes (machine/device) linked by the cable *)
  method involved_node_names = [left#node#name; right#node#name]

  (** Is a node connected to something with this cable? *)
  method is_node_involved node_name =
    List.mem node_name self#involved_node_names

  (** Return the list of devices (i.e. hubs, switches or routers) directly linked to this cable: *)
  method involved_node_and_port_index_list =
    [ (left#node, left#port_index); (right#node, right#port_index) ]

  (** Show its definition. Useful for debugging. *)
  method show prefix =
    (prefix^self#name^" ("^(string_of_cablekind self#cablekind)^")"^
    " ["^left#node#name^","^left#user_port_name^"] -> "^
    " ["^right#node#name^","^right#user_port_name^"]")

  method to_forest =
    Forest.leaf ("cable",
		    [ ("name"            ,  self#get_name )  ;
		      ("label"           ,  self#get_label)  ;
		      ("kind"            , (string_of_cablekind self#cablekind)) ;
		      ("leftnodename"    ,  self#get_left#node#name)    ;
		      ("leftreceptname"  ,  self#get_left#user_port_name)  ;
		      ("rightnodename"   ,  self#get_right#node#name)   ;
		      ("rightreceptname" ,  self#get_right#user_port_name) ;
		    ])

  (** A cable has just attributes (no childs) in this version. The attribute "kind" cannot be set,
      must be considered as a constant field of the class. *)
  method eval_forest_attribute =
    function
      | ("name"            , x ) -> self#set_name  x
      | ("label"           , x ) -> self#set_label x
      | ("kind"            , x ) -> () (* Constant field: cannot be set. *)
      | _ -> assert false

    (** A cable may be either connected or disconnected; it's connected by default: *)
    val connected = ref true

    (** Access method *)
    method is_connected = !connected

    (** Make the cable connected, or do nothing if it's already connected: *)
    method private connect_right_now =
      Recursive_mutex.with_mutex mutex
        (fun () ->
          (if not self#is_connected then begin
            Log.printf "Connecting the cable %s...\n" self#get_name;
            (* Turn on the relevant LEDgrid lights: *)
            let involved_node_and_port_index_list = self#involved_node_and_port_index_list in
            List.iter
              (fun (device, port) ->
                network#ledgrid_manager#set_port_connection_state
                  ~id:(device#id)
                  ~port
                  ~value:true
                  ())
              involved_node_and_port_index_list;
            connected := true;
            self#increment_alive_endpoint_no;
            Log.printf "Ok: connected\n";
          end);
          refresh_sketch ());

    (** Make the cable disconnected, or do nothing if it's already disconnected: *)
    method private disconnect_right_now =
      Recursive_mutex.with_mutex mutex
        (fun () ->
          (if self#is_connected then begin
            Log.printf "Disconnecting the cable %s...\n" self#get_name;
            (* Turn off the relevant LEDgrid lights: *)
            let involved_node_and_port_index_list = self#involved_node_and_port_index_list in
            List.iter
              (fun (device, port) ->
                network#ledgrid_manager#set_port_connection_state
                  ~id:(device#id)
                  ~port
                  ~value:false
                  ())
              involved_node_and_port_index_list;
            connected := false;
            self#decrement_alive_endpoint_no;
            Log.printf "Ok: disconnected\n";
          end);
          refresh_sketch ());

   (** 'Suspending means disconnecting for cables *)
   method suspend_right_now =
     self#disconnect_right_now

   (** 'Resuming' means connecting for cables *)
   method resume_right_now =
     self#connect_right_now

   (** An always up-to-date 'reference counter' storing the number of alive
       endpoints plus the cable connection state (either 0 for 'disconnected' or
       1 for 'connected'). A cable can be started in the simulation when this is
       exactly 3, and must be terminated when it becomes less than 3. *)
   val alive_endpoint_no = ref 1 (* cables are 'connected' by default *)

   (** Check that the reference counter is in [0, 3]. To do: disable this for
   production. *)
   method private check_alive_endpoint_no =
    Recursive_mutex.with_mutex mutex
      (fun () ->
        assert((!alive_endpoint_no >= 0) && (!alive_endpoint_no <= 3));
        Log.printf "The reference count is now %d\n" !alive_endpoint_no;
       )

   (** Record the fact that an endpoint has been created (at a lower level
       this means that its relevant {e hublet} has been created), and
       startup the simulated cable if appropriate. *)
   method increment_alive_endpoint_no =
     Recursive_mutex.with_mutex mutex
       (fun () ->
         Log.printf "Increment_alive_endpoint_no\n";
         self#check_alive_endpoint_no;
         alive_endpoint_no := !alive_endpoint_no + 1;
         self#check_alive_endpoint_no;
         if !alive_endpoint_no = 3 then begin
           Log.printf "The reference count raised to three: starting up a cable\n";
           self#startup_right_now
         end)

   (** Record the fact that an endpoint is no longer running (at a lower level
       this means that its relevant {e hublet} has been destroyed), and
       shutdown the simulated cable if appropriate. *)
   method decrement_alive_endpoint_no =
     Recursive_mutex.with_mutex mutex
       (fun () ->
         Log.printf "Decrement_alive_endpoint_no\n";
         self#check_alive_endpoint_no;
         alive_endpoint_no := !alive_endpoint_no - 1;
         self#check_alive_endpoint_no;
         if !alive_endpoint_no < 3 then begin
           (* Note that we destroy rather than terminating. This enables to re-create the
              simulated device later, at startup time, referring the correct hublets
              that will exist then, rather than the ones existing now *)
           Log.printf "The reference count dropped below three: destroying a cable\n";
           self#destroy_right_now;
         end)

   (** Make a new simulated device according to the current status *)
   method private make_simulated_device =
     Recursive_mutex.with_mutex mutex
       (fun () ->
         let left_hublet_process = 
           left#node#get_hublet_process_of_port left#port_index
         in
         let right_hublet_process =
           right#node#get_hublet_process_of_port right#port_index
         in
         let left_blink_command =
           match left#node#devkind with
             NotADevice -> None
           | _ -> Some (Printf.sprintf "(id: %i; port: %i)" left#node#id left#port_index) in
         let right_blink_command =
           match right#node#devkind with
             NotADevice -> None
           | _ -> Some (Printf.sprintf "(id: %i; port: %i)" right#node#id right#port_index) in
         Log.printf "Left hublet process socket name is \"%s\"\n" left_hublet_process#get_socket_name;
         Log.printf "Right hublet process socket name is \"%s\"\n" right_hublet_process#get_socket_name;
         new Simulated_network.ethernet_cable
           ~parent:self
           ~left_end:left_hublet_process
           ~right_end:right_hublet_process
           ~blinker_thread_socket_file_name:(Some network#ledgrid_manager#blinker_thread_socket_file_name)
           ~left_blink_command
           ~right_blink_command
           ~unexpected_death_callback:self#destroy_because_of_unexpected_death
           ())

   (** This has to be overridden for cables, because we can't 'poweroff' as easily as the
       other devices: *)
   method private destroy_because_of_unexpected_death () =
     Recursive_mutex.with_mutex mutex
       (fun () ->
         (* Refresh the process in some (ugly) way: *)
         if self#is_connected then begin
           (try self#disconnect_right_now with _ -> ());
           (try self#connect_right_now with _ -> ());
           connected := true;
         end else begin
           let current_alive_endpoint_no = !alive_endpoint_no in
           self_as_simulated_device#destroy_because_of_unexpected_death ();
           connected := true;
           alive_endpoint_no := 0;
           for i = 1 to current_alive_endpoint_no do
             self#increment_alive_endpoint_no;
           done
         end)

   (** To do: remove this ugly kludge, and make cables stoppable *)
   method can_startup = true (* To do: try reverting this *)
   method can_gracefully_shutdown = true (* To do: try reverting this *)
   method can_poweroff = true (* To do: try reverting this *)
   (** Only connected cables can be 'suspended' *)
   method can_suspend =
     Recursive_mutex.with_mutex mutex
       (fun () -> !connected)
   (** Only non-connected cables with refcount exactly equal to 2 can be 'resumed' *)
   method can_resume =
     Recursive_mutex.with_mutex mutex
       (fun () -> not !connected)

   (** Get the reference count right at the beginning: it starts at zero, but
       it's immediately incremented if endpoint hublet processes already
       exist: *)
   initializer
     self#set_correctness
       (network#would_a_cable_be_correct_between
          left#node#name
          right#node#name
          cablekind);
     (if left#node#has_hublet_processes then
       self#increment_alive_endpoint_no);
     (if right#node#has_hublet_processes then
       self#increment_alive_endpoint_no);
     Log.printf "The reference count for the just-created cable %s is %d\n" self#get_name !alive_endpoint_no;
end;;


(* *************************** *
        class device
 * *************************** *)

class virtual device_with_defects ~network () =
 object (self)

  method virtual defects_device_type : string
  method virtual get_name : string
  method virtual get_port_no : int
  method virtual port_prefix : string
  method virtual user_port_offset : int
  
  method private add_my_defects =
   match
     (network#defects:Defects_interface.defects_interface)#row_exists_with_binding
        "Name"
        self#get_name
   with
   | true ->
       Log.printf "The %s %s has already defects defined...\n"
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
    Log.printf "component \"%s\": destroying my defects.\n" self#get_name;
    network#defects#remove_device self#get_name;

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

(** Common class for hubs, switches and world_gateways
   (routers have a more specialized class): *)
class virtual device_with_ledgrid_and_defects
  ~network
  ~name
  ?(label="")
  ~devkind
  ~port_no
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
    ~nodekind:Device
    ~devkind
    ~port_no
    ~port_prefix
    ?user_port_offset
    ()
  as self_as_node_with_ports_card

  initializer
    (* TODO: the following line must be moved the a node initializer: *)
    network#add_device_new (self :> device_with_ledgrid_and_defects);
    self#add_destroy_callback (lazy (network#del_device_new self#get_name));
    (* this is correct here: *)
    self#add_my_ledgrid;
    self#add_destroy_callback (lazy self#destroy_my_ledgrid);

  inherit device_with_defects ~network:network_alias () as self_as_device_with_defects

  (** Dot adjustments *)

  (** Returns an image representig the device with the given iconsize. *)
  method virtual dotImg : iconsize -> string

  (** Returns the label to use for cable representation.
      For devices, the port X is represented by the string "[X]". *)
  method dotLabelForEdges (receptname:string) =
    let user_index = self#ports_card#user_port_index_of_user_port_name receptname in
    ("["^string_of_int user_index^"]")

  (** Return the string representing the port in cable representation. *
      Ignore the receptname and returns the empty string. *)
  method dotPortForEdges (receptname:string) = ""

  method to_forest = (* TODO remove it, is obsolete *)
   Forest.leaf ("device",[
  		  ("name" , self#get_name) ;
                  ("label", self#get_label);
                  ("kind" , (string_of_devkind self#devkind)) ;
                  ("eth"  , (string_of_int (self#get_port_no)));
                  ])

  (** A cable has just attributes (no childs) in this version. *)
  method eval_forest_attribute = function
  | ("name"  , x ) -> self#set_name  x
  | ("label" , x ) -> self#set_label x
  | ("kind"  , x ) -> self#set_devkind (devkind_of_string x)
  | ("eth"   , x ) -> self#set_port_no (int_of_string x)
  | _ -> assert false

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
   let leds_relative_subdir = string_of_devkind self#devkind in
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
    Log.printf "component \"%s\": destroying my ledgrid.\n" self#get_name;
    (network#ledgrid_manager:Ledgrid_manager.ledgrid_manager)#destroy_device_ledgrid
      ~id:(self#id)
      ()

  (* REDEFINED: *)
  method set_name new_name =
    let old_name = self#get_name in
    if old_name <> new_name then begin
      network#defects#rename_device old_name new_name;
      self_as_node_with_ports_card#set_name new_name;
    end;
 
  (* REDEFINED: *)
  method set_port_no new_port_no =
    let old_port_no = self#get_port_no in
    if new_port_no <> old_port_no then begin
      self_as_device_with_defects#defects_update_port_no new_port_no;
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

class virtual virtual_machine_with_history_and_details
  ~network
  ?epithet   (* Ex: "debian-lenny-42178" *)
  ?variant
  ?kernel    (* Also en epithet, ex: "2.6.18-ghost" *)
  ?terminal
  ~(history_icon:string)
  ~(details_device_type:string)
  ?(details_port_row_completions:Network_details_interface.port_row_completions option)
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
    self#add_my_details ?port_row_completions:details_port_row_completions self#get_port_no;
    self#add_destroy_callback (lazy self#destroy_my_details);
    self#add_my_history;
    self#add_destroy_callback (lazy self#destroy_my_history);

  (* Paramters *)
  method history_icon = history_icon
  method details_device_type = details_device_type

  method private banner =
    (Printf.sprintf "Mariokit.virtual_machine: setting %s: " self#get_name)

  method sprintf : 'a. ('a, unit, string, string) format4 -> 'a =
    Printf.ksprintf (fun x->self#banner^x)

  method failwith : 'a 'b. ('a, unit, string, string) format4 -> 'b =
    Obj.magic (Printf.ksprintf (fun x->failwith (self#banner^x)))
  
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
   | false -> self#failwith "the variant %s is not available" x

 method get_variant_realpath : string option =
   Option.map (vm_installations#variants_of self#get_epithet)#realpath_of_epithet self#get_variant

  (** A machine has an associated linux kernel, expressed by en epithet: *)
  val mutable kernel : string = kernel
  initializer ignore (self#check_kernel kernel)
  method get_kernel   = kernel
  method set_kernel x = kernel <- self#check_kernel x
  method private check_kernel x =
    match (vm_installations#kernels#epithet_exists kernel) with
    | true -> x
    | false -> self#failwith "unknown kernel %s" x

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
      vm_installations#filesystems#realpath_of_epithet self#get_epithet
   
  method get_kernel_file_name =
      vm_installations#kernels#realpath_of_epithet self#get_kernel
  
  method is_xnest_enabled =
      (vm_installations#terminal_manager_of self#get_epithet)#is_xnest self#get_terminal

  (* Used only to add a filesystem history device: *)
  method private prefixed_epithet = (vm_installations#prefix ^ self#get_epithet)

  method add_my_history =
   let icon = self#history_icon in
   let name = self#get_name in
   match ((network#history:Filesystem_history.states_interface)#number_of_states_with_name name) > 0 with
   | true -> Log.printf "The virtual machine %s has already history defined...\n" name
   | false ->
      Filesystem_history.add_device
          ~name
          ~prefixed_filesystem:self#prefixed_epithet
          ?variant:self#get_variant
          ?variant_realpath:self#get_variant_realpath
          ~icon
          ()
  
  method add_my_details
    ?(port_row_completions:Network_details_interface.port_row_completions option)
    (port_no:int) : unit
   =
   match
     (network#details:Network_details_interface.network_details_interface)#row_exists_with_binding
        "Name"
        self#get_name
   with
   | true  -> Log.printf "The %s %s has already details defined...\n" self#details_device_type self#get_name
   | false ->
      begin
      network#details#add_device
        ?port_row_completions
        self#get_name
        details_device_type
        self#get_port_no
      end

  method destroy_my_details =
    Log.printf "component \"%s\": destroying my details.\n" self#get_name;
    network#details#remove_device self#get_name;

  method destroy_my_history =
    Log.printf "component \"%s\": destroying my history.\n" self#get_name;
    Filesystem_history.remove_device_tree self#get_name;

  method update_virtual_machine_with ~name ~port_no kernel =
    network#details#update_port_no self#get_name port_no;
    network#details#rename_device self#get_name name;
    Filesystem_history.rename_device self#get_name name;
    self#set_kernel kernel;

  method create_cow_file_name =
    let history = (network#history:Filesystem_history.states_interface) in
    Printf.sprintf "%s%s"
      history#get_states_directory
      (Filesystem_history.add_state_for_device self#get_name)

end;; (* class virtual_machine_with_history_and_details *)


(* *************************** *
        class machine
 * *************************** *)

(** A machine is a node s.t.
    nodekind=Machine. Some receptacles are immediatly added
    at creation time. *)
class machine
   ~network
   ?(name="nomachinename")
   ?(label="")
   ?(memory:int=48)
   ?(port_no:int=1)
   ?epithet   (* Ex: "debian-lenny-42178" *)
   ?variant
   ?kernel    (* Also en epithet, ex: "2.6.18-ghost" *)
   ?terminal
   ()
   =
  let vm_installations = Disk.get_machine_installations ()
  in
  let check_port_no ?(name=name) x =
    if x<1 or x>8 (* TODO: fix the limit with a MARIONNET variable *)
      then failwith "value not in the eth's number range [1,8]"
      else x
  in
  let network_alias = network in
  object (self)

  inherit OoExtra.destroy_methods ()

  inherit node_with_ports_card
    ~network
    ~name
    ~label
    ~nodekind:Machine
    ~devkind:NotADevice 
    ~port_no:(check_port_no port_no)
    ~port_prefix:"eth"
    ~user_port_offset:0
    ()
  as self_as_node_with_ports_card

  inherit virtual_machine_with_history_and_details
    ~network:network_alias
    ?epithet ?variant ?kernel ?terminal
    ~history_icon:"machine"
    ~details_device_type:"machine"
    ~vm_installations ()
    as self_as_virtual_machine_with_history_and_details

  (** See the comment in the 'node' class for the meaning of this method: *)
  method polarity = MDI

  (** Get the full host pathname to the directory containing the guest hostfs
      filesystem: *)
  method hostfs_directory_pathname =
    match !simulated_device with
      Some d ->
        (d :> node Simulated_network.machine)#hostfs_directory_pathname
    | None ->
        failwith (self#name ^ " is not being simulated right now")

  (** REDEFINED for checking *)
  method set_port_no x =
    let x = (self#check_port_no x) in
    self_as_node_with_ports_card#set_port_no x

  method private check_port_no x = check_port_no ~name:self#name x

  (** A machine will be started with a certain amount of memory *)
  (* TODO: read a marionnet variable to fix limits: *)
  val mutable memory : int = memory
  initializer ignore (self#check_memory memory)
  method get_memory = memory
  method set_memory x = memory <- self#check_memory x
  method private check_memory x =
    match (x>=8) && (x<=1024) with
    | true  -> x
    | false -> self#failwith "value %d not in the memory range [8,1024]" x

  (** Show for debugging *)
  method show = name

  (** Return an image representing the machine with the given iconsize. *)
  method dotImg (z:iconsize) =
    let imgDir = Initialization.Path.images in
    (imgDir^"ico.machine."^(self#string_of_simulated_device_state)^"."^z^".png") (* distinguer redhat,debian, etc ? *)

  (** Machine to forest encoding. *)
  method to_forest =
   Forest.leaf ("machine", [
                   ("name"     ,  self#get_name );
                   ("memory"   ,  (string_of_int self#get_memory));
                   ("distrib"  ,  self#get_epithet  );
                   ("variant"  ,  self#get_variant_as_string);
                   ("kernel"   ,  self#get_kernel   );
                   ("terminal" ,  self#get_terminal );
                   ("eth"      ,  (string_of_int self#get_port_no))  ;
	           ])

 (** A machine has just attributes (no childs) in this version. *)
 method eval_forest_attribute = function
  | ("name"     , x ) -> self#set_name x
  | ("memory"   , x ) -> self#set_memory (int_of_string x)
  | ("distrib"  , x ) -> self#set_epithet x
  | ("variant"  , "aucune" ) -> self#set_variant None (* backward-compatibility *)
  | ("variant"  , "" )-> self#set_variant None
  | ("variant"  , x ) -> self#set_variant (Some x)
  | ("kernel"   , x ) -> self#set_kernel x
  | ("terminal" , x ) -> self#set_terminal x
  | ("eth"      , x ) -> self#set_port_no  (int_of_string x)
  | _ -> () (* Forward-comp. *)


  (** Create the simulated device *)
  method private make_simulated_device =
    let id = self#id in
    let cow_file_name = self#create_cow_file_name in
    let () =
     Log.printf
       "About to start the machine %s\n  with filesystem: %s\n  cow file: %s\n  kernel: %s\n  xnest: %b\n"
       self#name
       self#get_filesystem_file_name
       cow_file_name
       self#get_kernel_file_name
       self#is_xnest_enabled
    in
    new Simulated_network.machine
      ~parent:self
      ~kernel_file_name:self#get_kernel_file_name
      ~filesystem_file_name:self#get_filesystem_file_name
      ~cow_file_name
      ~ethernet_interface_no:self#get_port_no
      ~memory:self#get_memory
      ~umid:self#get_name
      ~id
      ~xnest:self#is_xnest_enabled
      ~unexpected_death_callback:self#destroy_because_of_unexpected_death
      ()

  (** Here we also have to manage cow files... *)
  method private gracefully_shutdown_right_now =
    Log.printf "Calling hostfs_directory_pathname on %s...\n" self#name;
    let hostfs_directory_pathname = self#hostfs_directory_pathname in
    Log.printf "Ok, we're still alive\n"; flush_all ();
    (* Do as usual... *)
    self_as_node_with_ports_card#gracefully_shutdown_right_now;
    (* If we're in exam mode then make the report available in the texts treeview: *)
    (if Command_line.are_we_in_exam_mode then begin
      let texts_interface = Texts_interface.get_texts_interface () in
      Log.printf "Adding the report on %s to the texts interface\n" self#name;
      texts_interface#import_report
        ~machine_or_router_name:self#name
        ~pathname:(hostfs_directory_pathname ^ "/report.html")
        ();
      Log.printf "Added the report on %s to the texts interface\n" self#name; flush_all ();
      Log.printf "Adding the history on %s to the texts interface\n" self#name; flush_all ();
      texts_interface#import_history
        ~machine_or_router_name:self#name
        ~pathname:(hostfs_directory_pathname ^ "/bash_history.text")
        ();
      Log.printf "Added the history on %s to the texts interface\n" self#name; flush_all ();
    end);
    (* ...And destroy, so that the next time we have to re-create the process command line
       can use a new cow file (see the make_simulated_device method) *)
    self#destroy_right_now

  (** Here we also have to manage cow files... *)
  method private poweroff_right_now =
    (* Do as usual... *)
    self_as_node_with_ports_card#poweroff_right_now;
    (* ...And destroy, so that the next time we have to re-create the process command line
       can use a new cow file (see the make_simulated_device method) *)
    self#destroy_right_now

end;;


(* *************************** *
        class cloud
 * *************************** *)

(** A cloud is an unknown network with some entry points. *)
class cloud =
  fun ~network
      ?(name="nocloudname")
      ?(label="")
      () ->

  let port_prefix = "port" in
  let port_no = 2 in
  object (self) inherit OoExtra.destroy_methods ()

  inherit node_with_ports_card
    ~network
    ~name
    ~label
    ~nodekind:Cloud
    ~devkind:NotADevice
    ~port_no
    ~port_prefix
    ()
  as self_as_node_with_ports_card

  (* TODO: this is temporary and not correct: *)
  method destroy = self#destroy_my_simulated_device
 
  (** See the comment in the 'node' class for the meaning of this method: *)
  method polarity = Intelligent (* Because it is didactically meaningless *)

  method show = self#name

  (** Dot adjustments *)

  (** Return an image representing the cloud with the given iconsize. *)
  method dotImg (z:iconsize) =
    let imgDir = Initialization.Path.images in
    (imgDir^"ico.cloud."^(self#string_of_simulated_device_state)^"."^z^".png")

  (** Cloud endpoints are represented in the same way of devices ones, with "[X]". *)
  method dotLabelForEdges (receptname:string) =
    let user_index = self#ports_card#user_port_index_of_user_port_name receptname in
    ("["^string_of_int user_index^"]")

  method dotPortForEdges (receptname:string) = ""

  method to_forest =
   Forest.leaf ("cloud",[
  		 ("name",self#get_name);
  		 ])

 (** A cloud has just attributes (no childs) in this version. *)
 method eval_forest_attribute = function
  | ("name"     , x ) -> self#set_name x
  | _ -> assert false

  (** Create the simulated device *)
  method private make_simulated_device =
    new Simulated_network.cloud
      ~parent:self
      ~unexpected_death_callback:self#destroy_because_of_unexpected_death
(*       ~id *)
      ()
end;; (* cloud *)


(* *************************** *
        class world_bridge
 * *************************** *)

(** A world_bridge has a single receptacle and an associated IP number. *)
class world_bridge =

  fun ~network
      ?(name="noworld_bridgename")
      ?(label="")
      () ->

  object (self) inherit OoExtra.destroy_methods ()

  inherit node_with_ports_card
    ~network
    ~name
    ~label
    ~nodekind:World_bridge
    ~devkind:NotADevice 
    ~port_no:1
    ~port_prefix:"eth"
    ()
  as self_as_node_with_ports_card

  (* TODO: this is temporary and not correct: *)
  method destroy = self#destroy_my_simulated_device

  (** See the comment in the 'node' class for the meaning of this method: *)
  method polarity = Intelligent (* Because is not pedagogic anyway. *)

  method show = (self#name^" (world bridge)")

  (** Dot adjustments *)

  (** Return an image representing the machine with the given iconsize. *)
  method dotImg (z:iconsize) =
    let imgDir = Initialization.Path.images in
    (imgDir^"ico.world_bridge."^(self#string_of_simulated_device_state)^"."^z^".png")

  (** Returns the label to use for cable representation.*)
  method dotLabelForEdges receptname = "" (* ip#toString *)

  (** Returns the port to use for cable representation.
      Ignore the receptname and returns the empty string. *)
  method dotPortForEdges receptname = ""

  method to_forest =
   Forest.leaf ("world_bridge", [
                  ("name", self#get_name);
                  ])

  (** A world_bridge has just attributes (no childs) in this version. *)
  method eval_forest_attribute = function
  | ("name"     , x ) -> self#set_name x
  | _ -> assert false

  (** Create the simulated device *)
  method private make_simulated_device =
    new Simulated_network.world_bridge
      ~parent:self
      ~bridge_name:Global_options.ethernet_socket_bridge_name
      ~unexpected_death_callback:self#destroy_because_of_unexpected_death
      ()
end;; (* world_bridge *)




(*********************
    Submodule Edge
 *********************)

module Edge = struct

(** Type of an edge. An edge is a triple (node1,receptname1), cable , (node2,receptname2) *)
type edge = (node*string) * cable * (node*string)
;;

(** The Dot translation of an edge of the graph. *)
let dotEdgeTrad ?(edgeoptions="") (labeldistance_base) (((n1,r1),c,(n2,r2)):edge) =
    begin

    let vertexlab node iden recept =

      let port      = node#dotPortForEdges  recept in
      let portlabel = node#dotLabelForEdges recept in

      match port,portlabel with
      | "",""  -> ("")
      | _ , "" -> (","^iden^"=\""^port^"\"")
      | _ , _  ->
          begin
          let port_line      = (StringExtra.assemble "<TR><TD>" port "</TD></TR>") in
          let portlabel_line = (StringExtra.assemble "<TR><TD><FONT COLOR=\"#3a3936\">" portlabel "</FONT></TD></TR>") in
(","^iden^"=<
<TABLE BORDER=\"0\" CELLBORDER=\"0\" CELLSPACING=\"0\" CELLPADDING=\"0\">
"^port_line^"
"^portlabel_line^"
</TABLE>
>")       end in


   let labeldistance =
      begin
      let p1  = n1#dotPortForEdges  r1 in
      let pl1 = n1#dotLabelForEdges r1 in
      let p2  = n2#dotPortForEdges  r2 in
      let pl2 = n2#dotLabelForEdges r2 in

      (* if there is a vertex with both port and portlabel not empty => set labeldistance +0.5 *)
      if ((p1<>"" && pl1<>"") or (p2<>"" && pl2<>""))
      then ("labeldistance="^(string_of_float (labeldistance_base +. 0.5))^",") else ""
      end in


   let (tail, head, taillabel, headlabel) =

    (* Invert left and right sides of the cable if required *)
    let (n1,r1,n2,r2) = if c#dotoptions#reverted#get then (n2,r2,n1,r1) else (n1,r1,n2,r2) in

     match (n1#nodekind, c#cablekind, n2#nodekind, n1#get_label, n2#get_label) with

     |   _   ,    _   ,    _  , "", "" -> (n1#name^":img"), (n2#name^":img"), (vertexlab n1 "taillabel" r1), (vertexlab n2 "headlabel" r2)
     |   _   ,    _   ,    _  , "", l2 -> (n1#name^":img"), (n2#name)       , (vertexlab n1 "taillabel" r1), (vertexlab n2 "headlabel" r2)
     |   _   ,    _   ,    _  , l1, "" -> (n1#name)       , (n2#name^":img"), (vertexlab n1 "taillabel" r1), (vertexlab n2 "headlabel" r2)
     |   _   ,    _   ,    _  , l1, l2 -> (n1#name)       , (n2#name)       , (vertexlab n1 "taillabel" r1), (vertexlab n2 "headlabel" r2)

   in

   let edgeoptions = if edgeoptions = "" then "" else (edgeoptions^",") in
   let cable_label = c#get_name ^ (if c#get_label = "" then "" else ("  "^c#get_label)) in

   let edgeoptions = edgeoptions ^ "arrowhead=obox, arrowtail=obox, arrowsize=0.4," ^
    (if c#is_connected then ""
   		       else "style=dashed,") in

   let label_color = if c#cablekind = Direct then Color.direct_cable else Color.crossover_cable in

    (tail^" -> "^head^" ["^edgeoptions^labeldistance^"label=<<FONT COLOR=\""^label_color^"\">"^cable_label^"</FONT>>"^taillabel^headlabel^"];")

 end;; (* function dotEdgeTrad *)


end;; (* Module Edge *)


open Edge;;


 module Eval_forest_child = struct

 let try_to_add_machine network (f:Xforest.tree) =
  try
   (match f with
   | Forest.NonEmpty (("machine", attrs) , childs , Forest.Empty) ->
        let x = new machine ~network () in
        x#from_forest ("machine", attrs) childs ;
        network#add_machine x;
        true
   | _ -> false
   )
  with _ -> false

 let try_to_add_cloud network (f:Xforest.tree) =
  try
   (match f with
    | Forest.NonEmpty (("cloud", attrs) , childs , Forest.Empty) ->
	let x = new cloud ~network () in
        x#from_forest ("cloud", attrs) childs ;
	network#add_cloud x;
        true
   | _ ->
        false
   )
  with _ -> false

 let try_to_add_world_bridge network (f:Xforest.tree) =
  try
   (match f with
    | Forest.NonEmpty (("world_bridge", attrs) , childs , Forest.Empty)
    | Forest.NonEmpty (("gateway" (* retro-compatibility *) , attrs) , childs , Forest.Empty) ->
        let x = new world_bridge ~network () in
	x#from_forest ("world_bridge", attrs) childs  ;
	network#add_world_bridge x;
        true
   | _ ->
        false
   )
  with _ -> false

 let try_to_add_cable network (f:Xforest.tree) =
  try
   (match f with
   | Forest.NonEmpty (("cable", attrs) , childs , Forest.Empty) ->
	(* Cables represent a special case: they must be builded knowing their endpoints. *)
	let name = List.assoc "name"    attrs in
	let ln = List.assoc "leftnodename"    attrs in
	let lr = List.assoc "leftreceptname"  attrs in
	let rn = List.assoc "rightnodename"   attrs in
	let rr = List.assoc "rightreceptname" attrs in
	let ck = List.assoc "kind"            attrs in
	let left  = network#make_endpoint_of ~node_name:ln ~user_port_name:lr in
	let right = network#make_endpoint_of ~node_name:rn ~user_port_name:rr in
	let cablekind = cablekind_of_string ck      in
	let x = new cable ~motherboard:network#motherboard ~name ~cablekind ~network ~left ~right () in
	x#from_forest ("cable", attrs) childs ;
	network#add_cable x;
        true
   | _ ->
        false
   )
  with _ -> false

 end (* module Eval_forest_child *)

(* *************************** *
        class network
 * *************************** *)

(** Class modelling the virtual network *)
class network () =
 let ledgrid_manager = Ledgrid_manager.the_one_and_only_ledgrid_manager in
 object (self)
 inherit Xforest.interpreter ()

 method defects = Defects_interface.get_defects_interface ()
 method details = Network_details_interface.get_network_details_interface ()
 method history = Filesystem_history.get_states_interface ()

 (** Motherboard is set in Gui_motherboard. *)
 val mutable motherboard : State_types.motherboard option = None
 method motherboard = match motherboard with Some x -> x | None -> assert false
 method set_motherboard m = motherboard <- Some m

 val mutable machines : (machine list) = []
 val mutable devices  : (device_with_ledgrid_and_defects  list) = []
 val mutable cables   : (cable   list) = []
 val mutable clouds   : (cloud   list) = []
 val mutable world_bridges : (world_bridge list) = []


 (** Buffers to backup/restore data. *)
 val mutable machines_buffer : (machine list) = []
 val mutable devices_buffer  : (device_with_ledgrid_and_defects  list) = []
 val mutable cables_buffer   : (cable   list) = []
 val mutable clouds_buffer   : (cloud   list) = []
 val mutable world_bridges_buffer : (world_bridge list) = []

 (** Accessors *)

 method machines        = machines
 method devices         = devices
 method cables          = cables
 method clouds          = clouds
 method world_bridges  = world_bridges
 method ledgrid_manager = ledgrid_manager

 (** Related dot options fro drawing this virtual network.
     This pointer is shared with the project instance. *)
 val mutable dotoptions : (Dotoptions.network option) = None
 method      dotoptions   = match dotoptions with Some x -> x | None -> raise (Failure "network#dotoptions")
 method  set_dotoptions x = dotoptions <- Some x

 method components : (component list) =
   ((machines :> component list) @
    (devices  :> component list) @
    (clouds   :> component list) @
    (world_bridges :> component list) @
    (cables   :> component list) (* CABLES MUST BE AT THE FINAL POSITION for marshaling !!!! *)
    )

 (** Setter *)

 (* The optional parameter [scheduled=true] means that this method is called
    in a task managed by the Task_runner. In this case, we have not to call
    the task runner method [wait_for_all_currently_scheduled_tasks]. *)
 method reset ?(scheduled=false) () =
   Log.print_string "---\n";
   Log.printf "network#reset: begin\n";
   Log.printf "\tDestroying all cables...\n";
   (List.iter
      (fun cable -> try cable#destroy with _ -> ())
      cables);
   Log.printf "\tDestroying all machines...\n";
   (List.iter
      (fun machine -> try machine#destroy with _ -> ()) 
      machines);
   Log.printf "\tDestroying all devices (switchs, hubs, routers, etc)...\n";
   (List.iter
      (fun device -> try device#destroy with _ -> ()) 
      devices);
   Log.printf "\tDestroying all clouds...\n";
   (List.iter
      (fun cloud -> try cloud#destroy with _ -> ())
      clouds);
   Log.printf "\tDestroying all world bridges...\n";
   (List.iter
      (fun world_bridge -> try world_bridge#destroy with _ -> ())
      world_bridges);
   Log.printf "\tSynchronously wait that everything terminates...\n";
   (if not scheduled then Task_runner.the_task_runner#wait_for_all_currently_scheduled_tasks);

   Log.printf "\tMaking the network graph empty...\n";
   machines <- [] ;
   devices  <- [] ;
   clouds   <- [] ;
   world_bridges <- [] ;
   cables   <- [] ;

   Log.printf "\tWait for all devices to terminate...\n";
   (** Make sure that all devices have actually been terminated before going
       on: we don't want them to lose filesystem access: *)
   Log.printf "\tAll devices did terminate.\n";
   Log.printf "network#reset: end (success)\n";
   Log.print_string "---\n";

 method destroy_process_before_quitting () =
  begin
   Log.printf "destroy_process_before_quitting: BEGIN\n";
   (List.iter (fun cable -> try cable#destroy_right_now with _ -> ()) cables);
   (List.iter (fun machine -> try machine#destroy_right_now with _ -> ()) machines);
   (List.iter (fun device -> try device#destroy_right_now with _ -> ()) devices);
   (List.iter (fun cloud -> try cloud#destroy_right_now with _ -> ()) clouds);
   (List.iter (fun world_bridge -> try world_bridge#destroy_right_now with _ -> ()) world_bridges);
   Log.printf "destroy_process_before_quitting: END (success)\n";
  end

 method restore_from_buffers =
  begin
   self#reset ();
   machines <- machines_buffer ;
   devices  <- devices_buffer  ;
   clouds   <- clouds_buffer   ;
   world_bridges <- world_bridges_buffer ;
   cables   <- cables_buffer
 end

 method save_to_buffers =
  begin
   machines_buffer <- machines ;
   devices_buffer  <- devices  ;
   clouds_buffer   <- clouds   ;
   world_bridges_buffer <- world_bridges ;
   cables_buffer   <- cables
  end

 method to_forest =
   let l = List.map (fun x->x#to_forest) self#components in
   Forest.tree ("network",[]) (Forest.of_treelist l)

 val try_to_add_procedure_list= ref []
 method subscribe_a_try_to_add_procedure p =
   try_to_add_procedure_list := p::(!try_to_add_procedure_list)

 initializer
   self#subscribe_a_try_to_add_procedure Eval_forest_child.try_to_add_machine;
(*    self#subscribe_a_try_to_add_procedure Eval_forest_child.try_to_add_router; *)
   self#subscribe_a_try_to_add_procedure Eval_forest_child.try_to_add_cloud;
(*    self#subscribe_a_try_to_add_procedure Eval_forest_child.try_to_add_hub; *)
(*    self#subscribe_a_try_to_add_procedure Eval_forest_child.try_to_add_switch; *)
   self#subscribe_a_try_to_add_procedure Eval_forest_child.try_to_add_world_bridge;
(*    self#subscribe_a_try_to_add_procedure Eval_forest_child.try_to_add_world_gateway; *)
   self#subscribe_a_try_to_add_procedure Eval_forest_child.try_to_add_cable;
   
 (** We redefine just the interpretation of a childs.
     We ignore (in this version) network attributes. *)
 method eval_forest_child (f:Xforest.tree) : unit =
  let xs = List.rev !try_to_add_procedure_list in
  let result = List.exists (fun p -> p self f) xs in
  match result with
  | true -> ()
  | false ->
    (match f with
    | Forest.NonEmpty ((nodename, attrs) , _ , _)
     -> let name  = List.assoc "name" attrs in
        (Log.printf "network#eval_forest_child: I can't interpret this \"%s\" name \"%s\".\n" nodename name)
        (* Forward-compatibility *)

    | Forest.Empty
     -> (Log.printf "network#eval_forest_child: I can't interpret the empty forest.\n")
        (* Forward-compatibility *)
    )


 method nodes : (node list) =
  List.concat [
    (machines :> node list);
    (devices  :> node list);
    (clouds   :> node list);
    (world_bridges :> node list);
    ]

 method names = (List.map (fun x->x#name) self#components)

 method suggestedName prefix =
   let rec tip prefix k =
     begin
     let prop = (prefix^(string_of_int k)) in
     if self#name_exists prop then tip prefix (k+1) else prop
     end in tip prefix 1


 (** Get machine, device or cable in the network by its name *)
 method get_machine_by_name n =
   try List.find (fun x->x#name=n) machines with _ -> failwith ("get_machine_by_name "^n)

 method get_device_by_name n =
   try List.find (fun x->x#name=n) devices with _ -> failwith ("get_device_by_name "^n)

 method get_cable_by_name n =
   try List.find (fun x->x#name=n) cables with _ -> failwith ("get_cable_by_name "^n)

 method get_cloud_by_name n =
   try List.find (fun x->x#name=n) clouds with _ -> failwith ("get_cloud_by_name "^n)

 method get_world_bridge_by_name n =
   try List.find (fun x->x#name=n) world_bridges with _ -> failwith ("get_world_bridge_by_name "^n)

 method get_node_by_name n =
   try List.find (fun x->x#name=n) self#nodes with _ -> failwith ("get_node_by_name \""^n^"\"")

 (** Managing endpoints: *)

 method make_endpoint_of ~node_name ~user_port_name =
  let node = List.find (fun n->n#name = node_name) self#nodes in
  let port_index = node#ports_card#internal_index_of_user_port_name user_port_name in
  (new endpoint ~node ~port_index)

 method involved_node_and_port_index_list =
   List.flatten (List.map (fun c->c#involved_node_and_port_index_list) cables)

 method busy_port_indexes_of_node (node:node) =
   let node_name = node#get_name in
   let related_busy_pairs =
     List.filter
       (fun (node, port_index) -> node#get_name = node_name)
        self#involved_node_and_port_index_list
   in
   List.map snd related_busy_pairs

 method free_port_indexes_of_node (node:node) =
   let node_port_indexes = ListExtra.range 0 (node#get_port_no-1) in
   ListExtra.substract node_port_indexes (self#busy_port_indexes_of_node node)

 method free_user_port_names_of_node node =
   List.map (node#ports_card#user_port_name_of_internal_index) (self#free_port_indexes_of_node node)

 method are_endpoints_free endpoints =
   List.for_all (self#is_endpoint_free) endpoints

 method is_endpoint_free endpoint =
   let busy_pairs = self#involved_node_and_port_index_list in
   List.iter (function (n,p) -> Log.printf "Involved: (%s,%d)\n" n#get_name p) busy_pairs; 
   not (List.mem (endpoint#involved_node_and_port_index) busy_pairs)

 (* The total number of endpoints in the network: *)
 method private endpoint_no =
   let sum xs = List.fold_left (+) 0 xs in
   sum (List.map (fun node -> node#get_port_no) self#nodes)
   
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
  let min_eth = (self#max_busy_port_index_of_node node + 1) in
  let min_multiple_of_4 = (ceil ((float_of_int min_eth) /. 4.0)) *. 4.0 in
  int_of_float (max min_multiple_of_4 4.0)

 method machine_exists n = let f=(fun x->x#name=n) in (List.exists f machines)
 method device_exists  n = let f=(fun x->x#name=n) in (List.exists f devices )
 method cable_exists   n = let f=(fun x->x#name=n) in (List.exists f cables  )
 method cloud_exists   n = let f=(fun x->x#name=n) in (List.exists f clouds  )
 method world_bridge_exists n = let f=(fun x->x#name=n) in (List.exists f world_bridges)
 method name_exists    n = List.mem n self#names

 (** What kind of node is it ? *)
 method nodeKind n = (self#get_node_by_name n)#nodekind

 (** Adding components *)

 (** Devices must have a unique name in the network *)
 method add_device_new (d:device_with_ledgrid_and_defects) =
    if (self#name_exists d#name) then
      raise (Failure "add_device: name already used in the network")
    else begin
      devices  <- (devices@[d]);
    end

 (** Remove a device from the network. Remove it from the [devices] list and remove all related cables *)
 method del_device_new dname =
     let d  = self#get_device_by_name dname in
     (* Destroy cables first: they refer what we're removing... *)
     let cables_to_remove = List.filter (fun c->c#is_node_involved dname) cables in
     List.iter 
       (fun cable -> (* TODO: this must became simply cable#destroy or (better) cable#update *)
         self#defects#remove_cable cable#name;
         self#del_cable cable#name)
       cables_to_remove;
     devices  <- List.filter (fun x->not (x=d)) devices

 (** Machines must have a unique name in the network *)
 method add_machine (m:machine) =
   if (self#name_exists m#name) then
     raise (Failure ("add_machine: name "^m#name^" already used in the network"))
   else begin
     (* Ok, we have fixed the variant name. Now just prepend the machine to the
        appropriate list: *)
     machines  <- (machines@[m]);
   end

 (** Cable must connect free ports: *)
 method add_cable (c:cable) =
    if (self#name_exists c#name)
    then raise (Failure "add_cable: name already used in the network")
    else
      let (left, right) = (c#get_left, c#get_right) in
      if (self#are_endpoints_free [left; right])
      then begin
        cables  <- (cables@[c]);
        (* If at least one endpoint is a device then set the port state to connected in
           the appropriate LED grid: *)
        if left#node#devkind != NotADevice then
          self#ledgrid_manager#set_port_connection_state
            ~id:(left#node#id)
            ~port:left#port_index
            ~value:true
            ();
        if right#node#devkind != NotADevice then
          self#ledgrid_manager#set_port_connection_state
            ~id:(right#node#id)
            ~port:right#port_index
            ~value:true
            ();
      end else
        raise (Failure ("add_cable: left and/or right endpoints busy or nonexistent for "^(c#show "")))

 method add_cloud (c:cloud) =
    if (self#name_exists c#name)
          then raise (Failure ("add_cloud: name "^c#name^" already used in the network"))
          else clouds  <- (clouds@[c])

 method add_world_bridge (b:world_bridge) =
    if (self#name_exists b#name)
          then raise (Failure ("add_world_bridge: name "^b#name^" already used in the network"))
          else world_bridges  <- (world_bridges@[b])


 (** Removing components *)

 (** Remove a machine from the network.
     Remove it from the [machines] list and remove all related cables *)
 method del_machine mname =
     let m  = self#get_machine_by_name mname in
     (* Destroy cables first, from the network and from the defects treeview (cables are not
        in the network details treeview): they refer what we're removing... *)
     let cables_to_remove = List.filter (fun c->c#is_node_involved mname) cables in
     let defects = Defects_interface.get_defects_interface () in
     List.iter
       (fun cable ->
         defects#remove_cable cable#name;
         self#del_cable cable#name)
       cables_to_remove;
     Filesystem_history.remove_device_tree mname;
     m#destroy;
     machines  <- List.filter (fun x->not (x=m)) machines

 (** Remove a device from the network. Remove it from the [devices] list and remove all related cables *)
 method del_device dname =
     let d  = self#get_device_by_name dname in
     (* Destroy cables first: they refer what we're removing... *)
     let cables_to_remove = List.filter (fun c->c#is_node_involved dname) cables in
     let defects = Defects_interface.get_defects_interface () in
     List.iter
       (fun cable ->
         defects#remove_cable cable#name;
         self#del_cable cable#name)
       cables_to_remove;
     (if d#devkind = Router then
       Filesystem_history.remove_device_tree dname);
     self#ledgrid_manager#destroy_device_ledgrid ~id:(d#id) ();
     d#destroy;
     devices  <- List.filter (fun x->not (x=d)) devices

 (** Remove a cable from network *)
 method del_cable cname =
     let c = self#get_cable_by_name cname in
     (* If at least one endpoint is a device then set the port state to disconnected in
        the appropriate LED grid: *)
     let (left, right) = (c#get_left, c#get_right) in
     (if left#node#devkind != NotADevice then
       self#ledgrid_manager#set_port_connection_state
         ~id:(left#node#id)
         ~port:left#port_index
         ~value:false
         ());
     (if right#node#devkind != NotADevice then
       self#ledgrid_manager#set_port_connection_state
         ~id:(right#node#id)
         ~port:right#port_index
         ~value:false
         ());
     cables <- (List.filter (fun x->not (x=c)) cables);
     c#destroy;

 (** Remove a cloud from the network. Remove it from the [clouds] list and remove all related cables *)
 method del_cloud clname =
     let cl  = self#get_cloud_by_name clname in
     (* Destroy cables first: they refer what we're removing... *)
     let cables_to_remove = List.filter (fun c->c#is_node_involved clname) cables in
     let defects = Defects_interface.get_defects_interface () in
     List.iter
       (fun cable ->
         defects#remove_cable cable#name;
         self#del_cable cable#name)
       cables_to_remove;
     cl#destroy;
     clouds  <- List.filter (fun x->not (x=cl)) clouds

 (** Remove a world_bridge from the network.
     Remove it from the [world_bridges] list and remove all related cables *)
 method del_world_bridge gname =
     let g  = self#get_world_bridge_by_name gname in
     (* Destroy cables first: they refer what we're removing... *)
     let cables_to_remove = List.filter (fun c->c#is_node_involved gname) cables in
     let defects = Defects_interface.get_defects_interface () in
     List.iter
       (fun cable ->
         defects#remove_cable cable#name;
         self#del_cable cable#name)
       cables_to_remove;
     g#destroy;
     world_bridges  <- List.filter (fun x->not (x=g)) world_bridges


 method change_node_name oldname newname =
   if oldname = newname then () else
   let node = self#get_node_by_name oldname in
   node#set_name newname ;

 (** Facilities *)

  (** Would a hypothetical cable of a given crossoverness be 'correct' if it connected the two
      given nodes? *)
  method would_a_cable_be_correct_between endpoint1_name endpoint2_name crossoverness =
    let polarity1 = (self#get_node_by_name endpoint1_name)#polarity in
    let polarity2 = (self#get_node_by_name endpoint2_name)#polarity in
    (* We need a crossover cable if the polarity is the same: *)
    match polarity1, polarity2 with
     | Intelligent , _      | _           , Intelligent -> true
     | MDI_X       , MDI    | MDI         , MDI_X       -> crossoverness = Direct
     | MDI_X       , MDI_X  | MDI         , MDI         -> crossoverness = Crossover

 (** List of machines names in the network *)
 method get_machine_names     =
   List.map (fun x->x#name) machines

 (** List of node names in the network *)
 method get_node_names  =
   List.map (fun x->x#name) (self#nodes)

 method get_device_names ?devkind () =
  let xs= match devkind with
  | None   -> devices
  | Some k -> List.filter (fun x -> x#devkind = k) devices
  in
  List.map (fun x->x#name) xs

 method get_devices_that_can_startup ~devkind () =
  ListExtra.filter_map
    (fun x -> if (x#devkind = devkind) && x#can_startup then Some x#get_name else None)
    devices

 method get_devices_that_can_gracefully_shutdown ~devkind () =
  ListExtra.filter_map
    (fun x -> if (x#devkind = devkind) && x#can_gracefully_shutdown then Some x#get_name else None)
    devices

 method get_devices_that_can_suspend ~devkind () =
  ListExtra.filter_map
    (fun x -> if (x#devkind = devkind) && x#can_suspend then Some x#get_name else None)
    devices

 method get_devices_that_can_resume ~devkind () =
  ListExtra.filter_map
    (fun x -> if (x#devkind = devkind) && x#can_resume then Some x#get_name else None)
    devices

 (** List of direct cable names in the network *)
 method get_direct_cable_names  =
   let clist= List.filter (fun x->x#cablekind=Direct) cables in
   List.map (fun x->x#name) clist

 (** List of crossover cable names in the network *)
 method get_crossover_cable_names  =
   let clist= List.filter (fun x->x#cablekind=Crossover) cables in
   List.map (fun x->x#name) clist

 (** List of cloud names in the network *)
 method get_cloud_names     =
   List.map (fun x->x#name) clouds

 (** List of world_bridge names in the network *)
 method get_world_bridge_names     =
   List.map (fun x->x#name) world_bridges

 (** Starting and showing the network *)

 (** List of reverted cables (used only for drawing network) *)
 method reverted_cables : (string list) =
   let clist= List.filter (fun x->x#dotoptions#reverted#get) cables in
   List.map (fun x->x#name) clist

 (** Set the reverted dotoptions field of a cable of the network (identified by name) *)
 method reverted_cable_set (x:bool) (cname:string) =
   (self#get_cable_by_name cname)#dotoptions#reverted#set x

 (** Show network topology *)
 method show =
   Log.printf "========== NETWORK STATUS ===========\n";
   (* show devices *)
   let msg= try
        (StringExtra.Fold.commacat
        (List.map (fun d->d#name^" ("^(string_of_devkind d#devkind)^")") devices))
        with _ -> ""
   in Log.printf "Devices \r\t\t: %s\n" msg;
   (* show machines *)
   let msg=try
        (StringExtra.Fold.commacat (List.map (fun m->m#show) machines))
        with _ -> ""
   in Log.printf "Machines \r\t\t: %s\n" msg;
   (* show clouds *)
   let msg=try
        (StringExtra.Fold.commacat (List.map (fun c->c#show) clouds))
        with _ -> ""
   in Log.printf "Clouds \r\t\t: %s\n" msg;
   (* show world_bridges *)
   let msg=try
        (StringExtra.Fold.commacat (List.map (fun c->c#show) world_bridges))
        with _ -> ""
   in Log.printf "World bridges \r\t\t: %s\n" msg;
   (* show links *)
   let msg=try
        (StringExtra.Fold.newlinecat (List.map (fun c->(c#show "\r\t\t  ")) cables))
        with _ -> ""
   in Log.printf "Cables \r\t\t: %s\n" msg


 (** {b Consider cable as Edge.edges} *)

 (** Gives the list of edges which left and right side verify the given conditions.
     An edge is a 3-uple (node1,receptname1), cable , (node2,receptname2) *)
 method edgesSuchThat (lpred: node->bool) (cpred:cable->bool) (rpred: node->bool) : edge list =
   let cable_info c =
     let (left, right) = (c#get_left, c#get_right) in
     let a = (left#node,  left#user_port_name) in
     let b = (right#node, right#user_port_name) in
     (a,c,b)
   in
   let l1 = List.map cable_info cables in
   let filter = fun ((n1,_),c,(n2,_)) -> (lpred n1) && (cpred c) && (rpred n2) in
   let l2 = (List.filter filter l1) in l2

 (** Gives the list of edges of a given kind. *)
 method edges_of_kind (lnkind: nodekind option) (ckind: cablekind option) (rnkind: nodekind option) =
   let npred nkind = fun n -> match nkind with None -> true | Some nk -> (n#nodekind  = nk) in
   let cpred kind  = fun c -> match kind  with None -> true | Some ck -> (c#cablekind = ck) in
   self#edgesSuchThat (npred lnkind) (cpred ckind) (npred rnkind)


 (** Gives the list of all edges in the network. *)
 method edges : edge list = self#edgesSuchThat (fun ln->true) (fun c->true) (fun rn->true)

(* ex colore dei cavi incrociati "#52504c" *)

 (** Network translation into the dot language *)
 method dotTrad () =
 let opt = self#dotoptions in
 let labeldistance = opt#labeldistance#get in
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
     (ListExtra.permute opt#shuffler_as_function self#nodes)
   ))
^"
/* ***********************
      DIRECT CABLE EDGES
   *********************** */


edge [dir=none,color=\""^Color.direct_cable^"\",fontsize=8,labelfontsize=8,minlen=1.6,"^
opt#labeldistance_for_dot^",tailclip=true];

"^
(StringExtra.Text.to_string
   (List.map (dotEdgeTrad labeldistance) (self#edges_of_kind None (Some Direct) None)))

^"
/* *********************************
      CROSSOVER/SERIAL CABLE EDGES
   ********************************* */


edge [headclip=true,minlen=1.6,color=\""^Color.crossover_cable^"\",weight=1];

"^
(StringExtra.Text.to_string
   (List.map (dotEdgeTrad labeldistance) (self#edges_of_kind None (Some Crossover) None)))

^"} //END of digraph\n"

 end (* method dotTrad *)

initializer

 self#set_dotoptions (new Dotoptions.network self);

end


(** {2 Saving and loading a Netmodel.network } *)

(** Pseudo XML now! (using xforest instead of ocamlduce) *)
module Xml = struct

 let network_marshaller = new Oomarshal.marshaller ;;

(** Parse the file containing an xforest representation of the network.
    The given network is updated during the parsing. *)
 let load_network (net:network) (fname:string) =
  let (forest:Xforest.t) = network_marshaller#from_file fname in
  Xforest.print_forest forest;
  match forest with
  | Forest.NonEmpty  (("network", attrs) , childs , Forest.Empty) ->
      net#from_forest ("network", attrs) childs
  | _ -> assert false
 ;;

(** Save the xforest representation of the network. *)
let save_network (net:network) (fname:string) =
 Log.printf "Netmodel.Xml.save_network: begin\n";
 Xforest.print_forest net#to_forest;
 network_marshaller#to_file net#to_forest fname;
 Log.printf "Netmodel.Xml.save_network: end (success)\n";;

end;; (* module Netmodel.Xml *)


end;; (* module Netmodel *)
