(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007  Jean-Vincent Loddo
   Copyright (C) 2007  Luca Saiu
   Updated in 2008 by Luca Saiu

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

open PreludeExtra.Prelude;; (* We want synchronous terminal output *)
open Strings;;
open Sugar;;
open ListExtra;;
open StringExtra;;
open SysExtra;;
open StrExtra;;
open UnixExtra;;
open Environment;;
open Oomarshal;;
open Task_runner;;
open Simple_dialogs;;
open Recursive_mutex;;

(** Some constants for drawing with colors. *)
module Color = struct
 let direct_cable  = "#949494" ;;
 let crossed_cable = "#6d8dc0" ;;
end;;

(** A thunk allowing to invoke the sketch refresh method, accessible from many
    modules: *)
let the_refresh_sketch_thunk : (unit -> unit) option ref = ref None;;
let set_refresh_sketch_thunk refresh_sketch_thunk =
  assert(!the_refresh_sketch_thunk = None);
  the_refresh_sketch_thunk := Some refresh_sketch_thunk;;
(** The sketch-refreshing procedure, implemented using the global thunk: *)
let refresh_sketch () =
  match !the_refresh_sketch_thunk with
    Some refresh_sketch_thunk ->
      refresh_sketch_thunk ()
  | None ->
      failwith "get_refresh_sketch_thunk: no global state has been set";;

(* **************************************** *
              Module MSys
 * **************************************** *)

(** Marionnet specific additionnal tools for system operations. *)
module MSys = struct 
 type x_policy = HostXServer | Xnest | NoX;;
 
 let x_policies       = [ HostXServer; Xnest; NoX ];;
 let hostxserver_name = "X HOST";;
 let xnest_name       = "X NEST";;
 let nox_name         = "No X";;
 
 let x_policy_of_string s =
   if s = hostxserver_name then HostXServer
   else if s = xnest_name then Xnest
   else if s = nox_name then NoX
   else failwith (Printf.sprintf "x_policy_of_string: ill-formed argument %s" s);;
 
 let string_of_x_policy p =
   match p with
     HostXServer -> hostxserver_name
   | Xnest -> xnest_name
   | NoX   -> nox_name;;
 
 let x_policies_as_strings = List.map string_of_x_policy x_policies;;

 let marionnet_home             = Pathnames.marionnet_home ;;
 let marionnet_home_filesystems = Pathnames.marionnet_home_filesystems ;;
 let marionnet_home_kernels     = Pathnames.marionnet_home_kernels ;;
 let marionnet_home_images      = Pathnames.marionnet_home_images ;;
 
 let machine_prefix = "machine-" ;;
 let router_prefix  = "router-"  ;;
 let kernel_prefix  = "linux-"   ;;
 
 let machine_pathname_prefix = marionnet_home_filesystems ^ machine_prefix ;;
 let router_pathname_prefix  = marionnet_home_filesystems ^ router_prefix  ;;
 let kernel_pathname_prefix  = marionnet_home_kernels     ^ kernel_prefix  ;;

 (** Read the given directory searching for names like [prefix ^ "xxxxx"];
     @return the list of ["xxxxx"]. If [suggested_text] is in the list, then
     return it as the {e first} element. If ?extra_element is given then return
     it as the first element (the second if [suggested_text] is present): *)
 let get_unprefixed_file_list ?extra_element prefix directory =
   let prefix_length = String.length prefix in
   let strip_prefix s = String.sub s prefix_length ((String.length s) - prefix_length) in
   let list =
     Sys.readdir_into_list
       ~namefilter:(fun file_name -> ((String.length file_name) > prefix_length) &&
                                     ((String.sub file_name 0 prefix_length) = prefix))
       ~nameconverter:strip_prefix
       directory in
   let list = (* Remove directories from the list *)
     List.filter
       (fun file_or_directory ->
         try
           let _ = Sys.readdir (directory ^ "/"^ prefix ^file_or_directory) in
           (* we never get here if readdir raises an exception *)
           false
         with _ ->
           true)
       list in
   let list =
     match extra_element with
       None -> list
     | Some extra_element -> extra_element :: list in
   let list_with_default_first_if_default_is_present =
     if List.exists (fun x -> x = suggested_text) list then
       suggested_text :: (List.filter (fun x -> not (x = suggested_text)) list)
     else
       list in
   list_with_default_first_if_default_is_present;;

 (** Return the list of unprefixed machine filesystem file names: *)
  let machine_filesystem_list () = 
    get_unprefixed_file_list
      machine_prefix
      marionnet_home_filesystems;;

 (** Return the list of unprefixed router filesystem file names: *)
  let router_filesystem_list () = 
    get_unprefixed_file_list
      router_prefix
      marionnet_home_filesystems;;

 (** Return the list of unprefixed kernel file names: *)
 let kernelList () = 
    get_unprefixed_file_list
      kernel_prefix
      marionnet_home_kernels;;

 (** Return the list of unprefixed variant file names for the given (prefixed)
     filesystem; if an entry named like filename_which_should_be_first_if_present,
     when specified, exists, then return it before everything else including
     no_variant_text.*)
 let variant_list_of prefixed_filesystem () =
   get_unprefixed_file_list
     ~extra_element:no_variant_text
     ""
     (marionnet_home_filesystems ^ "/" ^ prefixed_filesystem ^ "_variants");;

 (** Terminal choices to handle uml machines *)
 let termList = List.map string_of_x_policy [HostXServer; Xnest] (*x_policies_as_strings*);;

 (** Kinds for Marionnet related files *)
 type file_kind = Icon | Patch | Network | Distrib | Script ;;

 (** Correct the name of a program resource (not a project resource!) 
     according to the install directory of the application *)
 let whereis filename kind = match kind with         (* TO IMPLEMENT *)
 | Icon   -> Pathnames.marionnet_home^"/images/"^filename
 |  _     -> filename
 ;;
end;;



(* *************************** *
        Module Dotoptions
 * *************************** *) 

module Dotoptions = struct

(** The iconsize converter float -> string *)
let iconsize_of_float x =
     match (int_of_float x) with
     | 0 -> "small"
     | 1 -> "med"
     | 2 -> "large"
     | 3 -> "xxl" 
     | default -> "large" 
;;

(** The iconsize converter string -> float *)
let float_of_iconsize s = 
    match s with
     | "small" -> 0. 
     | "med"   -> 1.
     | "large" -> 2.
     | "xxl"   -> 3. 
     | default -> 2.
;;

(** Handler for reading or setting related widgets *)
class guiHandler = fun (win:Gui.window_MARIONNET) -> 

  object (self)
 
  (** Handling iconsize adjustment *)

  method get_iconsize : string        =  iconsize_of_float (win#adj_iconsize#adjustment#value)
  method set_iconsize (x:string)      =  x => (float_of_iconsize || win#adj_iconsize#adjustment#set_value)  

  (** Handling nodesep adjustment *)

  (* Non-linear (quadratic) adjustment in the range [0,2] inches *) 
  method get_nodesep  : float         =  let formule = fun x -> (((x /. 20.) ** 2.) *. 2.) in
                                         win#adj_nodesep#adjustment#value => formule
  
  method set_nodesep  (y:float)       =  let inverse = fun y -> 20. *. sqrt (y /. 2.) in 
                                         y => (inverse || win#adj_nodesep#adjustment#set_value)  

  (** Handling labeldistance adjustment *)

  (* Non-linear (quadratic) adjustment in the range [0,2] inches *) 
  method get_labeldistance  : float   =  let formule = fun x -> (((x /. 20.) ** 2.) *. 2.) in
                                         win#adj_labeldistance#adjustment#value => formule

  method set_labeldistance  (y:float) =  let inverse = fun y -> 20. *. sqrt (y /. 2.) in 
                                         y => (inverse || win#adj_labeldistance#adjustment#set_value)  


  (** Handling extrasize adjustment *)

  method get_extrasize  : float     =  win#adj_extrasize#adjustment#value
  method set_extrasize  (x:float)   =  win#adj_extrasize#adjustment#set_value x 

  (** Handling the network image *)

  method get_image                =  win#sketch#pixbuf 
  method get_image_current_width  = (GdkPixbuf.get_width  win#sketch#pixbuf) 
  method get_image_current_height = (GdkPixbuf.get_height win#sketch#pixbuf) 
  
  val mutable image_original_width  = None
  val mutable image_original_height = None

  (** Called in update_sketch () *)
  method reset_image_size () = image_original_width <- None; image_original_height <- None 

  (* Get and affect if need (but only the first time) *) 
  method get_image_original_width   = match image_original_width with
  | None    -> (let x = self#get_image_current_width in image_original_width <- Some x; x) 
  | Some x  -> x 

  (* Get and affect if need (but only the first time) *) 
  method get_image_original_height   = match image_original_height with
  | None    -> (let x = self#get_image_current_height in image_original_height <- Some x; x) 
  | Some x  -> x 
  
end ;; (* class gui *)



(* ************************* 
    class Dotoptions.network 
   ************************* *)
 
type index = int;; (* 0..(length-1) *)
type shuffler = index list ;; (* represent a permutation of indexes of a list*)

(** Dot options for a network *)
let network_marshaller = new Oomarshal.marshaller;;

class network = 
  
  fun ?(iconsize="large") ?(shuffler=[]) ?(rankdir="TB") ?(nodesep=0.5) ?(labeldistance=1.6) ?(extrasize=0.) 

      (* The handler for gui related part *)
      (gui:guiHandler) 

      (* The handler for the real network *)
      (net:(<invertedCables:(string list); invertedCableSet:(bool->string->unit); ..>))  ->

  object (self)
  inherit Xforest.interpreter ()
  
  val mutable iconsize        : string       = iconsize   
  val mutable shuffler        : (index list) = shuffler  
  val mutable rankdir         : string       = rankdir      
  val mutable nodesep         : float        = nodesep    
  val mutable labeldistance   : float        = labeldistance        
  val mutable extrasize       : float        = extrasize
  val mutable gui_callbacks_disable : bool   = false

  method rankdir              = rankdir
  method labeldistance        = labeldistance

  (** Handlig methods of the real network *)
  method invertedCables       = net#invertedCables
  method invertedCableSet     = net#invertedCableSet

  method get_iconsize         = iconsize 
  method get_shuffler         = shuffler => List.asFunction  (* returns the permutation function *)
  method get_rankdir          = "rankdir="^rankdir^";"
  method get_nodesep          = let s=(string_of_float nodesep) in ("nodesep="^s^"; ranksep="^s)
  method get_labeldistance    = "labeldistance="^(string_of_float labeldistance)

  (** This is the method used in user gui callbacks (reactions) *)
  method are_gui_callbacks_disable   = gui_callbacks_disable

  method disable_gui_callbacks    () = gui_callbacks_disable <- true
  method enable_gui_callbacks     () = 
   begin
   (GMain.Timeout.add ~ms:500 ~callback:(fun () -> gui_callbacks_disable <- false; false)) => ignore 
   end
 
  (** This method must be used with List.shuffleIndexes, for instance : 
      opt#set_shuffler (List.shuffleIndexes (net#nodes)); *)
  method set_shuffler (l:index list) =  shuffler     <- l
  method set_rankdir        x        =  rankdir      <- x
  method reset_shuffler    ()        =  shuffler     <- []
  method set_extrasize      x        =  extrasize    <- x
  
  (* Accessors stuff *)
  method get_iconsize   = iconsize
  method set_iconsize x = iconsize <- x
  method set_nodesep x = nodesep <- x
  method set_labeldistance x = labeldistance <- x
  method get_gui_callbacks_disable   = gui_callbacks_disable
  method set_gui_callbacks_disable x = gui_callbacks_disable <- x


  method reset_extrasize () = 
    begin
    self#gui#reset_image_size ();
    extrasize <- 0.; 
    ()
    end
  
  method reset_defaults () = 
    begin
      iconsize  <- "large"; shuffler <- []; rankdir <- "TB"; nodesep <- 0.5; labeldistance <- 1.6 ;
      List.foreach self#invertedCables (self#invertedCableSet false) ;
      self#reset_extrasize () ;
      self#write_gui ()
    end

  method ratio : string = if (extrasize = 0.) then "ratio=compress;" else 
   begin
    let x = self#gui#get_image_original_width  => Widget.Image.inch_of_pixels in
    let y = self#gui#get_image_original_height => Widget.Image.inch_of_pixels in
    prerr_endline ("ratio original x="^(x => string_of_float));  
    prerr_endline ("ratio original y="^(y => string_of_float));  
    let area  = x *. y in
    let delta_area = extrasize *. area /. 100. in
    let delta = sqrt( (x+.y)**2. +. 4.*. delta_area  )  -.  (x+.y)  in
    let x = x +. delta => string_of_float in
    let y = y +. delta => string_of_float in
    prerr_endline ("ratio x="^x);  
    prerr_endline ("ratio y="^y);
    "size=\""^x^","^y^
    "\";\nratio=fill;"
   end

  (** Methods for handling gui (for getting and setting). Inverted cables corresponds to dynamic menus,
      so they not need to be reactualized (the dynamic menus are recalculated each time from self#invertedCables. *)
  method gui = gui

  (** The value read from gui is used to set the correspondent field of self, then is returned as result *)  

  method read_gui_iconsize       () : string = iconsize      <- (self#gui#get_iconsize)       ; iconsize 
  method read_gui_nodesep        () : float  = nodesep       <- (self#gui#get_nodesep)        ; nodesep
  method read_gui_labeldistance  () : float  = labeldistance <- (self#gui#get_labeldistance)  ; labeldistance
  method read_gui_extrasize      () : float  = extrasize     <- (self#gui#get_extrasize )     ; extrasize

  (** The dotoption gui reactualization *)

  method write_gui_iconsize      () : unit = self#gui#set_iconsize      iconsize 
  method write_gui_nodesep       () : unit = self#gui#set_nodesep       nodesep 
  method write_gui_labeldistance () : unit = self#gui#set_labeldistance labeldistance
  method write_gui_extrasize     () : unit = self#gui#set_extrasize     extrasize
  method write_gui               () : unit = 
    begin
      self#disable_gui_callbacks   () ;
      self#write_gui_iconsize      () ;
      self#write_gui_nodesep       () ;
      self#write_gui_labeldistance () ;
      self#write_gui_extrasize     () ;
      self#enable_gui_callbacks    () ;
      ()
    end

  (** Marshalling is performed in this ugly way because directly dumping the whole [self] object
      would involve resolving references to Gtk callbacks, which are outside the OCaml heap and
      hence (understandably) not supported by the marshaller. *)

(** {b To do: should I also manage invertedCables and invertedCableSet? Ask Jean} *)

  (** Dump the current state of [self] into the given file. *)
  method save_to_file (file_name : string) =
    Xforest.print_forest net#to_forest;
    network_marshaller#to_file self#to_forest file_name

  (** This method is used just for undumping dotoptions, so is not strict. 
      For instance, exceptions provoked by bad cable names are simply ignored. *)
  method set_invertedCables names =  
    List.foreach names (fun n -> try (self#invertedCableSet true n) with _ -> ()) 

  (** Undump the state of [self] from the given file. *)
  method load_from_file (file_name : string) = 
   let forest = network_marshaller#from_file file_name in
   Xforest.print_forest forest;
   match forest with
   | Forest.NonEmpty  (("dotoptions", attrs) , childs , Forest.Empty) -> 
      self#from_forest ("dotoptions", attrs) childs

 (** Dotoptions to forest encoding. *)
  method to_forest = 
   Forest.leaf ("dotoptions", [
    		   ("iconsize"      , iconsize                       ) ;
    		   ("shuffler"      , (Xforest.encode shuffler)      ) ;
                   ("rankdir"       , rankdir                        ) ;
                   ("nodesep"       , (string_of_float nodesep)      ) ;
                   ("labeldistance" , (string_of_float labeldistance)) ;
                   ("extrasize"     , (string_of_float extrasize)    ) ;
                   ("gui_callbacks_disable", (string_of_bool gui_callbacks_disable)) ;
                   ("invertedCables", (Xforest.encode self#invertedCables)) ;
	           ])

 (** A Dotoption.network has just attributes (no childs) in this version. 
     The Dotoption.network must be undumped AFTER the Netmodel.network in 
     order to have significant cable names (invertedCables). *)
 method eval_forest_attribute = function
  | ("iconsize"             , x ) -> self#set_iconsize       x
  | ("shuffler"             , x ) -> self#set_shuffler      (Xforest.decode x)
  | ("rankdir"              , x ) -> self#set_rankdir        x
  | ("nodesep"              , x ) -> self#set_nodesep       (float_of_string x)
  | ("labeldistance"        , x ) -> self#set_labeldistance (float_of_string x)
  | ("extrasize"            , x ) -> self#set_extrasize     (float_of_string x)
  | ("gui_callbacks_disable", x ) -> self#set_gui_callbacks_disable (bool_of_string x)
  | ("invertedCables"       , x ) -> self#set_invertedCables (Xforest.decode x)
  | _ -> () (* Forward-comp. *)
 
end;; (* class Dotoptions.network *)  


(** Dot options for a cable *)
class cable =
  fun ?(inverted=false)
      () ->
object (self)
   val mutable inverted   = inverted 
   method      inverted   = inverted
   method  set_inverted x = inverted <- x 

end;; (* class Dotoptions.cable *)


end;; (* module Dotoptions *)  


(* **************************************** *
              Module Netmodel
 * **************************************** *)


(** Model for managing the virtual network defined step by step by user *)
module Netmodel = struct 

(** {2 Basic functions and types } *)

(** A device may be a Hub, a Switch or a Router. *) 
type devkind   = Hub | Switch | Router | NotADevice ;; 

(** A cable may be Direct, Crossed or NullModem. *) 
type cablekind = Direct | Crossed | NullModem ;;
 
(** A port (of machine or device) may Eth (ethernet) or Ttys (serial). *) 
type portkind  = Eth | TtyS ;;

(** A node is a Machine, a Device, a Cloud or a GwInternet. 
    Cable are edge in the network, which is a graph of these nodes. *) 
type nodekind  = Machine | Device | Cloud | Gateway ;;

(** String conversion for a devkind. *)
let string_of_devkind = function 
  | Hub        -> "hub" 
  | Switch     -> "switch"
  | Router     -> "router"
  | NotADevice -> raise (Failure "string_of_devkind: NotADevice")
;;

(** String conversion for a cablekind. *)
let string_of_cablekind = function 
  | Direct     -> "direct"
  | Crossed    -> "crossed"
  | NullModem  -> "nullmodem"
;;

(** String conversion for a portkind. *)
let string_of_portkind = function 
  | Eth   -> "eth" 
  | TtyS  -> "ttyS"
;;

(** The portkind interpretation of the given string. *)
let devkind_of_string x = match x with 
  | "hub"     -> Hub 
  | "switch"  -> Switch
  | "router"  -> Router
  | _         -> raise (Failure ("devkind_of_string"^x))
;;

(** The cablekind interpretation of the given string. *)
let cablekind_of_string x = match x with  
  | "direct"    -> Direct
  | "crossed"   -> Crossed
  | "nullmodem" -> NullModem
  | "serial"    -> NullModem
  | _           -> raise (Failure ("cablekind_of_string: "^x))
;;


(** Examples: pc1, pc2 if the node is a machines, A,B,C if the node is a device *)
type nodename   = string ;;

(** Examples: eth0..eth4, ttyS0, ttyS1 *)
type receptname = string ;;

(** A socketname is a pair (nodename,receptname), for instance ("rome";"eth0") 
    or ("H1";"eth2") or ("paris","ttyS1"). *)
type socketname = { mutable nodename:string ; mutable receptname:string ; } ;;

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

exception ForbiddenTransition;;

(** This represents the current state of a simulated device (as per
    network_simulation.ml) and enables easy high-level state transitions which 
    conveniently hide the complexity of managing switches and cables; when the
    user tries to invoke any forbidden state transition an exception is
    raised. *)
class virtual simulated_device = object(self)
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
  val simulated_device : Simulated_network.device option ref =
    ref None

  (** Create a new simulated device according to the current status *)
  method virtual make_simulated_device : Simulated_network.device

  (** Return the list of cables directly linked to a port of self as an endpoint.
      This is needed so that simulated cables can be automatically started/destroyed
      as soon as both their endpoints are created/destroyed *)
  method private get_involved_cables = []

  (** Return a hublet process by name, so that we're able to easily find where to
      connect/disconnect cables. This fails if the device is non-existing, as there
      are no hublets in this case *)
  method get_hublet_process_by_interface_name name =
    match !simulated_device with
      Some(sd) -> sd#get_ethernet_port_by_name name
    | None -> failwith "looking for a hublet when its device is non-existing"

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
          Printf.printf "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"; 
          let message = 
            Printf.sprintf "enqueue_task_with_progress_bar: %s %s failed (%s)"
              verb self#get_name (Printexc.to_string e) in
          Printf.printf "%s\n" message;
          Simple_dialogs.warning message message ();
          Printf.printf "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n";
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
  
  method destroy =
    (* This is invisible for the user: don't set the next state *)
    the_task_runner#schedule ~name:("destroy "^self#get_name)(fun () -> self#destroy_right_now)

  method startup =
    self#set_next_simulated_device_state (Some DeviceOn);
    self#enqueue_task_with_progress_bar "Démarrage de" (fun () -> if self#can_startup then self#startup_right_now)
  
  method suspend =
    self#set_next_simulated_device_state (Some DeviceSleeping);
    self#enqueue_task_with_progress_bar "Suspension de" (fun () -> if self#can_suspend then self#suspend_right_now)
  
  method resume =
    self#set_next_simulated_device_state (Some DeviceOn);
    self#enqueue_task_with_progress_bar "Réveil de" (fun () -> if self#can_resume then self#resume_right_now)
  
  method gracefully_shutdown =
    self#set_next_simulated_device_state (Some DeviceOff);
    self#enqueue_task_with_progress_bar "Arrêt de" (fun () -> if self#can_gracefully_shutdown then self#gracefully_shutdown_right_now)
  
  method poweroff =
    self#set_next_simulated_device_state (Some DeviceOff);
    self#enqueue_task_with_progress_bar "Débranchement de" (fun () -> if self#can_poweroff then self#poweroff_right_now)

  method (*private*) create_right_now =
    with_mutex mutex
      (fun () ->
        print_string ("+ About to create the simulated device " ^ (self#get_name) ^ "\n");
        print_string ("  (it's connected to " ^
                      (string_of_int (List.length (self#get_involved_cables))) ^
                      " cables)\n");
        match !automaton_state, !simulated_device with
          NoDevice, None -> (simulated_device := (Some self#make_simulated_device);
                             automaton_state := DeviceOff;
                             self#set_next_simulated_device_state None;
                             (* An endpoint for cables linked to self was just added; we
                                may need to start some cables. *)
                             ignore (List.map
                                       (fun cable -> print_string ("+ Working on cable\n"^(cable#show "")^"\n");
                                         cable#increment_alive_endpoints_no)
                                       (self#get_involved_cables)))
        | _ -> raise ForbiddenTransition)

  (** The unit parameter is needed: see how it's used in simulated_network: *)
  method private destroy_because_of_unexpected_death () =
    Printf.printf "You don't deadlock here %s, do you? -1\n" self#get_name; flush_all ();
    with_mutex mutex
      (fun () ->
        Printf.printf "You don't deadlock here %s, do you? 0\n" self#get_name;
        flush_all ();
        (try
          self#destroy_right_now
        with e -> begin
          Printf.printf "WARNING: destroy_because_of_unexpected_death: failed (%s)\n"
            (Printexc.to_string e);
        end;
          self#set_next_simulated_device_state None)); (* don't show next-state icons for this *)

  method (*private*) destroy_right_now =
    with_mutex mutex
      (fun () ->
        print_string ("- About to destroy the simulated device " ^ (self#get_name) ^ "\n");
        match !automaton_state, !simulated_device with
          (DeviceOn | DeviceSleeping), Some(d) ->
            (print_string ("  (destroying the on/sleeping device " ^ (self#get_name) ^ ". Powering it off first...)\n");
             self#poweroff_right_now; (* non-gracefully *)
             self#destroy_right_now)
        | NoDevice, None ->
            print_string ("  (destroying the already 'no-device' device " ^ (self#get_name) ^ ". Doing nothing...)\n");
            () (* Do nothing, but don't fail. *)
        | DeviceOff, Some(d) ->
            ((* An endpoint for cables linked to self was just added; we
                may need to start some cables. *)
             print_string ("  (destroying the off device " ^ (self#get_name) ^ ": decrementing its cables rc...)\n");
             List.iter
               (fun cable ->
                 print_string ("- Unpinning the cable "^(cable#show "")^"\n"); flush_all ();
                 cable#decrement_alive_endpoints_no;
                 print_string ("- The cable "^(cable#show "")^" was unpinned with success\n"); flush_all ())
               self#get_involved_cables;
             print_string ("  (destroying the simulated device implementing " ^ (self#get_name) ^ "...)\n");
             d#destroy; (* This is the a method from some object in Simulated_network *)
             simulated_device := None;
             automaton_state := NoDevice;
             self#set_next_simulated_device_state None;
             Printf.printf "- We're not deadlocked yet (%s). Great.\n" self#get_name; flush_all ());
        | _ ->
            raise ForbiddenTransition);
    print_string ("- The simulated device " ^ (self#get_name) ^ " was destroyed with success\n");
    flush_all ()
    

  method (*private*) startup_right_now =
    with_mutex mutex
      (fun () ->
        (* Don't startup ``incorrect'' devices. This is currently limited to cables of the
           wrong crossedness which the user has defined by mistake: *)
        if self#is_correct then begin
          print_string ("* starting up the device " ^ (self#get_name) ^ "...\n");
          match !automaton_state, !simulated_device with 
            NoDevice, None -> (print_string ("  (creating processes for " ^ (self#get_name) ^ " first...)\n");
                               self#create_right_now; 
                               print_string ("  (processes for " ^ (self#get_name) ^ " were created...)\n");
                               self#startup_right_now) 
          | DeviceOff, Some(d) -> (d#startup;  (* This is the a method from some object in Simulated_network *)
                                   automaton_state := DeviceOn;
                                   self#set_next_simulated_device_state None;
                                   print_string ("* The device " ^ (self#get_name) ^ " was started up\n"))
          | _ -> raise ForbiddenTransition
        end else begin
          print_string ("* REFUSING TO START UP the ``incorrect'' device " ^ (self#get_name) ^ "...\n");
        end)

  method (*private*) suspend_right_now =
    with_mutex mutex
      (fun () ->
        print_string ("|| Suspending up the device " ^ (self#get_name) ^ "...\n");
        match !automaton_state, !simulated_device with
          DeviceOn, Some(d) -> (d#suspend; (* This is the a method from some object in Simulated_network *)
                                automaton_state := DeviceSleeping;
                                self#set_next_simulated_device_state None)
        | _ -> raise ForbiddenTransition)
              
  method (*private*) resume_right_now =
    with_mutex mutex
      (fun () ->
        print_string ("|> Resuming the device " ^ (self#get_name) ^ "...\n");
        match !automaton_state, !simulated_device with
          DeviceSleeping, Some(d) -> (d#resume; (* This is the a method from some object in Simulated_network *)
                                      automaton_state := DeviceOn;
                                      self#set_next_simulated_device_state None)
        | _ -> raise ForbiddenTransition)

  method (*private*) gracefully_shutdown_right_now =
    with_mutex mutex
      (fun () ->
        print_string ("* Gracefully shutting down the device " ^ (self#get_name) ^ "...\n");
        match !automaton_state, !simulated_device with
          DeviceOn, Some(d) -> (d#gracefully_shutdown; (* This is the a method from some object in Simulated_network *)
                                automaton_state := DeviceOff;
                                self#set_next_simulated_device_state None)
        | DeviceSleeping, Some(d) -> (self#resume_right_now;
                                      self#gracefully_shutdown_right_now)
        | _ -> raise ForbiddenTransition)

  method (*private*) poweroff_right_now =
    with_mutex mutex
      (fun () ->
        print_string ("* Powering off the device " ^ (self#get_name) ^ "...\n");
        match !automaton_state, !simulated_device with
          DeviceOn, Some(d) -> (d#shutdown; (* non-gracefully *)
                                automaton_state := DeviceOff;
                                self#set_next_simulated_device_state None)
        | DeviceSleeping, Some(d) -> (self#resume_right_now;
                                      self#poweroff_right_now)
        | _ -> raise ForbiddenTransition)

  method set_hublet_processes_no n =
    with_mutex mutex
      (fun () ->
        print_string ("* Updating the number of hublets of the device " ^ (self#get_name) ^ "...\n");
        match !automaton_state, !simulated_device with
          DeviceOff, Some(d) -> d#set_hublet_processes_no n (* update hublets and don't change state *)
        | NoDevice, None -> (self#create_right_now;                    (* Make hublets... *)
                             self#set_hublet_processes_no n) (* ...and do the real work in state Off *)
        | _ ->  raise ForbiddenTransition)
              
  (** Return true iff the current state allows to 'startup' the device from the GUI. *)
  method can_startup =
    with_mutex mutex
      (fun () ->
        match !automaton_state with NoDevice | DeviceOff -> true | _ -> false)

  (** Return true iff the current state allows to 'shutdown' a device from the GUI. *)
  method can_gracefully_shutdown =
    with_mutex mutex
      (fun () ->
        match !automaton_state with DeviceOn | DeviceSleeping -> true | _ -> false)

  (** Return true iff the current state allows to 'power off' a device from the GUI. *)
  method can_poweroff =
    with_mutex mutex
      (fun () ->
        match !automaton_state with NoDevice | DeviceOff -> false | _ -> true)

  (** Return true iff the current state allows to 'suspend' a device from the GUI. *)
  method can_suspend =
    with_mutex mutex
      (fun () ->
        match !automaton_state with DeviceOn -> true | _ -> false)

  (** Return true iff the current state allows to 'resume' a device from the GUI. *)
  method can_resume =
    with_mutex mutex
      (fun () ->
        match !automaton_state with DeviceSleeping -> true | _ -> false)

  (** 'Correctness' support: this is needed so that we can refuse to start incorrectly
      placed components such as Ethernet cables of the wrong crossedness, which the user
      may have created by mistake: *)
  val is_correct =
    ref true (* devices are ``correct'' by default *)
  method is_correct =
    with_mutex mutex
      (fun () ->
        !is_correct)
  method set_correctness correctness =
    with_mutex mutex
      (fun () ->
        is_correct := correctness)
end;;


(* *************************** *
      class common 
 * *************************** *) 

(** General-purpose class with common fields as
    - an automatically generated identifier
    - a (mutable) name; i.e. valid string identifier
    - a (mutable) label (string) not containing '<' and '>' (for prevent conflict with dot)
*)
class id_name_label = fun ?(name="noname") ?(label="") () -> 
   
  (* Some checks over used name and label *)
  let wellFormedLabel =  (Str.Bool.match_string ".*[><].*") || not in

  let check_name  x = 
  	if not (Str.wellFormedName  x) 
       	then failwith ("Setting component "^name^": invalid name")
        else x in
  
  let check_label x = 
  	if not (wellFormedLabel x)         
        then failwith ("Setting component "^name^": invalid label")
        else (String.strip x) in
  
  object (self)
  
  (** A component has an immutable identifier. *)
  val id : int = Identifier.fresh ()
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

(** A component may be a node (machine or device) or a cable (direct, crossed or nullmodem).
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
          class receptacle
 * *************************** *) 

(** A receptacle is the abstract notion representing both ethernet and serial ports.
    It's a component with a portkind [(Eth|TtyS)]. *)
class receptacle = fun ~network (name:receptname) (index:int) label (kind:portkind) -> 
  
  (* Some checks over receptacle name *)

   let wellFormedReceptName k x =  
    match k with
    | Eth  -> (Str.Bool.match_string "^eth[0-9]$"        x) or 
              (Str.Bool.match_string "^eth[1-9][0-9]+$"  x) or 
              (Str.Bool.match_string "^port[0-9]$"       x) or 
              (Str.Bool.match_string "^port[1-9][0-9]+$" x) 
  
    | TtyS -> (Str.Bool.match_string "^ttyS[0-9]$"       x) or 
              (Str.Bool.match_string "^ttyS[1-9][0-9]+$" x) in

  let check_receptname k x = if not (wellFormedReceptName k x) then 
    failwith ("Setting receptacle: name "^x^" not valid for this kind of receptacle")
    else x in

  object (self)

  inherit id_name_label ~name:(check_receptname kind name) ~label ()

  (** The kind of receptacle (Eth|TtyS) *)
  method portkind = kind

  (** A receptacle has an index in its container. This field is normally coherent with the label. 
      For instance, one expects that a receptacle with the label "eth2" will have the index=2. *)
  method index = if (index>=0)
                 then index 
                 else name => ((Str.extract_groups (Str.regexp "[a-z]*\([0-9]+\)")) || List.hd || int_of_string)
end;;


(* *************************** *
          class node
 * *************************** *) 


(** A node of the network is a container of receptacles. 
    Defects may be added after the creation, using the related method. *)
class virtual node = fun 
   ~network 
   ?(name="nonodename") 
   ?(label="") 
   ?(nodekind = Machine) 
   ?(devkind  = NotADevice) 
   ?(rlist:receptacle list = []) () -> 

   object (self)
   inherit component ~network ~name ~label ()
   inherit simulated_device

  (** 'Static' methods (in the sense of C++/Java). Polarity is used to decide the correct
      kind of Ethernet cable needed to connect a pair of devices: the cable should be
      crossover iff both endpoints have the same polarity: *)
  method virtual polarity : bool

  (** Constant fields (methods) *)
  
  (** The mutable list of receptacle of this component. *)
  val mutable nodekind = nodekind
  val mutable devkind  = devkind

  method set_nodekind x = nodekind <- x
  method set_devkind  x = devkind  <- x

  (** The kind of the node (Machine or Device). Alias for get_kind.*)
  method nodekind = nodekind

  (** The kind of the device (if the node is a device). *)
  method devkind = devkind

  (** The method which give access to the object describing defects of this component. *)
  (* method defect = defect *)

  (** Variable fields *)

  (** The mutable list of receptacle of this component. *)
  val mutable receptacles : (receptacle list) = rlist

  method set_receptacles x = receptacles <- x
  
  (** Retrieving informations about receptacles *)

  (** Basic accessor for the list of receptacles of this node. The portkind may be specified. *)
  method get_receptacles ?(portkind:(portkind option)=None) () = 
    match portkind with
    | None   -> receptacles
    | Some k -> List.filter (fun r->r#portkind=k) receptacles

  (** Get a receptacle of this node by the name of this receptacle (for instance ["eth0"]). *)
  method getReceptacleByName rname = List.find (fun x->x#name=rname) receptacles

  (** A receptacle of the given name exists in this node? *)
  method receptacleExists n = let f=(fun x->x#name=n) in (List.exists f receptacles)

  (** The number of receptacles of this node. The portkind may be specified. *)
  method numberOfReceptacles ?(portkind:(portkind option)=None) () 
      = List.length (self#get_receptacles ~portkind ())

  (** The max index in the list of receptacles of this node. The portkind is here mandatory. *)
  method maxReceptacleIndex (k:portkind)  = 
    let l = (List.map (fun x->x#index) (self#get_receptacles ~portkind:(Some k) ())) in
    if l=[] then -1 else (List.max l)

  (** The list of names of receptacles of this node. The portkind may be specified. *)
  method receptacleNames ?(portkind:(portkind option)=None) () = 
    List.map (fun x->x#name) (self#get_receptacles ~portkind ())

  (** The list of socketnames [\{nodename:string; receptname:string\}] 
      contained in this node. The [nodename] field will be constant to the name of this node in the returned list. 
      The portkind may be specified. *)
(*  method socketnames ?(portkind:(portkind option)=None) () = 
    List.map (fun x->{nodename=self#name; receptname=x}) (self#receptacleNames ~portkind ()) *)
  method socketnames = 
    List.map (fun x->{nodename=self#name; receptname=x}) (self#receptacleNames ())

  (** Just a version of socketnames returning something easier to manage than a list *)
  method socketnames_as_pair = 
    match self#socketnames with
      [ a; b ] ->
        a, b
    | _ ->
        assert false

  (** Adding and removing receptacles *)
      
  (** Append a receptacle at the end of the receptacles list of this node. This action is performed 
      checking the unicity of the receptacle name. *)
  method append_receptacle r  = 
    let name_exists = List.exists (fun x->x=r#name) (self#receptacleNames ()) in
    if  name_exists 
          then raise (Failure "add_receptacle : name already exists in this node")
          else receptacles <- (receptacles@[r])  

  (** Append a receptacle at the end of the receptacles list of this node. This action is performed 
      using a fresh index and name for the created receptacle. The portkind is here mandatory. *)
  method add_progressive_receptacle ?(prefix=None) (k:portkind) = 
    let num = (self#maxReceptacleIndex k) + 1 in
    let pref = (match prefix with None -> (string_of_portkind k) | Some x -> x) in 
    let name = pref^(string_of_int num) in
    let r = new receptacle network name num "" k in
    self#append_receptacle r


  (** Remove the last receptacle (with the maximal index) of the given portkind from this node. *)
  method rm_last_receptacle (k:portkind) = 
    let i = self#maxReceptacleIndex k in
    let concerned = (fun r->(r#portkind=k) && (r#index=i)) in
    receptacles <- List.filter (fun r -> not (concerned r)) receptacles 

  (** Re-adjusting the number of receptacles *)

  (** Set the number of receptacles of the given [portkind] to the given number [x]. 
      If the current number of receptacles is greater than [x] then append else remove the necessary number
      of receptacles.*)
  method readjust_receptacleNumber ?(prefix=None) (k:portkind) x = 
    let n = (self#numberOfReceptacles ~portkind:(Some k) ()) in 
    match (n>x,n<x) with
    | (true,  _ )  -> (self#rm_last_receptacle k)                 ; self#readjust_receptacleNumber ~prefix k x
    | ( _  ,true)  -> (self#add_progressive_receptacle ~prefix k) ; self#readjust_receptacleNumber ~prefix k x 
    |      _       -> ()


  (** {b Convenient aliases} *)

  method get_eth_number                   = self#numberOfReceptacles ~portkind:(Some Eth) ()
  method set_eth_number ?(prefix="eth") x = self#readjust_receptacleNumber ~prefix:(Some prefix) Eth x 

  method get_ttyS_number   = self#numberOfReceptacles ~portkind:(Some TtyS) ()
  method set_ttyS_number x = self#readjust_receptacleNumber TtyS x 

  
  (** Node dot traduction *)

  (** Returns an image representig the node with the given iconsize. *)    
  method virtual dotImg : iconsize -> string

  (** Returns the label to use for cable representation. 
      This method may be redefined (for instance in [gateway]). *)    
  method dotLabelForEdges (receptname:string) = self#get_label

  (** Returns the port to use for cable representation. 
      This method may be redefined (for instance in [gateway]). *)    
  method dotPortForEdges (receptname:string)  = receptname

  (** A node is represented in dot with an HTML label which is a table 
      with a first line containing the name, with a second line containing the node associated image (method [dotImg]),
      and, if the node has a label, a third line containing the label. With the [nodeoptions] parameter one can force,
      for example, the fontsize or fontname for both name and label : 
      [ dotTrad ~nodeoptions="fontsize=8" "large" ] *)
  method dotTrad ?(nodeoptions="") (z:iconsize) = 
    let label_line = if self#get_label="" then "" else "<TR><TD><FONT COLOR=\"#3a3936\">"^self#get_label^"</FONT></TD></TR>" in
    let fontsize   = if self#nodekind=Machine then "" else "fontsize=8," in
    let nodeoption = if nodeoptions = "" then "" else (nodeoptions^",") in
    begin
    self#name^" ["^fontsize^nodeoptions^"shape=plaintext,label=<
<TABLE BORDER=\"0\" CELLBORDER=\"0\" CELLSPACING=\"0\" CELLPADDING=\"0\">
  <TR><TD>"^self#name^"</TD></TR>
  <TR><TD PORT=\"img\"><IMG SRC=\""^(self#dotImg z)^"\"></IMG></TD></TR>
"^label_line^"
</TABLE>>];"
    end

  (** make_simulated_device is defined in subclasses, not here  *)

  (* Return the list of cables of which a port of self is an endpoint: *)
  method private get_involved_cables =
    List.filter (fun c->c#isNodeinvolved self#get_name) network#cables

  (** I need these even if such mechods are only useful for machines (and routers,
      in the future), because of type problems... This looks like the easiest way. *)
  method get_distrib : string =
    failwith ("The device " ^ self#get_name ^ " has not a type supporting distribution")

  method get_variant : string =
    failwith ("The device " ^ self#get_name ^ " has not a type supporting variants")

  method set_variant : string -> unit =
    failwith ("The device " ^ self#get_name ^ " has not a type supporting variants")
  (** Don't store the variant name as a symlink; resolve it if it's a symlink: *)

  method resolve_variant = 
    try (* Let's support this on *every* device; it's simpler... *)
      let variant = self#get_variant in
      (try
        let marionnet_home_filesystems = Pathnames.marionnet_home_filesystems in
        let prefixed_filesystem = "machine-" ^ self#get_distrib in
        let variant_pathname =
          marionnet_home_filesystems ^ "/" ^ prefixed_filesystem ^ "_variants/" ^ variant in
        let resolved_variant =
          Filename.basename (Unix.readlink variant_pathname) in
        self#set_variant resolved_variant;
        Printf.printf "The variant \"%s\" was a symlink. Resolved into \"%s\".\n" variant resolved_variant;
        flush_all ();
      with _ ->
        Printf.printf "The variant \"%s\" is not a symlink.\n" variant; flush_all ());
    with _ ->
      ();
end;;


(* *************************** *
        class cable
 * *************************** *) 

(** A cable defines an edge in the network graph. It is defined by two socketnames (mutables), i.e. two records
   in the form [ { nodename:string; receptname:string } ] representing the connection provided by the cable.
   Defects may be added after creation. *)
class cable =
   fun
   ~network
   ?(name="nocablename")
   ?(label="")
   ?(cablekind=Direct)
   ~(left :socketname)
   ~(right:socketname)
   () ->
  let dotoptions = new Dotoptions.cable () in
  let with_mutex mutex thunk =
(*     Printf.printf "I disabled synchronization here: BEGIN\n"; flush_all (); *)
    try
      let result = thunk () in
(*       Printf.printf "I disabled synchronization here: END\n"; flush_all (); *)
      result
    with e -> begin
      Printf.printf "I disabled synchronization here: RE-RAISING %s\n" (Printexc.to_string e); flush_all ();
      raise e;
    end
  in
object (self)
  inherit component ~network ~name ~label ()
  inherit simulated_device as super_simulated_device

  val mutable cablekind = cablekind
  method cablekind  =
    with_mutex mutex
      (fun () ->
        cablekind)
  method set_cablekind x =
    with_mutex mutex
      (fun () ->
        cablekind <- x)
  
  method dotoptions = dotoptions

  (** A cable may be either connected or disconnected; it's connected by default: *)
  val connected =
    ref true

  (** A cable has two connected socketnames. *)
  val mutable left  : socketname = left
  val mutable right : socketname = right

  (** Accessors *)
  method get_left    =
    with_mutex mutex
      (fun () -> left)
  method get_right   =
    with_mutex mutex
      (fun () -> right)
  method set_left  x =
    with_mutex mutex
      (fun () -> left  <- x)
  method set_right x =
    with_mutex mutex
      (fun () -> right <- x)

  (** The list of concerned socketnames *)
  method socketnames =
    with_mutex mutex
      (fun () -> 
        [self#get_left; self#get_right])
  
  (** Just a version of socketnames returning something easier to manage than a list *)
  method socketnames_as_pair = 
    with_mutex mutex
      (fun () -> 
        match self#socketnames with
          [ a; b ] ->
            a, b
        | _ ->
            assert false)
  
  (** The list of two names of nodes (machine/device) linked by the cable *)
  method involvedNodeNames =
    with_mutex mutex
      (fun () -> 
        [self#get_left.nodename; self#get_right.nodename])

  (** The pair of nodes (machine/device) linked by the cable *)
  method involvedNodes =
    with_mutex mutex
      (fun () -> 
        (network#getNodeByName self#get_left.nodename),
        (network#getNodeByName self#get_right.nodename))

  (** Return the list of devices (i.e. hubs, switches or routers) directly linked to this cable: *)
  method involved_devices_and_port_nos =
    with_mutex mutex
      (fun () -> 
        let left_port_name, right_port_name = self#receptacle_names in
        let left_port_index = (self#left_endpoint#getReceptacleByName left_port_name)#index in
        let right_port_index = (self#right_endpoint#getReceptacleByName right_port_name)#index in
        let left_node = 
          network#getNodeByName self#get_left.nodename in
        let right_node =
          network#getNodeByName self#get_right.nodename in
        let left_device =
          match left_node#devkind with
            NotADevice -> None
          | _ -> Some (left_node, left_port_index) in
        let right_device =
          match right_node#devkind with
            NotADevice -> None
          | _ -> Some (right_node, right_port_index) in
        match left_device, right_device with
          None, None ->     []
        | None, (Some d) -> [ d ]
        | (Some d), None -> [ d ]
        | (Some d1), (Some d2) -> [ d1; d2 ])

  (** Is a node connected to something with this cable? *)
  method isNodeinvolved nodename =
    with_mutex mutex
      (fun () -> 
        List.mem nodename self#involvedNodeNames)

  (** Change the left or right nodename if needed. *)
  method changeNodeName oldname newname = 
    with_mutex mutex
      (fun () -> 
        if left.nodename  = oldname then left.nodename  <- newname ;
        if right.nodename = oldname then right.nodename <- newname)

  (** Look for socktname in the left or right side of the cable and return the other socketname if exists *)
  method partnerOf (x:socketname) = 
    with_mutex mutex
      (fun () -> 
        if (x=self#get_left) then self#get_right 
        else 
          (if (x=self#get_right) then self#get_left else raise (Failure "partnerOf")))

  (** Show its definition. Useful for debugging. *)
  method show prefix =
    with_mutex mutex
      (fun () -> 
        prefix^self#name^" ("^(string_of_cablekind self#cablekind)^")"^
        " ["^self#get_left.nodename^","^self#get_left.receptname^"] -> "^
        " ["^self#get_right.nodename^","^self#get_right.receptname^"]")
                                                                      
  method to_forest = 
    with_mutex mutex
      (fun () -> 
        Forest.leaf ("cable",[
                     ("name"            ,  self#get_name )  ;
   		     ("label"           ,  self#get_label)  ;
                     ("kind"            , (string_of_cablekind self#cablekind)) ;
                     ("leftnodename"    ,  self#get_left.nodename)    ;
                     ("leftreceptname"  ,  self#get_left.receptname)  ;
                     ("rightnodename"   ,  self#get_right.nodename)   ;
                     ("rightreceptname" ,  self#get_right.receptname) ;
                   ]))

  (** A cable has just attributes (no childs) in this version. *)
  method eval_forest_attribute =
    function
      | ("name"            , x ) -> self#set_name  x 
      | ("label"           , x ) -> self#set_label x 
      | ("kind"            , x ) -> self#set_cablekind (cablekind_of_string x)
      | ("leftnodename"    , x ) -> self#get_left.nodename    <- x ;
      | ("leftreceptname"  , x ) -> self#get_left.receptname  <- x ;
      | ("rightnodename"   , x ) -> self#get_right.nodename   <- x ;
      | ("rightreceptname" , x ) -> self#get_right.receptname <- x ;
          
    (** Access method *)
    method is_connected =
      with_mutex mutex
        (fun () -> !connected);

    (** Make the cable connected, or do nothing if it's already connected: *)
    method private connect_right_now =
      with_mutex mutex
        (fun () -> 
          (if not self#is_connected then begin
            print_string ("Connecting the cable " ^ (self#get_name) ^ "\n");
            (* Turn on the relevant LEDgrid lights: *)
            let involved_devices_and_port_nos = self#involved_devices_and_port_nos in
            List.iter
              (fun (device, port_no) ->
                network#ledgrid_manager#set_port_connection_state
                  ~id:(device#id)
                  ~port:port_no
                  ~value:true
                  ())
              involved_devices_and_port_nos;
            connected := true;
            self#increment_alive_endpoints_no;
            print_string "Ok: connected\n";
          end);
          refresh_sketch ());

    (** Make the cable disconnected, or do nothing if it's already disconnected: *)
    method private disconnect_right_now =
      with_mutex mutex
        (fun () -> 
          (if self#is_connected then begin
            print_string ("Disconnecting the cable " ^ (self#get_name) ^ "\n");
            (* Turn off the relevant LEDgrid lights: *)
            let involved_devices_and_port_nos = self#involved_devices_and_port_nos in
            List.iter
              (fun (device, port_no) ->
                network#ledgrid_manager#set_port_connection_state
                  ~id:(device#id)
                  ~port:port_no
                  ~value:false
                  ())
              involved_devices_and_port_nos;
            connected := false;
            self#decrement_alive_endpoints_no;
            print_string "Ok: disconnected\n";
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
   val alive_endpoints_no = ref 1 (* cables are 'connected' by default *)
   
   (** Check that the reference counter is in [0, 3]. To do: disable this for
   production. *)
   method private check_alive_endpoints_no =
    with_mutex mutex
      (fun () -> 
        assert((!alive_endpoints_no >= 0) && (!alive_endpoints_no <= 3));
        print_string "The reference count is now ";
        print_int !alive_endpoints_no; print_string "\n")

   (** Record the fact that an endpoint has been created (at a lower level
       this means that its relevant {e hublet} has been created), and
       startup the simulated cable if appropriate. *)
   method increment_alive_endpoints_no =
     with_mutex mutex
       (fun () ->
         print_string "\n+++ increment_alive_endpoints_no\n\n";
         self#check_alive_endpoints_no;
         alive_endpoints_no := !alive_endpoints_no + 1;
         self#check_alive_endpoints_no;
         if !alive_endpoints_no = 3 then begin
           print_string "The reference count raised to three: starting up a cable\n";
           self#startup_right_now
         end)
   
   (** Record the fact that an endpoint is no longer running (at a lower level
       this means that its relevant {e hublet} has been destroyed), and
       shutdown the simulated cable if appropriate. *)
   method decrement_alive_endpoints_no =
     with_mutex mutex
       (fun () ->
         print_string "\n--- decrement_alive_endpoints_no\n\n";
         self#check_alive_endpoints_no;
         alive_endpoints_no := !alive_endpoints_no - 1;
         self#check_alive_endpoints_no;
         if !alive_endpoints_no < 3 then begin
           (* Note that we destroy rather than terminating. This enables to re-create the
              simulated device later, at startup time, referring the correct hublets
              that will exist then, rather than the ones existing now *)
           print_string "The reference count dropped below three: destroying a cable\n";
(*        self#destroy_right_now; (\* Before radical synchronization changes *\) *)
           self#destroy_right_now;
         end)
   
   (** Make a new simulated device according to the current status *)
   method private make_simulated_device =
     with_mutex mutex
       (fun () ->
         let left, right = self#get_left, self#get_right in
         let left_node = network#getNodeByName left.nodename in
         let right_node = network#getNodeByName right.nodename in
         let left_hublet_process =
           left_node#get_hublet_process_by_interface_name left.receptname in
         let right_hublet_process =
           right_node#get_hublet_process_by_interface_name right.receptname in
         let left_port_index =
           (left_node#getReceptacleByName left.receptname)#index in
         let right_port_index =
           (right_node#getReceptacleByName right.receptname)#index in
         let left_blink_command =
           match left_node#devkind with
             NotADevice -> None
           | _ -> Some (Printf.sprintf "%i %i" left_node#id left_port_index) in
         let right_blink_command =
           match right_node#devkind with
             NotADevice -> None
           | _ -> Some (Printf.sprintf "%i %i" right_node#id right_port_index) in
         print_string ("left hublet process socket name is " ^ left_hublet_process#get_socket_name ^ "\n");
         print_string ("right hublet process socket name is " ^ right_hublet_process#get_socket_name ^ "\n");
         let name = self#get_name in
         new Simulated_network.ethernet_cable
           ~name:self#get_name
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
     with_mutex mutex
       (fun () ->
         (* Refresh the process in some (ugly) way: *)
         if self#is_connected then begin
           (try self#disconnect_right_now with _ -> ());
           (try self#connect_right_now with _ -> ());
           connected := true;
         end else begin
           let current_alive_endpoints_no = !alive_endpoints_no in
           super_simulated_device#destroy_because_of_unexpected_death ();
           connected := true;
           alive_endpoints_no := 0;
           for i = 1 to current_alive_endpoints_no do
             self#increment_alive_endpoints_no;
           done
         end)

   (** To do: remove this ugly kludge, and make cables stoppable *)
   method can_startup = true (* To do: try reverting this *)
   method can_gracefully_shutdown = true (* To do: try reverting this *)
   method can_poweroff = true (* To do: try reverting this *)
   (** Only connected cables can be 'suspended' *)
   method can_suspend =
     with_mutex mutex
       (fun () -> !connected)
   (** Only non-connected cables with refcount exactly equal to 2 can be 'resumed' *)
   method can_resume =
     with_mutex mutex
       (fun () -> not !connected)

   (** A cable is never directly connected to any other cable *)
   method private get_involved_cables = []

   method receptacle_names =
    with_mutex mutex
      (fun () -> 
        match self#socketnames with [ left_port; right_port ] ->
          let left_receptacle_name = left_port.receptname in
          let right_receptacle_name = right_port.receptname in
          left_receptacle_name, right_receptacle_name
        | _ -> assert false)

   method left_receptacle_name =
    with_mutex mutex
      (fun () -> 
        let (left_receptacle_name, _) = self#receptacle_names in
        left_receptacle_name)

   method right_receptacle_name =
    with_mutex mutex
      (fun () -> 
        let (_, right_receptacle_name) = self#receptacle_names in
        right_receptacle_name)

   method endpoints : (node * node) =
    with_mutex mutex
      (fun () -> 
        let left_endpoint_name = self#get_left.nodename in
        let left_endpoint = network#getNodeByName left_endpoint_name in
        let right_endpoint_name = self#get_right.nodename in
        let right_endpoint = network#getNodeByName right_endpoint_name in
        left_endpoint, right_endpoint)

   method left_endpoint =
    with_mutex mutex
      (fun () -> 
        let (left_endpoint, _) = self#endpoints in
        left_endpoint)

   method right_endpoint =
    with_mutex mutex
      (fun () -> 
        let (_, right_endpoint) = self#endpoints in
        right_endpoint)

   (** Get the reference count right at the beginning: it starts at zero, but
       it's immediately incremented if endpoint hublet processes already
       exist: *)
   initializer
     let left_endpoint, right_endpoint = self#endpoints in
     self#set_correctness
       (network#would_a_cable_be_correct_between
          left_endpoint#name
          right_endpoint#name
          cablekind);
     (if left_endpoint#has_hublet_processes then
       self#increment_alive_endpoints_no);
     (if right_endpoint#has_hublet_processes then
       self#increment_alive_endpoints_no);
     print_string ("The reference count for the just-created cable " ^ (self#get_name)^" is ");
     print_int !alive_endpoints_no; print_string "\n";
end;;

(** Function for make receptacles from 0 to k (so call it with desired number - 1) of desired portkind 
    and with the given portprefix for name (for instance "eth"). *)
let rec mkrecepts network k portkind portprefix = match k with
| -1 -> []
| _ -> let r = new receptacle network (portprefix^(string_of_int k)) k nolabel portkind in
  (mkrecepts network (k-1) portkind portprefix)@[r] 
;;


(* *************************** *
        class device
 * *************************** *) 

let is_there_a_router_variant () =
  let result =
    List.mem "default" (MSys.variant_list_of ("router-" ^ router_unprefixed_filesystem) ())
  in
(*
  Printf.printf "is_there_a_router_variant (): %b\n" result; flush_all ();
  List.iter
    (fun s -> Printf.printf "* %s\n" s; flush_all ())
    (MSys.variant_list_of ("router-" ^ router_unprefixed_filesystem) ());
*)
  result;;

(** A device is a nodes s.t. nodekind=Device. Defects may be added after the creation. *)
class device =
  let variantsListOf d =
    MSys.variant_list_of ("router-" ^ d) () in
  let check_variant x d =
    if not (List.mem x (variantsListOf d)) then
      raise (Failure ("Setting router variant: "^x^" not in the list of available variants, which is:"^((variantsListOf d)=> String.Text.to_string)))
    else
      x in
 fun 
  ~network 
  ?(name="nodevicename") 
  ?(label="") 
  ?(devkind=Hub)
  ?(variant=no_variant_text)
  (ethnum:int) () -> 
  let intialReceptacles = (mkrecepts network (ethnum-1) Eth "port") in

  object (self)

  inherit node ~network ~name ~label ~nodekind:Device ~devkind ~rlist:intialReceptacles () as super

  (** See the comment in the 'node' class for the meaning of this method: *)
  method polarity = false

  (** Get the full host pathname to the directory containing the guest hostfs
      filesystem: *)
  method hostfs_directory_pathname =
    if self#devkind = Router then
      match !simulated_device with
        Some d ->
          (d :> Simulated_network.router)#hostfs_directory_pathname
      | None ->
          failwith (self#name ^ " is not being simulated right now")
    else
      failwith "You can't use call hostfs_directory_pathname on a non-router device"

  (** Dot adjustments *)

  (** Returns an image representig the device with the given iconsize. *)    
  method dotImg (z:iconsize) = 
    let imgDir = (MSys.whereis "" MSys.Icon) in
    (imgDir^"ico."^(string_of_devkind self#devkind)^"."^(self#string_of_simulated_device_state)^"."^z^".png")

  (** Returns the label to use for cable representation. 
      For devices, the port X is represented by the string "[X]". *)    
  method dotLabelForEdges (receptname:string) = 
    let index = (self#getReceptacleByName receptname)#index in
    ("["^string_of_int index^"]")

  (** Return the string representing the port in cable representation. *
      Ignore the receptname and returns the empty string. *)
  method dotPortForEdges (receptname:string) = ""

  method to_forest = 
   Forest.leaf ("device",[
  		  ("name" , self#get_name) ;
                  ("label", self#get_label);  
                  ("kind" , (string_of_devkind self#devkind)) ; 
                  ("eth"  , (string_of_int (self#get_eth_number)));
                  ])

  (** A cable has just attributes (no childs) in this version. *)
  method eval_forest_attribute = function
  | ("name"  , x ) -> self#set_name  x 
  | ("label" , x ) -> self#set_label x 
  | ("kind"  , x ) -> self#set_devkind (devkind_of_string x)  
  | ("eth"   , x ) -> self#set_eth_number (int_of_string x)

  (** Create the simulated device *)
  method private make_simulated_device =
    let id = self#id in
    let ethernet_receptacles = self#get_receptacles ~portkind:(Some Eth) () in
    if self#devkind != Router then
      let name = self#get_name in
      let hublets_no = List.length ethernet_receptacles in
      let unexpected_death_callback = self#destroy_because_of_unexpected_death in
      (match self#devkind with
        Hub -> new Simulated_network.hub 
      | Switch -> new Simulated_network.switch
      | _ -> assert false)
        ~name ~hublets_no ~unexpected_death_callback ()
    else begin
      let cow_file_name =
        (Filesystem_history.get_states_directory ()) ^
        (Filesystem_history.add_state_for_device self#name) in
      Printf.printf
        "About to start the router %s with the cow file %s\n"
        self#name
        cow_file_name;
      new Simulated_network.router
        ~name:self#get_name
        ~kernel_file_name:(MSys.marionnet_home_kernels ^ "linux-default")
        ~cow_file_name (* To do: use the variant!!! *)
        ~filesystem_file_name:(MSys.router_pathname_prefix^router_unprefixed_filesystem)
        ~ethernet_interfaces_no:(List.length ethernet_receptacles)
        ~umid:self#get_name
        ~id
        ~unexpected_death_callback:self#destroy_because_of_unexpected_death
        ()
    end

  (** Here we also have to manage LED grids: *)
  method private startup_right_now =
    (* Do as usual... *)
    super#startup_right_now;
    (* ...and also show the LED grid: *)
    network#ledgrid_manager#show_device_ledgrid ~id:(self#id) ();
(*
  (** Here we also have to manage LED grids and, for routers, cow files: *)
  method private gracefully_shutdown_right_now =
    (* Do as usual... *)
    super#gracefully_shutdown_right_now;
    (* ...and also hide the LED grid... *)
    network#ledgrid_manager#hide_device_ledgrid ~id:(self#id) ();
    (* ...and if this is a router then destroy, so that the next time we have to
       re-create a simulated device, and we start with a new cow *)
    if self#devkind = Router then
      self#destroy_right_now
*)
  (** Here we also have to manage cow files... *)
  method private gracefully_shutdown_right_now =
    (* Do as usual... *)
    super#gracefully_shutdown_right_now;
    (* ...and also hide the LED grid... *)
    network#ledgrid_manager#hide_device_ledgrid ~id:(self#id) ();
    (* Only for routers: we have to manage the hostfs stuff (when in exam mode) and
       destroy the simulated device, so that we can use a new cow file the next time: *)
    if self#devkind = Router then begin
      Printf.printf "Calling hostfs_directory_pathname on %s...\n" self#name; flush_all ();
      let hostfs_directory_pathname = self#hostfs_directory_pathname in
      Printf.printf "Ok, we're still alive\n"; flush_all ();
      (* If we're in exam mode then make the report available in the texts treeview: *)
      (if Command_line.are_we_in_exam_mode then begin
        let texts_interface = Texts_interface.get_texts_interface () in
        Printf.printf "Adding the report on %s to the texts interface\n" self#name; flush_all ();
        texts_interface#import_report 
          ~machine_or_router_name:self#name
          ~pathname:(hostfs_directory_pathname ^ "/report.html")
          ();
        Printf.printf "Added the report on %s to the texts interface\n" self#name; flush_all ();
        (* (* We don't export the shell history for routers *)
        Printf.printf "Adding the history on %s to the texts interface\n" self#name; flush_all ();
        texts_interface#import_history 
          ~machine_or_router_name:self#name
          ~pathname:(hostfs_directory_pathname ^ "/bash_history.text")
          ();
        Printf.printf "Added the history on %s to the texts interface\n" self#name; flush_all (); *)
      end);
      (* ...And destroy, so that the next time we have to re-create the process command line
         can use a new cow file (see the make_simulated_device method) *)
      self#destroy_right_now
    end;


  (** Here we also have to manage LED grids and, for routers, cow files: *)
  method private poweroff_right_now =
    (* Do as usual... *)
    super#poweroff_right_now;
    (* ...and also hide the LED grid... *)
    network#ledgrid_manager#hide_device_ledgrid ~id:(self#id) ();
    (* ...and if this is a router then destroy, so that the next time we have to
       re-create a simulated device, and we start with a new cow *)
    if self#devkind = Router then
      self#destroy_right_now

  (** A router has an associated initial variant *)
  val the_variant =
    ref (check_variant variant router_unprefixed_filesystem)

  method get_variant =
    if self#devkind = Router then
      !the_variant
    else 
      super#get_variant (* fail *)

  method set_variant x =
    if self#devkind = Router then
      the_variant := check_variant x router_unprefixed_filesystem
    else
      failwith "Can not set a variant on a non-router device"
end;;


(* *************************** *
        class machine
 * *************************** *) 

(** A machine is a node (a container of receptacles) s.t. 
    nodekind=Machine. Some receptacles are immediatly added 
    at creation time. *)
class machine = 

  fun ~network
      ?(name="nomachinename") 
      ?(label="") 
      ?(mem:int=48) 
      ?(ethnum:int=1) 
      ?(ttySnum:int=1) 
      ?(distr:string="default")
      ?(variant:string=no_variant_text)
      ?(ker:string="default") 
      ?(ter:string="X HOST") () -> 

  (* Some checks over machine parameters *)

  let check_mem x = if x<8 or x>1024 then 
    failwith ("Setting machine "^name^": value "^(string_of_int x)^" not in the memory range [8,1024]")  
    else x in

  let check_ethnum x = if x<1 or x>5 then 
    failwith ("Setting machine "^name^": value "^(string_of_int x)^" not in the eth's number range [1,5]")
    else x in

  let check_ttySnum x = if x<1 or x>3 then 
    failwith ("Setting machine "^name^": value "^(string_of_int x)^" not in the ttyS's number range [1,3]")
    else x in

  let check_distrib x = if (not (x = "default")) && not (List.mem x (MSys.machine_filesystem_list ())) then 
    failwith ("Setting machine "^name^": unknown GNU/Linux filesystem "^x)
    else x in

  let variantsListOf d =
    MSys.variant_list_of ("machine-" ^ d) () in

  let check_variant x d = if not (List.mem x (variantsListOf d)) then
    raise (Failure ("Setting machine "^name^": variant "^x^" not in the list of available variants, which is:"^((variantsListOf d)=> String.Text.to_string)))  
    else x in

  let check_kernel x = if not (List.mem x (MSys.kernelList ())) then 
    raise (Failure ("Setting machine "^name^": unknown kernel "^x))  
    else x in

  let check_term x = if not (List.mem x (MSys.termList)) then 
    raise (Failure ("Setting machine "^name^": unexpected uml terminal choice "^x))  
    else x in

  let intialReceptacles = 
   (mkrecepts network ((check_ethnum ethnum)-1) Eth "eth")@(mkrecepts network ((check_ttySnum ttySnum)-1) TtyS "ttyS") in

  object (self)
  
  inherit node ~network ~name ~label ~nodekind:Machine ~rlist:intialReceptacles () as super

  (** See the comment in the 'node' class for the meaning of this method: *)
  method polarity = true

  (** Get the full host pathname to the directory containing the guest hostfs
      filesystem: *)
  method hostfs_directory_pathname =
    match !simulated_device with
      Some d ->
        (d :> Simulated_network.machine)#hostfs_directory_pathname
    | None ->
        failwith (self#name ^ " is not being simulated right now")

  (** Redefining node methods for adding checks *)
  method set_eth_number ?(prefix="eth") x = self#readjust_receptacleNumber ~prefix:(Some prefix) Eth (check_ethnum  x) 
  method set_ttyS_number x = self#readjust_receptacleNumber TtyS (check_ttySnum x) 

  (** A machine will be started with a certain amount of memory *)
  val mutable memory : int = check_mem mem

  method get_memory   = memory
  method set_memory x = memory <- check_mem x

  (** A machine has a Linux filesystem *)
  val mutable distrib : string = check_distrib distr

  method get_distrib   = distrib
  method set_distrib x = distrib <- check_distrib x

  (** A machine has an associated initial variant *)
  val mutable variant : string = check_variant variant distr

  method get_variant   = variant
  method set_variant x = variant <- check_variant x distrib

  (** A machine has a linux kernel. *)
  val mutable kernel : string = check_kernel ker

  method get_kernel   = kernel
  method set_kernel x = kernel <- check_kernel x
  
  (** A machine can be used accessed in a specific terminal mode. *)
  val mutable terminal : string = check_term ter

  method get_terminal   = terminal
  method set_terminal x = terminal <- check_term x

  (** Show for debugging *)
  method show = name

  (** Return an image representing the machine with the given iconsize. *)    
  method dotImg (z:iconsize) = 
    let imgDir = (MSys.whereis "" MSys.Icon) in
    (imgDir^"ico.machine."^(self#string_of_simulated_device_state)^"."^z^".png") (* distinguer redhat,debian, etc ? *)

  (** Machine to forest encoding. *)
  method to_forest = 
   Forest.leaf ("machine", [
                   ("name"     ,  self#get_name ) ; 
                   ("memory"   ,  (string_of_int self#get_memory)) ;
                   ("distrib"  ,  self#get_distrib  ) ; 
                   ("variant"  ,  self#get_variant  ) ;  (* EX "state" attenzione nel from_forest *)
                   ("kernel"   ,  self#get_kernel   ) ;  
                   ("terminal" ,  self#get_terminal ) ; 
                   ("eth"      ,  (string_of_int self#get_eth_number))  ;
                   ("ttyS"     ,  (string_of_int self#get_ttyS_number)) ;
	           ])

 (** A machine has just attributes (no childs) in this version. *)
 method eval_forest_attribute = function
  | ("name"     , x ) -> self#set_name x 
  | ("memory"   , x ) -> self#set_memory (int_of_string x)
  | ("distrib"  , x ) -> self#set_distrib x 
  | ("variant"  , x ) -> self#set_variant x
  | ("kernel"   , x ) -> self#set_kernel x  
  | ("terminal" , x ) -> self#set_terminal x 
  | ("eth"      , x ) -> self#set_eth_number  (int_of_string x)
  | ("ttyS"     , x ) -> self#set_ttyS_number (int_of_string x)
  | _ -> () (* Forward-comp. *)


  (** Create the simulated device *)
  method private make_simulated_device =
    let id = self#id in
    let ethernet_receptacles = self#get_receptacles ~portkind:(Some Eth) () in
    let cow_file_name =
      (Filesystem_history.get_states_directory ()) ^
      (Filesystem_history.add_state_for_device self#name) in
    Printf.printf
      "About to start the machine %s with the cow file %s\n"
      self#name
      cow_file_name;
    new Simulated_network.machine
      ~name:self#get_name
      ~kernel_file_name:(MSys.kernel_pathname_prefix ^ kernel)
      ~filesystem_file_name:(MSys.machine_pathname_prefix^distrib)
      ~cow_file_name
      ~ethernet_interfaces_no:(List.length ethernet_receptacles)
      ~memory:self#get_memory
      ~umid:self#get_name
      ~id
      ~xnest:(match MSys.x_policy_of_string terminal with MSys.Xnest -> true | _ -> false)
      ~unexpected_death_callback:self#destroy_because_of_unexpected_death
      ()

  (** Here we also have to manage cow files... *)
  method private gracefully_shutdown_right_now =
    Printf.printf "Calling hostfs_directory_pathname on %s...\n" self#name; flush_all ();
    let hostfs_directory_pathname = self#hostfs_directory_pathname in
    Printf.printf "Ok, we're still alive\n"; flush_all ();
    (* Do as usual... *)
    super#gracefully_shutdown_right_now;
    (* If we're in exam mode then make the report available in the texts treeview: *)
    (if Command_line.are_we_in_exam_mode then begin
      let texts_interface = Texts_interface.get_texts_interface () in
      Printf.printf "Adding the report on %s to the texts interface\n" self#name; flush_all ();
      texts_interface#import_report 
        ~machine_or_router_name:self#name
        ~pathname:(hostfs_directory_pathname ^ "/report.html")
        ();
      Printf.printf "Added the report on %s to the texts interface\n" self#name; flush_all ();
      Printf.printf "Adding the history on %s to the texts interface\n" self#name; flush_all ();
      texts_interface#import_history 
        ~machine_or_router_name:self#name
        ~pathname:(hostfs_directory_pathname ^ "/bash_history.text")
        ();
      Printf.printf "Added the history on %s to the texts interface\n" self#name; flush_all ();
    end);
    (* ...And destroy, so that the next time we have to re-create the process command line
       can use a new cow file (see the make_simulated_device method) *)
    self#destroy_right_now

  (** Here we also have to manage cow files... *)
  method private poweroff_right_now =
    (* Do as usual... *)
    super#poweroff_right_now;
    (* ...And destroy, so that the next time we have to re-create the process command line
       can use a new cow file (see the make_simulated_device method) *)
    self#destroy_right_now

end;;


(* *************************** *
        class cloud
 * *************************** *) 

(** A cloud is an unknown network with some gateways. *)
class cloud =   
  fun ~network
      ?(name="nocloudname") 
      ?(label="") 
      () -> 

  let gwnum             = 2 in
  let intialReceptacles = (mkrecepts network (gwnum-1) Eth "port") in
 
  object (self)
  
  inherit node ~network ~name ~label ~nodekind:Cloud ~rlist:intialReceptacles ()

  (** See the comment in the 'node' class for the meaning of this method: *)
  method polarity = false

  method show = self#name

  method eth_number        = 2

  (** Dot adjustments *)

  (** Return an image representing the cloud with the given iconsize. *)    
  method dotImg (z:iconsize) = 
    let imgDir = (MSys.whereis "" MSys.Icon) in
    (imgDir^"ico.cloud."^(self#string_of_simulated_device_state)^"."^z^".png")

  (** Cloud endpoints are represented in the same way of devices ones, with "[X]". *)
  method dotLabelForEdges (receptname:string) = 
    let index = (self#getReceptacleByName receptname)#index in
    ("["^string_of_int index^"]")

  method dotPortForEdges (receptname:string) = ""

  method to_forest = 
   Forest.leaf ("cloud",[
  		 ("name",self#get_name);
  		 ])

 (** A cloud has just attributes (no childs) in this version. *)
 method eval_forest_attribute = function
  | ("name"     , x ) -> self#set_name x 

  (** Create the simulated device *)
  method private make_simulated_device =
    new Simulated_network.cloud
      ~name:self#get_name
      ~unexpected_death_callback:self#destroy_because_of_unexpected_death
(*       ~id *)
      ()
end;; (* cloud *)


(* *************************** *
        class gateway
 * *************************** *) 

(** An internet gateway is simply a node with a single receptacle and an associated IP number. *)
class gateway = 

  fun ~network
      ?(name="nogatewayname")
      ?(label="")
      () -> 

  let intialReceptacles = (mkrecepts network 0 Eth "eth") in

  object (self)
  
  inherit node ~network ~name ~label ~nodekind:Gateway ~rlist:intialReceptacles ()

  (** See the comment in the 'node' class for the meaning of this method: *)
  method polarity = false

  method show = (self#name^" (gateway)")

  (** Dot adjustments *)

  (** Return an image representing the machine with the given iconsize. *)    
  method dotImg (z:iconsize) = 
    let imgDir = (MSys.whereis "" MSys.Icon) in
    (imgDir^"ico.socket."^(self#string_of_simulated_device_state)^"."^z^".png")

  (** Returns the label to use for cable representation.*)    
  method dotLabelForEdges receptname = "" (* ip#toString *)

  (** Returns the port to use for cable representation. 
      Ignore the receptname and returns the empty string. *)    
  method dotPortForEdges receptname = ""

  method to_forest = 
   Forest.leaf ("gateway", [
                  ("name", self#get_name);
                  ])

  (** A gateway has just attributes (no childs) in this version. *)
  method eval_forest_attribute = function
  | ("name"     , x ) -> self#set_name x 

  (** Create the simulated device *)
  method private make_simulated_device =
    new Simulated_network.gateway
      ~name:self#get_name
      ~bridge_name:Global_options.ethernet_socket_bridge_name
      ~unexpected_death_callback:self#destroy_because_of_unexpected_death
      ()
end;; (* gwinternet *)


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
          let port_line      = (String.assemble "<TR><TD>" port "</TD></TR>") in
          let portlabel_line = (String.assemble "<TR><TD><FONT COLOR=\"#3a3936\">" portlabel "</FONT></TD></TR>") in
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
    let (n1,r1,n2,r2) = if c#dotoptions#inverted then (n2,r2,n1,r1) else (n1,r1,n2,r2) in
 
     match (n1#nodekind, c#cablekind, n2#nodekind, n1#get_label, n2#get_label) with

     |   _   ,    _   ,    _  , "", "" -> (n1#name^":img"), (n2#name^":img"), (vertexlab n1 "taillabel" r1), (vertexlab n2 "headlabel" r2) 
     |   _   ,    _   ,    _  , "", l2 -> (n1#name^":img"), (n2#name)       , (vertexlab n1 "taillabel" r1), (vertexlab n2 "headlabel" r2) 
     |   _   ,    _   ,    _  , l1, "" -> (n1#name)       , (n2#name^":img"), (vertexlab n1 "taillabel" r1), (vertexlab n2 "headlabel" r2) 
     |   _   ,    _   ,    _  , l1, l2 -> (n1#name)       , (n2#name)       , (vertexlab n1 "taillabel" r1), (vertexlab n2 "headlabel" r2) 

(* Now we treat all nodes in the same way (except the gateway)!!!!
     |   _   , Direct , Device, "", _  -> (n1#name^":img"), (n2#name^":img"), (vertexlab n1 "taillabel" r1), ("")  
     |   _   , Direct , Device, l1, _  -> (n1#name)       , (n2#name^":img"), (vertexlab n1 "taillabel" r1), ("") 

     | Device, Direct ,    _  , _ , "" -> (n1#name^":img"), (n2#name^":img"), ("")                         , (vertexlab n2 "headlabel" r2)  
     | Device, Direct ,    _  , _ , l2 -> (n1#name^":img"), (n2#name)       , ("")                         , (vertexlab n2 "headlabel" r2) 

     | Device, Crossed, Device, _ , _  -> (n1#name^":img"), (n2#name^":img"), ("")                         , ("")
     |   _   ,    _   ,    _  , "", "" -> (n1#name^":img"), (n2#name^":img"), (vertexlab n1 "taillabel" r1), (vertexlab n2 "headlabel" r2) 
     |   _   ,    _   ,    _  , "", l2 -> (n1#name^":img"), (n2#name)       , (vertexlab n1 "taillabel" r1), (vertexlab n2 "headlabel" r2) 
     |   _   ,    _   ,    _  , l1, "" -> (n1#name)       , (n2#name^":img"), (vertexlab n1 "taillabel" r1), (vertexlab n2 "headlabel" r2) 
     |   _   ,    _   ,    _  , l1, l2 -> (n1#name)       , (n2#name)       , (vertexlab n1 "taillabel" r1), (vertexlab n2 "headlabel" r2) 
 *)   in

   let edgeoptions = if edgeoptions = "" then "" else (edgeoptions^",") in
   let cable_label = c#get_name ^ (if c#get_label = "" then "" else ("  "^c#get_label)) in
   
   let edgeoptions = edgeoptions ^ "arrowhead=obox, arrowtail=obox, arrowsize=0.4," ^ 
    (if c#is_connected then ""
   		       else "style=dashed,") in

   let label_color = if c#cablekind = Direct then Color.direct_cable else Color.crossed_cable in
    
    (tail^" -> "^head^" ["^edgeoptions^labeldistance^"label=<<FONT COLOR=\""^label_color^"\">"^cable_label^"</FONT>>"^taillabel^headlabel^"];")
 
 end;; (* function dotEdgeTrad *)


end;; (* Module Edge *)


open Edge;;


(* *************************** *
        class network
 * *************************** *) 

(** Class modelling the virtual network *)
class network = fun () -> 

 object (self)
 inherit Xforest.interpreter ()

 val mutable machines : (machine list) = []
 val mutable devices  : (device  list) = []
 val mutable cables   : (cable   list) = []

 val mutable clouds   : (cloud   list) = []
 val mutable gateways : (gateway list) = []

 val ledgrid_manager = Ledgrid_manager.the_one_and_only_ledgrid_manager

 (** Accessors *) 

 method machines        = machines
 method devices         = devices
 method cables          = cables
 method clouds          = clouds
 method gateways        = gateways
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
    (gateways :> component list) @
    (cables   :> component list)    (* CABLES MUST BE AT THE FINAL POSITION for marshaling !!!! *) 
    )

 (** Setter *)

 method reset () = 
   print_string "reset: begin\n";
   print_string "resetting the LEDgrid manager...\n";
   print_string "destroying all cables...\n";
   (List.iter
      (fun cable -> try cable#destroy with _ -> ()) (* "right_now" was here before the recent radical synchronization changes*)
      cables);
   print_string "destroying all machines...\n";
   (List.iter
      (fun machine -> try machine#destroy with _ -> ()) (* "right_now" was here before the recent radical synchronization changes*)
      machines);
   print_string "destroying all switchs, hubs and routers...\n";
   (List.iter
      (fun device -> try device#destroy with _ -> ()) (* "right_now" was here before the recent radical synchronization changes*)
      devices);
   print_string "destroying all clouds...\n";
   (List.iter
      (fun cloud -> try cloud#destroy with _ -> ()) (* "right_now" was here before the recent radical synchronization changes*)
      clouds);
   print_string "destroying all gateways...\n";
   (List.iter
      (fun gateway -> try gateway#destroy with _ -> ()) (* "right_now" was here before the recent radical synchronization changes*)
      gateways);
   print_string "Synchronously wait that everything terminates...\n";
   Task_runner.the_task_runner#wait_for_all_currently_scheduled_tasks;

   print_string "Making the network graph empty...\n";
   machines <- [] ;
   devices  <- [] ;
   clouds   <- [] ;
   gateways <- [] ;
   cables   <- [] ;

   print_string "Wait for all devices to terminate...\n"; flush_all ();
   (** Make sure that all devices have actually been terminated before going
       on: we don't want them to lose filesystem access: *)
   print_string "All devices did terminate.\n"; flush_all ();

   print_string "reset: end (success)\n";

 method copyFrom (net:network) = 
   self#reset ();
   machines <- net#machines ;
   devices  <- net#devices  ;
   clouds   <- net#clouds   ;
   gateways <- net#gateways ;
   cables   <- net#cables   

 method to_forest = 
   let l = List.map (fun x->x#to_forest) self#components in
   Forest.tree ("network",[]) (Forest.of_treelist l)

 (** We redefine just the interpretation of a childs.
     We ignore (in this version) network attributes. *)
 method eval_forest_child (f:Xforest.tree) : unit = try begin 
  match f with
  | Forest.NonEmpty (("machine", attrs) , childs , Forest.Empty) -> 
      let x = new machine ~network:self () in
      x#from_forest ("machine", attrs) childs ;
      self#addMachine x

 | Forest.NonEmpty (("device", attrs) , childs , Forest.Empty) -> 
      let ethno = List.assoc "eth" attrs in
      let x = new device ~network:self (int_of_string ethno) () in
      x#from_forest ("device", attrs) childs ;
      self#addDevice x 

 | Forest.NonEmpty (("cloud", attrs) , childs , Forest.Empty) -> 
      let x = new cloud ~network:self () in
      x#from_forest ("cloud", attrs) childs ;
      self#addCloud x; 

 | Forest.NonEmpty (("gateway", attrs) , childs , Forest.Empty) -> 
      let x = new gateway ~network:self () in
      x#from_forest ("gateway", attrs) childs  ;
      self#addGateway x; 

 | Forest.NonEmpty (("cable", attrs) , childs , Forest.Empty) -> 
      (* Cables represent a special case: they must be builded knowing their endpoints. *)
      let ln = List.assoc "leftnodename"    attrs in
      let lr = List.assoc "leftreceptname"  attrs in
      let rn = List.assoc "rightnodename"   attrs in
      let rr = List.assoc "rightreceptname" attrs in
      let left  = { nodename=ln; receptname=lr }  in
      let right = { nodename=rn; receptname=rr }  in
      let x = new cable ~network:self ~left ~right () in
      x#from_forest ("cable", attrs) childs ;
      self#addCable x; 

  | Forest.NonEmpty ((nodename, _) , _ , _)
     -> (Printf.printf "network#eval_forest_child: I can't interpret this nodename '%s'.\n" nodename) 
        (* Forward-compatibility *)
  
  | Forest.Empty 
     -> (Printf.printf "network#eval_forest_child: I can't interpret the empty forest.\n") 
        (* Forward-compatibility *)
 end
 with e  -> 
   Printf.printf "network#eval_forest_child: something goes wrong interpreting a child (%s).\n" 
   (Printexc.to_string e); () (* Forward-compatibility *)

 (* Destruct the x-value into a concrete xml string *)
(* method xml : string = 
    let trad = ref "" in 
    let update x = trad:=!trad ^ x in 
    let _ = Ocamlduce.Print.print_xml update self#xvalue in
    let s = Xml.parse_string !trad in
    Printf.sprintf "%s" (Xml.to_string_fmt s)*)

 method nodes : (node list) = 
   ((machines :> node list) @ (devices  :> node list) @ 
    (clouds   :> node list) @ (gateways :> node list) )

 method names = (List.map (fun x->x#name) self#components)

 method suggestedName prefix = 
   let rec tip prefix k = 
     begin
     let prop = (prefix^(string_of_int k)) in 
     if self#nameExists prop then tip prefix (k+1) else prop 
     end in tip prefix 1

 
 (** Get machine, device or cable in the network by its name *)
 method getMachineByName n = try List.find (fun x->x#name=n) machines   with _ -> failwith ("getMachineByName "^n)
 method getDeviceByName  n = try List.find (fun x->x#name=n) devices    with _ -> failwith ("getDeviceByName "^n)
 method getCableByName   n = try List.find (fun x->x#name=n) cables     with _ -> failwith ("getCableByName "^n)
 method getCloudByName   n = try List.find (fun x->x#name=n) clouds     with _ -> failwith ("getCloudByName "^n)
 method getGatewayByName n = try List.find (fun x->x#name=n) gateways   with _ -> failwith ("getGatewayByName "^n)
 method getNodeByName    n = try List.find (fun x->x#name=n) self#nodes with _ -> failwith ("getNodeByName \""^n^"\"")

 (** Managing socketnames *)

 (** List of socketnames involved in the network (in its nodes) *)
 method socketnames = List.flatten (List.map (fun node->node#socketnames) (self#nodes))

 (** List of socketnames busy by a cable in the network *)
 method busySocketnames = List.flatten (List.map (fun c->c#socketnames) cables)

 (** List of available socketnames in the network *)
 method freeSocketnames = List.substract self#socketnames self#busySocketnames

 (** Is a socketname available for connection? *)
 method isSocketnameFree x = List.mem x self#freeSocketnames

 (** Managing receptacle of a given node *)

 (** List of receptacles of a node and of a given kind. *)
 method receptaclesOfNode name (k:portkind) = 
     (self#getNodeByName name)#get_receptacles ~portkind:(Some k) ()

 (** List of receptacles names of a node and of a given kind. *)
 method receptaclesNamesOfNode name (k:portkind) = 
     (self#getNodeByName name)#receptacleNames ~portkind:(Some k) ()

 (** List of free receptacles of a nodename and of a given portkind. *)
 method freeReceptaclesOfNode nodename portkind = 
   let recepts = (self#receptaclesOfNode nodename portkind) in
   let freeSocketnames  = self#freeSocketnames in
   List.filter (fun r ->(List.mem {nodename=nodename;receptname=r#name} freeSocketnames)) recepts

 (** List of free receptacles names of a nodename and of a given portkind. *)
 method freeReceptaclesNamesOfNode nodename portkind = 
   let recept_names   = (self#receptaclesNamesOfNode nodename portkind) in
   let freeSocketnames  = self#freeSocketnames in
   List.filter (fun r ->(List.mem {nodename=nodename;receptname=r} freeSocketnames)) recept_names 

 (** List of busy receptacles of a nodename and of a given portkind. *)
 method busyReceptaclesOfNode nodename portkind = 
   let recepts = (self#receptaclesOfNode nodename portkind) in
   let freeSocketnames  = self#freeSocketnames in
   List.filter (fun r ->(not (List.mem {nodename=nodename;receptname=r#name} freeSocketnames))) recepts

 (** List of busy receptacles names of a nodename and of a given portkind. *)
 method busyReceptaclesNamesOfNode nodename portkind = 
   let recept_names   = (self#receptaclesNamesOfNode nodename portkind) in
   let freeSocketnames  = self#freeSocketnames in
   List.filter (fun r ->not (List.mem {nodename=nodename;receptname=r} freeSocketnames)) recept_names 

 (** The max index among busy receptacles of a given kind of a given node. 
     The user cannot change the number of receptacle of the given node to a number less than this index+1. 
     For instance, if the (maxBusyReceptacleIndex "rome" Eth) = 2 then the user can change
     the number of receptacle of rome but only with a number >= 3.  *)
 method maxBusyReceptacleIndex nodename portkind : int =
   let bl = (self#busyReceptaclesOfNode nodename portkind) in
   let il = (List.map (fun x->x#index) bl) in
   if il=[] then -1 else List.max il

 (** Component exists? *)

 method machineExists n = let f=(fun x->x#name=n) in (List.exists f machines)
 method deviceExists  n = let f=(fun x->x#name=n) in (List.exists f devices )
 method cableExists   n = let f=(fun x->x#name=n) in (List.exists f cables  )
 method cloudExists   n = let f=(fun x->x#name=n) in (List.exists f clouds  )
 method gatewayExists n = let f=(fun x->x#name=n) in (List.exists f gateways)
 method nameExists    n = List.mem n self#names

 (** What kind of node is it ? *)
 method nodeKind n = (self#getNodeByName n)#nodekind

 (** Adding components *)
     
   method make_device_ledgrid (d:device) =
     (* Make a new device LEDgrid: *)
     self#ledgrid_manager#make_device_ledgrid
       ~id:(d#id)
       ~title:(d#get_name(*  ^ " (" ^(string_of_devkind d#devkind) ^ ")" *))
       ~label:(match d#devkind with
                 Hub -> "Hub"
               | Switch -> "Switch"
               | Router -> "Router"
               | _ -> assert false)
       ~ports_no:(d#numberOfReceptacles ~portkind:(Some Eth) ())
       ~image_directory:(MSys.marionnet_home_images^"/leds/"^(string_of_devkind d#devkind))
       ();
     (* Set port connection state: *)
     let busy_ports =
       self#busyReceptaclesOfNode d#get_name Eth in
     let busy_ports_as_indices =
       List.map (fun receptacle -> receptacle#index) busy_ports in
     ignore (List.map
               (fun port_index -> (self#ledgrid_manager#set_port_connection_state
                                     ~id:d#id
                                     ~port:port_index
                                     ~value:true
                                     ()))
               busy_ports_as_indices)     
       
 (** Devices must have a unique name in the network *)
 method addDevice (d:device) = 
    if (self#nameExists d#name) then
      raise (Failure "addDevice : name already used in the network")
    else begin
      devices  <- (devices@[d]);
      self#make_device_ledgrid d;
      if (d#devkind = Router) &&
         ((Filesystem_history.number_of_states_with_name d#name) = 0) then (* not after load *)
        Filesystem_history.add_device
          d#name
          (MSys.router_prefix ^ router_unprefixed_filesystem)
          d#get_variant
          "router"
    end

 (** Machines must have a unique name in the network *)
 method addMachine (m:machine) = 
   if (self#nameExists m#name) then
     raise (Failure ("addMachine : name "^m#name^" already used in the network"))
   else begin
     (* Ok, we have fixed the variant name. Now just prepend the machine to the
        appropriate list: *)
     machines  <- (machines@[m]);
   end
 (** Cable must connect free socketnames  *)
 method addCable (c:cable) = 
    if (self#nameExists c#name) 
    then raise (Failure "addCable : name already used in the network")
    else 
      if (self#isSocketnameFree c#get_left) && (self#isSocketnameFree c#get_right)
      then begin
        cables  <- (cables@[c]);
        (* If at least one endpoint is a device then set the port state to connected in
           the appropriate LED grid: *)
        let left_endpoint, right_endpoint = c#endpoints in
        let left_port_name, right_port_name = c#receptacle_names in
        let left_port_index = (left_endpoint#getReceptacleByName left_port_name)#index in
        let right_port_index = (right_endpoint#getReceptacleByName right_port_name)#index in
(*         print_string ("\nLeft side: " ^ left_port_name ^ "\n Right side: "^right_port_name^"\n"); *)
        if left_endpoint#devkind != NotADevice then
          self#ledgrid_manager#set_port_connection_state
            ~id:(left_endpoint#id)
            ~port:left_port_index
            ~value:true
            ();
        if right_endpoint#devkind != NotADevice then
          self#ledgrid_manager#set_port_connection_state
            ~id:(right_endpoint#id)
            ~port:right_port_index
            ~value:true
            ();
      end else
        raise (Failure ("addCable : left and/or right socketname busy or nonexistent for "^(c#show "")))

 (** Clouds must have a unique name in the network *)
 method addCloud (c:cloud) = 
    if (self#nameExists c#name) 
          then raise (Failure ("addCloud : name "^c#name^" already used in the network"))
          else clouds  <- (clouds@[c]) 

 (** Gateways must have a unique name in the network *)
 method addGateway (g:gateway) = 
    if (self#nameExists g#name) 
          then raise (Failure ("addGateway : name "^g#name^" already used in the network"))
          else gateways  <- (gateways@[g]) 


 (** Removing components *)

 (** Remove a machine from the network. Remove it from the [machines] list and remove all related cables *)
 method delMachine mname = 
     let m  = self#getMachineByName mname in
     (* Destroy cables first, from the network and from the defects treeview (cables are not
        in the network details treeview): they refer what we're removing... *)
     let cables_to_remove = List.filter (fun c->c#isNodeinvolved mname) cables in
     let defects = Defects_interface.get_defects_interface () in
     List.iter
       (fun cable ->
         defects#remove_cable cable#name;
         self#delCable cable#name)
       cables_to_remove;
     Filesystem_history.remove_device_tree mname;
     m#destroy;(*      m#destroy_right_now; *) (* This was here before the radical synchronization changes *)
     machines  <- List.filter (fun x->not (x=m)) machines

 (** Remove a device from the network. Remove it from the [devices] list and remove all related cables *)
 method delDevice dname = 
     let d  = self#getDeviceByName dname in
     (* Destroy cables first: they refer what we're removing... *)
     let cables_to_remove = List.filter (fun c->c#isNodeinvolved dname) cables in
     let defects = Defects_interface.get_defects_interface () in
     List.iter
       (fun cable ->
         defects#remove_cable cable#name;
         self#delCable cable#name)
       cables_to_remove;
     (if d#devkind = Router then
       Filesystem_history.remove_device_tree dname);
     d#destroy;(*      d#destroy_right_now; *) (* This was here before the radical synchronization changes *)
     devices  <- List.filter (fun x->not (x=d)) devices

 (** Remove a cable from network *)
 method delCable cname =
     let c = self#getCableByName cname in
(*      cables  <- (cables@[c]); *) (* No, this was a cut/paste error, I think. *)
     (* If at least one endpoint is a device then set the port state to disconnected in
        the appropriate LED grid: *)
     let left_endpoint, right_endpoint = c#endpoints in
     let left_port_name, right_port_name = c#receptacle_names in
     let left_port_index = (left_endpoint#getReceptacleByName left_port_name)#index in
     let right_port_index = (right_endpoint#getReceptacleByName right_port_name)#index in
(*         print_string ("\nLeft side: " ^ left_port_name ^ "\n Right side: "^right_port_name^"\n"); *)
     (if left_endpoint#devkind != NotADevice then
       self#ledgrid_manager#set_port_connection_state
         ~id:(left_endpoint#id)
         ~port:left_port_index
         ~value:false
         ());
     (if right_endpoint#devkind != NotADevice then
       self#ledgrid_manager#set_port_connection_state
         ~id:(right_endpoint#id)
         ~port:right_port_index
         ~value:false
         ());
     cables <- (List.filter (fun x->not (x=c)) cables);
(* To do: c#destroy_right_now was at the beginning, but I think it should be here. *)
     c#destroy;(*      c#destroy_right_now; *) (* This was here before the radical synchronization changes *)
     (* Remove the cable also from the defects interface (cables are not in the network
        details interface): *)
     (* (* This should not be needed; it's actually harmful *)
     (try
       let defects = Defects_interface.get_defects_interface () in
       defects#remove_cable cname
     with _ ->
       ()); *)

 (** Remove a cloud from the network. Remove it from the [clouds] list and remove all related cables *)
 method delCloud clname = 
     let cl  = self#getCloudByName clname in
     (* Destroy cables first: they refer what we're removing... *)
     let cables_to_remove = List.filter (fun c->c#isNodeinvolved clname) cables in
     let defects = Defects_interface.get_defects_interface () in
     List.iter
       (fun cable ->
         defects#remove_cable cable#name;
         self#delCable cable#name)
       cables_to_remove;
     cl#destroy;(*      cl#destroy_right_now; *) (* This was here before the radical synchronization changes *)
     clouds  <- List.filter (fun x->not (x=cl)) clouds

 (** Remove a gateway from the network. Remove it from the [gateways] list and remove all related cables *)
 method delGateway gname = 
     let g  = self#getGatewayByName gname in
     (* Destroy cables first: they refer what we're removing... *)
     let cables_to_remove = List.filter (fun c->c#isNodeinvolved gname) cables in
     let defects = Defects_interface.get_defects_interface () in
     List.iter
       (fun cable ->
         defects#remove_cable cable#name;
         self#delCable cable#name)
       cables_to_remove;
     g#destroy;(*      g#destroy_right_now; *) (* This was here before the radical synchronization changes *)
     gateways  <- List.filter (fun x->not (x=g)) gateways
 
 (** Change the name of a node => change all cable socketnames refering this node *)
 method changeNodeName oldname newname = 
   if oldname = newname then () else
   let node = self#getNodeByName oldname in 
   node#set_name newname ;
   List.foreach cables (fun c->c#changeNodeName oldname newname)
   
 (** Facilities *)

  (** Would a hypothetical cable of a given crossedness be 'correct' if it connected the two
      given nodes? *)
  method would_a_cable_be_correct_between endpoint1_name endpoint2_name crossedness =
    let polarity1 = (self#getNodeByName endpoint1_name)#polarity in
    let polarity2 = (self#getNodeByName endpoint2_name)#polarity in
    (* We need a crossover cable iff the polarity is the same: *)
    if polarity1 = polarity2 then
      crossedness = Crossed
    else
      crossedness = Direct

 (** List of machines names in the network *)
 method getMachineNames     = 
   List.map (fun x->x#name) machines

 (** List of device names in the network *)
 method getDeviceNames     = 
   List.map (fun x->x#name) devices

 (** List of node names in the network *)
 method getNodeNames  = 
   List.map (fun x->x#name) (self#nodes)

 (** List of hub names in the network *)
 method getHubNames     = 
   let hublist= List.filter (fun x->x#devkind=Hub) devices in
   List.map (fun x->x#name) hublist

 (** List of switch names in the network *)
 method getSwitchNames  = 
   let swlist= List.filter (fun x->x#devkind=Switch) devices in
   List.map (fun x->x#name) swlist

 (** List of router names in the network *)
 method getRouterNames  = 
   let routerlist= List.filter (fun x->x#devkind=Router) devices in
   List.map (fun x->x#name) routerlist

 (** List of direct cable names in the network *)
 method getDirectCableNames  = 
   let clist= List.filter (fun x->x#cablekind=Direct) cables in
   List.map (fun x->x#name) clist

 (** List of crossed cable names in the network *)
 method getCrossedCableNames  = 
   let clist= List.filter (fun x->x#cablekind=Crossed) cables in
   List.map (fun x->x#name) clist

 (** List of serial cable names in the network *)
 method getSerialCableNames  = 
   let clist= List.filter (fun x->x#cablekind=NullModem) cables in
   List.map (fun x->x#name) clist

 (** List of cloud names in the network *)
 method getCloudNames     = 
   List.map (fun x->x#name) clouds

 (** List of gateway names in the network *)
 method getGatewayNames     = 
   List.map (fun x->x#name) gateways

 (** Starting and showing the network *)

 (** List of inverted cables (used only for drawing network) *)
 method invertedCables : (string list) = 
   let clist= List.filter (fun x->x#dotoptions#inverted) cables in
   List.map (fun x->x#name) clist

 (** Set the inverted dotoptions field of a cable of the network (identified by name) *)
 method invertedCableSet (x:bool) (cname:string) = 
   (self#getCableByName cname)#dotoptions#set_inverted x

 (** Toggle the inverted dotoptions field of a cable of the network (identified by name) *)
 method invertedCableToggle (cname:string) = 
   let c = (self#getCableByName cname) in c#dotoptions#set_inverted (not c#dotoptions#inverted)

 
 (* The network start commnand *)
 (*method startCmd = String.synthesis (^) (List.map (fun m->m#startCmd) machines) *)

 (** Show network topology *)
 method show = 
   prerr_endline "========== NETWORK STATUS ===========";
   (* show devices *)
   let msg= try 
        (String.Fold.commacat 
        (List.map (fun d->d#name^" ("^(string_of_devkind d#devkind)^")") devices))
        with _ -> "" 
     in prerr_endline ("Devices \r\t\t: "^msg);
   (* show machines *)
   let msg=try
        (String.Fold.commacat (List.map (fun m->m#show) machines)) 
        with _ -> ""
   in prerr_endline ("Machines \r\t\t: "^msg);
   (* show clouds *)
   let msg=try
        (String.Fold.commacat (List.map (fun c->c#show) clouds)) 
        with _ -> ""
   in prerr_endline ("Clouds \r\t\t: "^msg);
   (* show gateways *)
   let msg=try
        (String.Fold.commacat (List.map (fun c->c#show) gateways)) 
        with _ -> ""
   in prerr_endline ("Gateways \r\t\t: "^msg);
   (* show links *)
   let msg=try
        (String.Fold.newlinecat (List.map (fun c->(c#show "\r\t\t  ")) cables)) 
        with _ -> ""
   in prerr_endline ("Cables \r\t\t: "^msg)


 (** {b Consider cable as Edge.edges} *)
 
 (** Gives the list of edges which left and right side verify the given conditions. 
     An edge is a 3-uple (node1,receptname1), cable , (node2,receptname2) *)
 method edgesSuchThat (lpred: node->bool) (cpred:cable->bool) (rpred: node->bool) : edge list =
   let cable_info c = 
       begin 
       let (l,r) = (c#get_left,c#get_right) in 
       let a = ((self#getNodeByName l.nodename), l.receptname) in
       let b = ((self#getNodeByName r.nodename), r.receptname) in (a,c,b)
       end in
   let l1 = List.map cable_info cables in   
   let filter = fun ((n1,_),c,(n2,_)) -> (lpred n1) && (cpred c) && (rpred n2) in
   let l2 = (List.filter filter l1) in l2

 (** Gives the list of edges of a given kind. *)
 method edgesOfKind (lnkind: nodekind option) (ckind: cablekind option) (rnkind: nodekind option) =
   let npred nkind = fun n -> match nkind with None -> true | Some nk -> (n#nodekind  = nk) in
   let cpred kind  = fun c -> match kind  with None -> true | Some ck -> (c#cablekind = ck) in
   self#edgesSuchThat (npred lnkind) (cpred ckind) (npred rnkind)  


 (** Gives the list of all edges in the network. *)
 method edges : edge list = self#edgesSuchThat (fun ln->true) (fun c->true) (fun rn->true)

(* ex colore dei cavi incrociati "#52504c" *)

 (** Network translation into the dot language *)
 method dotTrad () = let opt = self#dotoptions in
 begin
"digraph plan {

"^opt#ratio^"
"^opt#get_rankdir^"
"^opt#get_nodesep^";"^"

/* ***************
        NODES 
   *************** */

"^ 
(String.Text.to_string 
   (List.map 
     (fun (n:node)->n#dotTrad opt#get_iconsize) 
     (List.permute opt#get_shuffler self#nodes)
   ))
^"
/* ***********************
      DIRECT CABLE EDGES 
   *********************** */


edge [dir=none,color=\""^Color.direct_cable^"\",fontsize=8,labelfontsize=8,minlen=1.6,"^
opt#get_labeldistance^",tailclip=true];

"^
(String.Text.to_string 
   (List.map (dotEdgeTrad opt#labeldistance) (self#edgesOfKind None (Some Direct) None)))

^"
/* *********************************
      CROSSED/SERIAL CABLE EDGES 
   ********************************* */


edge [headclip=true,minlen=1.6,color=\""^Color.crossed_cable^"\",weight=1];

"^ 
(String.Text.to_string 
   (List.map (dotEdgeTrad opt#labeldistance) (self#edgesOfKind None (Some Crossed) None)))

^
(String.Text.to_string 
   (List.map 
      (dotEdgeTrad ~edgeoptions:"style=bold" opt#labeldistance) 
      (self#edgesOfKind None (Some NullModem) None)
   ))

^"} //END of digraph\n"

 end (* method dotTrad *)

 
  
end


(** {2 Simplified constructors } *)

(** Simplified machine constructor *)
let newMachine ~network name = new machine ~name ~ter:(MSys.string_of_x_policy MSys.HostXServer) ();; 

(** Simplified hub constructor (by default 8 ports). *)
let newHub ~network name = new device ~network ~name ~devkind:Hub 8 ();;

(** Simplified switch constructor (by default 8 ports). *)
let newSwitch ~network name = new device ~network ~name ~devkind:Switch 8 ();;

(** Simplified router constructor (by default 8 ports). *)
let newRouter ~network name = new device ~network ~name ~devkind:Router 8 ();;

(** Simplified direct cable constructor. 

    {[ Example: newDirectCable "dc1" ("rome","eth0") ("A","port0") ]}   *)
let newDirectCable ~network name ((m1,r1):(nodename*receptname)) ((m2,r2):(nodename*receptname)) = 
  new cable ~network ~name ~cablekind:Direct ~left:{nodename=m1;receptname=r1} ~right:{nodename=m2;receptname=r2} () 

(** Simplified crossed cable constructor. 

    {[ Example: newCrossedCable "cc1" ("rome","eth0") ("paris","eth1") ]}  *)
let newCrossedCable ~network name ((m1,r1):(nodename*receptname)) ((m2,r2):(nodename*receptname)) =
  new cable ~network ~name ~cablekind:Crossed ~left:{nodename=m1;receptname=r1} ~right:{nodename=m2;receptname=r2} () 

(** Simplified serial cable constructor. 

    {[ Example: newSerialCable "sc1" ("rome","ttyS0") ("paris","ttyS0") ]} *)
let newSerialCable ~network name ((m1,r1):(nodename*receptname)) ((m2,r2):(nodename*receptname)) =
  new cable ~network ~name ~cablekind:NullModem ~left:{nodename=m1;receptname=r1} ~right:{nodename=m2;receptname=r2} () 

(** Simplified cloud with defect parameters. *)
let newCloud ~network name = new cloud ~network ~name ;; 


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
 ;;

(** Save the xforest representation of the network. *)
let save_network (net:network) (fname:string) = 
 print_string "Netmodel.Xml.save_network: begin\n";
 Xforest.print_forest net#to_forest;
 network_marshaller#to_file net#to_forest fname;
 print_string "Netmodel.Xml.save_network: end (success)\n";;

end;; (* module Netmodel.Xml *)


end;; (* module Netmodel *)
