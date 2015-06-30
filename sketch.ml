(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2015  Jean-Vincent Loddo
   Copyright (C) 2015  Université Paris 13

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

(** Sketch related modules and classes *)

open Gettext;;
module Recursive_mutex = MutexExtra.Recursive ;;

(** A thunk allowing to invoke the sketch refresh method, accessible from many
    modules: *)
module Refresh_sketch_thunk = Stateful_modules.Variable (struct
  type t = unit->unit
  let name = Some "Refresh_sketch_thunk"
  end)
let refresh_sketch () = Refresh_sketch_thunk.extract () ()

type index    = int         (* 0..(length-1) *)
 and shuffler = index list  (* represents a permutation of indexes of a list *)

(* This part of the state will be filled loading Gui_toolbar_DOT_TUNING. *)
class type toolbar_driver =
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
end (* class toolbar_driver *)

(** Dot options for the network sketch: *)
let network_marshaller = new Oomarshal.marshaller;;

class tuning 
  ?(iconsize="large") 
  ?(shuffler=[]) 
  ?(rankdir="TB") 
  ?(nodesep=0.5) 
  ?(labeldistance=1.6) 
  ?(extrasize=0.) 
  ?(curved_lines=false)
  ~(network: < reversed_cables:(string list); reversed_cable_set:(bool->string->unit); .. >)  (* The handler for the real network *) 
  ()
  =
  let iconsize_default      = iconsize in
  let shuffler_default      = shuffler in
  let rankdir_default       = rankdir  in
  let nodesep_default       = nodesep  in
  let labeldistance_default = labeldistance in
  let curved_lines_default  = curved_lines in
  let extrasize_default     = extrasize in
  object (self)
  inherit Xforest.interpreter ()
   
  method direct_cable_color    = "#949494"
  method crossover_cable_color = "#6d8dc0"

  val iconsize = Cortex.return (iconsize)
  method iconsize = iconsize

  val rankdir  = Cortex.return (rankdir)
  method rankdir = rankdir

  val curved_lines = Cortex.return curved_lines
  method curved_lines = curved_lines
  method curved_lines_commute = fst (Cortex.move (curved_lines) (not))

  val shuffler = Cortex.return shuffler
  method shuffler = shuffler
  method shuffler_reset = Cortex.set shuffler (shuffler_default)
  method shuffler_as_function = Cortex.apply shuffler ListExtra.asFunction (* returns the permutation function *)

  val nodesep = Cortex.return nodesep
  method nodesep = nodesep

  val labeldistance = Cortex.return labeldistance
  method labeldistance = labeldistance

  val extrasize = Cortex.return extrasize
  method extrasize = extrasize
  method extrasize_reset = begin
    self#toolbar_driver#reset_image_size ();
    Cortex.set (extrasize) (extrasize_default);
    end

  method iconsize_for_dot  = Cortex.get (iconsize)
  method rankdir_for_dot   = "rankdir="^(Cortex.get rankdir)^";"
  method nodesep_for_dot   = let s=(string_of_float (Cortex.get nodesep)) in ("nodesep="^s^"; ranksep="^s)
  method labeldistance_for_dot = "labeldistance="^(string_of_float (Cortex.get labeldistance))

  (** This is the method used in user gui callbacks (reactions) *)
  val mutable gui_callbacks_disable : bool   = false
  method gui_callbacks_disable   = gui_callbacks_disable
  method set_gui_callbacks_disable x = gui_callbacks_disable <- x
  method disable_gui_callbacks    () = gui_callbacks_disable <- true
  method enable_gui_callbacks     () =
   ignore (GMain.Timeout.add ~ms:500 ~callback:(fun () -> gui_callbacks_disable <- false; false))

  (* Delete _alone here:  *)
  method reset_defaults () =
    begin
      Cortex.set (iconsize)      (iconsize_default);
      Cortex.set (shuffler)      (shuffler_default);     (* self#shuffler_reset *)
      Cortex.set (rankdir)       (rankdir_default);
      Cortex.set (curved_lines)  (curved_lines_default);
      Cortex.set (nodesep)       (nodesep_default);
      Cortex.set (labeldistance) (labeldistance_default); 
      ListExtra.foreach (network#reversed_cables) (network#reversed_cable_set false) ;
      self#extrasize_reset;
      self#set_toolbar_widgets ()
    end

  method ratio : string =
   let extrasize = Cortex.get (extrasize) in
   if (extrasize = 0.) then "ratio=compress;" else
   begin (* BUG HERE !!!!!!!!!!!!!!!!!!!!!!!!!!!! when starting marionnet and loading a project with an extrasize >0 defined, 
            we go to this branche and because there isn't an "original" image, we have an ugly exception!!!! *)
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

  val mutable toolbar_driver : toolbar_driver option = None
  method set_toolbar_driver t = toolbar_driver <- Some t
  method toolbar_driver = match toolbar_driver with Some t -> t | None -> assert false

  (** The dotoption gui reactualization *)

  method set_toolbar_widgets () : unit =
    begin
      self#disable_gui_callbacks   () ;
      self#toolbar_driver#set_iconsize      (Cortex.get iconsize);
      self#toolbar_driver#set_nodesep       (Cortex.get nodesep);
      self#toolbar_driver#set_labeldistance (Cortex.get labeldistance);
      self#toolbar_driver#set_extrasize     (Cortex.get extrasize);
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
     ("iconsize"      , (Cortex.get iconsize)          );
     ("shuffler"      , (Xforest.encode (Cortex.get shuffler)) );
     ("rankdir"       , (Cortex.get rankdir)           );
     ("curved_lines"  , (string_of_bool (Cortex.get curved_lines)));
     ("nodesep"       , (string_of_float (Cortex.get nodesep))     );
     ("labeldistance" , (string_of_float (Cortex.get labeldistance)));
     ("extrasize"     , (string_of_float (Cortex.get extrasize))   );
     ("gui_callbacks_disable", (string_of_bool gui_callbacks_disable));
     ("invertedCables", (Xforest.encode network#reversed_cables));
     ])

 (** A Dotoption.network has just attributes (no children) in this version.
     The Dotoption.network must be undumped AFTER the Netmodel.network in
     order to have significant cable names (reversed_cables). *)
 method eval_forest_attribute = function
  | ("iconsize"             , x ) -> (Cortex.set self#iconsize x)
  | ("shuffler"             , x ) -> (Cortex.set self#shuffler (Xforest.decode x))
  | ("rankdir"              , x ) -> (Cortex.set self#rankdir x)
  | ("curved_lines"         , x ) -> (Cortex.set self#curved_lines (bool_of_string x))
  | ("nodesep"              , x ) -> (Cortex.set self#nodesep (float_of_string x))
  | ("labeldistance"        , x ) -> (Cortex.set self#labeldistance (float_of_string x))
  | ("extrasize"            , x ) -> (Cortex.set self#extrasize     (float_of_string x))
  | ("gui_callbacks_disable", x ) -> self#set_gui_callbacks_disable (bool_of_string x)
  | ("invertedCables"       , x ) -> self#set_reversed_cables (Xforest.decode x)
  | _ -> () (* Forward-comp. *)

end (* class tuning *)
