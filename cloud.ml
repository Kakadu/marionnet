(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2010  Jean-Vincent Loddo
   Copyright (C) 2010  Université Paris 13

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


(** "cloud" component implementation. *)

#load "where_p4.cmo"
;;

open Gettext

(* Cloud related constants: *)
(* TODO: make it configurable! *)
module Const = struct
 let port_no_default = 2
 let port_no_min = 2
 let port_no_max = 2
end

(* The type of data exchanged with the dialog: *)
module Data = struct
type t = {
  name        : string;
  label       : string;
  old_name    : string;
  }

let to_string t = "<obj>" (* TODO? *)
end (* Data *)

module Make_menus (Params : sig
  val st      : State.globalState
  val packing : [ `toolbar of GButton.toolbar | `menu_parent of Menu_factory.menu_parent ]
 end) = struct

  open Params

  module Toolbar_entry = struct
   let imagefile = "ico.cloud.palette.png"
   let tooltip   = s_ "Unknown layer 2 sub-network"
   let packing   = Params.packing
  end

  module Add = struct
    include Data

    let key = None

    let ok_callback t = Gui_bricks.Ok_callback.check_name t.name t.old_name st#network#name_exists t

    let dialog () =
      let name = st#network#suggestedName "N" in
      Dialog_add_or_update.make ~title:(s_ "Add cloud") ~name ~ok_callback ()

    let reaction { name = name; label = label } =
      let action () = ignore (
        new User_level_cloud.cloud
          ~network:st#network
          ~name
          ~label
          ()) in
      st#network_change action ();

  end

  module Properties = struct
    include Data
    let dynlist () = st#network#get_node_names_that_can_startup ~devkind:`Cloud ()

    let dialog name () =
     let d = (st#network#get_node_by_name name) in
     let title = (s_ "Modify cloud")^" "^name in
     let label = d#get_label in
     Dialog_add_or_update.make ~title ~name ~label ~ok_callback:Add.ok_callback ()

    let reaction { name = name; label = label; old_name = old_name } =
      let d = (st#network#get_node_by_name old_name) in
      let h = ((Obj.magic d):> User_level_cloud.cloud) in
      let action () = h#update_cloud_with ~name ~label in
      st#network_change action ();

  end

  module Remove = struct
    type t = string (* just the name *)
    let to_string = (Printf.sprintf "name = %s\n")

    let dynlist = Properties.dynlist

    let dialog name () =
      Gui_bricks.Dialog.yes_or_cancel_question
        ~title:(s_ "Remove")
        ~markup:(Printf.sprintf (f_ "Are you sure that you want to remove %s\nand all the cables connected to this %s?") name (s_ "cloud"))
        ~context:name
        ()

    let reaction name =
      let d = (st#network#get_node_by_name name) in
      let h = ((Obj.magic d):> User_level_cloud.cloud) in
      let action () = h#destroy in
      st#network_change action ();

  end

  module Startup = struct
    type t = string (* just the name *)
    let to_string = (Printf.sprintf "name = %s\n")
    let dynlist    = Properties.dynlist
    let dialog     = Menu_factory.no_dialog_but_simply_return_name
    let reaction name = (st#network#get_node_by_name name)#startup

  end

  module Stop = struct
    type t = string (* just the name *)
    let to_string = (Printf.sprintf "name = %s\n")
    let dynlist () = st#network#get_node_names_that_can_gracefully_shutdown ~devkind:`Cloud ()
    let dialog = Menu_factory.no_dialog_but_simply_return_name
    let reaction name = (st#network#get_node_by_name name)#gracefully_shutdown

  end

  module Suspend = struct
    type t = string (* just the name *)
    let to_string = (Printf.sprintf "name = %s\n")
    let dynlist () = st#network#get_node_names_that_can_suspend ~devkind:`Cloud ()
    let dialog = Menu_factory.no_dialog_but_simply_return_name
    let reaction name = (st#network#get_node_by_name name)#suspend

  end

  module Resume = struct
    type t = string (* just the name *)
    let to_string = (Printf.sprintf "name = %s\n")
    let dynlist () = st#network#get_node_names_that_can_resume ~devkind:`Cloud ()
    let dialog = Menu_factory.no_dialog_but_simply_return_name
    let reaction name = (st#network#get_node_by_name name)#resume

  end

 module Create_entries =
  Gui_toolbar_COMPONENTS_layouts.Layout_for_network_node (Params) (Toolbar_entry) (Add) (Properties) (Remove) (Startup) (Stop) (Suspend) (Resume)

 (* Subscribe this kind of component to the network club: *)
 st#network#subscribe_a_try_to_add_procedure Eval_forest_child.try_to_add_cloud;

end

(*-----*)
  WHERE
(*-----*)

module Dialog_add_or_update = struct

(* This function may be useful for testing the widget creation without
   recompiling the whole project. *)
let make
 ?(title="Add cloud")
 ?(name="")
 ?label
 ?(help_callback=help_callback) (* defined backward with "WHERE" *)
 ?(ok_callback=(fun data -> Some data))
 ?(dialog_image_file=Initialization.Path.images^"ico.cloud.dialog.png")
 () :'result option =
  let old_name = name in
  let (w,_,name,label) =
    Gui_bricks.Dialog_add_or_update.make_window_image_name_and_label
      ~title
      ~image_file:dialog_image_file
      ~image_tooltip:(s_ "Unknown layer 2 sub-network")
      ~name
      ~name_tooltip:(s_ "Sub-network name. This name must be unique in the virtual network. Suggested: N1, N2, ... ")
      ?label
      ()
  in

  let get_widget_data () :'result =
    let name = name#text in
    let label = label#text in
      { Data.name = name;
        Data.label = label;
        Data.old_name = old_name;
        }
  in
  (* The result of make is the result of the dialog loop (of type 'result option): *)
  Gui_bricks.Dialog_run.ok_or_cancel w ~ok_callback ~help_callback ~get_widget_data ()

(*-----*)
  WHERE
(*-----*)

 let help_callback =
   let title = (s_ "ADD OR MODIFY A CLOUD" ) in
   let msg   = (s_ "In this dialog window you can define the name of a cloud. \
This component is an Ethernet network with an unknown internal \
structure introducing delays and other anomalies when packets \
pass through.\n\
Once the cloud is defined, use the tab 'Anomalies' to control delays, \
frame loss and the other anomalies.")
   in Simple_dialogs.help title msg

end

(*-----*)
  WHERE
(*-----*)

module Eval_forest_child = struct

 let try_to_add_cloud (network:User_level.network) ((root,children):Xforest.tree) =
  try
   (match root with
    | ("cloud", attrs) ->
    	let name  = List.assoc "name"  attrs in
        Log.printf1 "Importing cloud \"%s\"...\n" name;
        let x = new User_level_cloud.cloud ~network ~name () in
	x#from_tree ("cloud", attrs) children  ;
        Log.printf1 "Cloud \"%s\" successfully imported.\n" name;
        true
   | _ ->
        false
   )
  with _ -> false

end (* module Eval_forest_child *)


(*-----*)
  WHERE
(*-----*)


module User_level_cloud = struct

class cloud =

 fun ~network
     ~name
     ?label
     () ->
  object (self) inherit OoExtra.destroy_methods ()

  inherit
    User_level.node_with_defects
      ~network
      ~name ?label ~devkind:`Cloud
      ~port_no:Const.port_no_default
      ~port_no_min:Const.port_no_min
      ~port_no_max:Const.port_no_max
      ~user_port_offset:0
      ~port_prefix:"port"
      ()
    as self_as_node_with_defects
  method defects_device_type = "cloud"
  method polarity = User_level.MDI_Auto (* Because it is didactically meaningless *)
  method string_of_devkind = "cloud"

  method dotImg iconsize =
   let imgDir = Initialization.Path.images in
   (imgDir^"ico.cloud."^(self#string_of_simulated_device_state)^"."^iconsize^".png")

  method update_cloud_with ~name ~label =
   self_as_node_with_defects#update_with ~name ~label ~port_no:2;

  (** Create the simulated device *)
  method private make_simulated_device =
   ((new Simulation_level_cloud.cloud
        ~parent:self
        ~working_directory:(network#working_directory)
        ~unexpected_death_callback:self#destroy_because_of_unexpected_death
        ()) :> User_level.node Simulation_level.device)

  method to_tree =
   Forest.tree_of_leaf ("cloud", [
     ("name"     ,  self#get_name );
     ("label"    ,  self#get_label);
     ])

  method eval_forest_attribute = function
  | ("name"     , x ) -> self#set_name x
  | ("label"    , x ) -> self#set_label x
  | _ -> () (* Forward-comp. *)

end (* class cloud *)

end (* module User_level_cloud *)

(*-----*)
  WHERE
(*-----*)

module Simulation_level_cloud = struct

open Daemon_language

class ['parent] cloud =
  fun (* ~id *)
      ~(parent:'parent)
      ~working_directory
      ~unexpected_death_callback
      () ->
object(self)
  inherit ['parent] Simulation_level.device
      ~parent
      ~hublet_no:2
      ~working_directory
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
    let the_internal_cable_process =
      Simulation_level.make_ethernet_cable_process
        ~left_end:(self#get_hublet_process_of_port 0)
        ~right_end:(self#get_hublet_process_of_port 1)
        ~leftward_defects:(parent#ports_card#get_my_inward_defects_by_index 0)
        ~rightward_defects:(parent#ports_card#get_my_outward_defects_by_index 0)
        ~unexpected_death_callback:self#execute_the_unexpected_death_callback
        ()
    in
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


end (* module Simulation_level_cloud *)

(** Just for testing: *)
let test = Dialog_add_or_update.make
