(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2010  Jean-Vincent Loddo

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


(** "Switch" component implementation. *)

#load "where_p4.cmo"
;;

open Gettext

(* Switch related constants: *)
(* TODO: make it configurable! *)
module Const = struct
 let port_no_default = 4
 let port_no_min = 4
 let port_no_max = 16
end

(* The type of data exchanged with the dialog: *)
module Data = struct
type t = {
  name              : string;
  label             : string;
  port_no           : int;
  show_vde_terminal : bool;
  old_name          : string;
  }

let to_string t = "<obj>" (* TODO? *)
end (* Data *)

module Make_menus (Params : sig
  val st      : State.globalState
  val packing : [ `toolbar of GButton.toolbar | `menu_parent of Menu_factory.menu_parent ]
 end) = struct

  open Params

  module Toolbar_entry = struct
   let imagefile = "ico.switch.palette.png"
   let tooltip   = (s_ "Switch")
   let packing   = Params.packing
  end

  module Add = struct
    include Data

    let key = Some GdkKeysyms._S

    let ok_callback t = Gui_bricks.Ok_callback.check_name t.name t.old_name st#network#name_exists t

    let dialog () =
      let name = st#network#suggestedName "S" in
      Dialog_add_or_update.make ~title:(s_ "Add switch") ~name ~ok_callback ()

    let reaction { name = name; label = label; port_no = port_no; show_vde_terminal = show_vde_terminal; } =
      let action () =
        ignore
          (new User_level_switch.switch ~network:st#network ~name ~label ~port_no ~show_vde_terminal ())
      in
      st#network_change action ();

  end

  module Properties = struct
    include Data
    let dynlist () = st#network#get_devices_that_can_startup ~devkind:`Switch ()

    let dialog name () =
     let d = (st#network#get_device_by_name name) in
     let s = ((Obj.magic d):> User_level_switch.switch) in
     let title = (s_ "Modify switch")^" "^name in
     let label = s#get_label in
     let port_no = s#get_port_no in
     let port_no_min = st#network#port_no_lower_of (s :> User_level.node) in
     let show_vde_terminal = s#get_show_vde_terminal in
     Dialog_add_or_update.make
       ~title ~name ~label ~port_no ~port_no_min ~show_vde_terminal ~ok_callback:Add.ok_callback ()

    let reaction { name = name; label = label; port_no = port_no; old_name = old_name;
                   show_vde_terminal = show_vde_terminal; } =
      let d = (st#network#get_device_by_name old_name) in
      let s = ((Obj.magic d):> User_level_switch.switch) in
      let action () = s#update_switch_with ~name ~label ~port_no ~show_vde_terminal in
      st#network_change action ();

  end

  module Remove = struct
    type t = string (* just the name *)
    let to_string = (Printf.sprintf "name = %s\n")

    let dynlist = Properties.dynlist

    let dialog name () =
      Gui_bricks.Dialog.yes_or_cancel_question
        ~title:(s_ "Remove")
        ~markup:(Printf.sprintf (f_ "Are you sure that you want to remove %s\nand all the cables connected to this %s?") name (s_ "switch"))
        ~context:name
        ()

    let reaction name =
      let d = (st#network#get_device_by_name name) in
      let h = ((Obj.magic d):> User_level_switch.switch) in
      let action () = h#destroy in
      st#network_change action ();

  end

  module Startup = struct
    type t = string (* just the name *)
    let to_string = (Printf.sprintf "name = %s\n")
    let dynlist    = Properties.dynlist
    let dialog     = Menu_factory.no_dialog_but_simply_return_name
    let reaction name = (st#network#get_device_by_name name)#startup

  end

  module Stop = struct
    type t = string (* just the name *)
    let to_string = (Printf.sprintf "name = %s\n")
    let dynlist () = st#network#get_devices_that_can_gracefully_shutdown ~devkind:`Switch ()
    let dialog = Menu_factory.no_dialog_but_simply_return_name
    let reaction name = (st#network#get_device_by_name name)#gracefully_shutdown

  end

  module Suspend = struct
    type t = string (* just the name *)
    let to_string = (Printf.sprintf "name = %s\n")
    let dynlist () = st#network#get_devices_that_can_suspend ~devkind:`Switch ()
    let dialog = Menu_factory.no_dialog_but_simply_return_name
    let reaction name = (st#network#get_device_by_name name)#suspend

  end

  module Resume = struct
    type t = string (* just the name *)
    let to_string = (Printf.sprintf "name = %s\n")
    let dynlist () = st#network#get_devices_that_can_resume ~devkind:`Switch ()
    let dialog = Menu_factory.no_dialog_but_simply_return_name
    let reaction name = (st#network#get_device_by_name name)#resume

  end

 module Create_entries =
  Gui_toolbar_COMPONENTS_layouts.Layout_for_network_node (Params) (Toolbar_entry) (Add) (Properties) (Remove) (Startup) (Stop) (Suspend) (Resume)

 (* Subscribe this kind of component to the network club: *)
 st#network#subscribe_a_try_to_add_procedure Eval_forest_child.try_to_add_switch;

end

(*-----*)
  WHERE
(*-----*)

module Dialog_add_or_update = struct

(* This function may be useful for testing the widget creation without
   recompiling the whole project. *)
let make
 ?(title="Add switch")
 ?(name="")
 ?label
 ?(port_no=Const.port_no_default)
 ?(port_no_min=Const.port_no_min)
 ?(port_no_max=Const.port_no_max)
 ?(show_vde_terminal=false)
 ?(help_callback=help_callback) (* defined backward with "WHERE" *)
 ?(ok_callback=(fun data -> Some data))
 ?(dialog_image_file=Initialization.Path.images^"ico.switch.dialog.png")
 () :'result option =
  let old_name = name in
  let (w,_,name,label) =
    Gui_bricks.Dialog_add_or_update.make_window_image_name_and_label
      ~title
      ~image_file:dialog_image_file
      ~image_tooltip:(s_ "Switch")
      ~name
      ~name_tooltip:(s_ "Switch name. This name must be unique in the virtual network. Suggested: S1, S2, ...")
      ?label
      ()
  in
  let (port_no, show_vde_terminal) =
    let vbox = GPack.vbox ~homogeneous:false ~border_width:20 ~spacing:10 ~packing:w#vbox#add () in
    let form =
      Gui_bricks.make_form_with_labels
        ~packing:vbox#add
        [(s_ "Ports number");
         (s_ "Show VDE terminal");
        ]
    in
    let port_no =
      Gui_bricks.spin_byte
        ~packing:(form#add_with_tooltip (s_ "Switch ports number"))
        ~lower:port_no_min ~upper:port_no_max ~step_incr:2
        port_no
    in
    let show_vde_terminal =
      GButton.check_button
        ~active:show_vde_terminal
        ~packing:(form#add_with_tooltip (s_ "Check to access the switch through a terminal" ))
        ()
    in
    (port_no, show_vde_terminal)
  in

  let get_widget_data () :'result =
    let name = name#text in
    let label = label#text in
    let port_no = int_of_float port_no#value in
    let show_vde_terminal = show_vde_terminal#active in
      { Data.name = name;
        Data.label = label;
        Data.port_no = port_no;
        Data.show_vde_terminal = show_vde_terminal;
        Data.old_name = old_name;
        }
  in
  (* The result of make is the result of the dialog loop (of type 'result option): *)
  Gui_bricks.Dialog_run.ok_or_cancel w ~ok_callback ~help_callback ~get_widget_data ()

(*-----*)
  WHERE
(*-----*)

 let help_callback =
   let title = (s_ "ADD OR MODIFY A SWITCH") in
   let msg   = (s_ "\
In this dialog window you can define the name of an Ethernet switch \
and set parameters for it:\n\n\
- Label: a string appearing near the switch icon in the network graph; it may \
allow, for example, to know at a glance the Ethernet network realized by the device; \
this field is exclusively for graphic purposes, is not taken in consideration \
for the configuration.\n\n\
- Nb of Ports: the number of ports of the switch (default 4); this number must \
not be increased without a reason, because the number of processes needed for the \
device emulation is proportional to his ports number.")
   in Simple_dialogs.help title msg

end

(*-----*)
  WHERE
(*-----*)

module Eval_forest_child = struct

 let try_to_add_switch (network:User_level.network) (f:Xforest.tree) =
  try
   (match f with
    | Forest.NonEmpty (("switch", attrs) , childs , Forest.Empty) ->
    	let name  = List.assoc "name" attrs in
	let port_no = int_of_string (List.assoc "port_no" attrs) in
        Log.printf "Importing switch \"%s\" with %d ports...\n" name port_no;
	let x = new User_level_switch.switch ~network ~name ~port_no () in
	x#from_forest ("switch", attrs) childs;
        Log.printf "Switch \"%s\" successfully imported.\n" name;
        true

    (* backward compatibility *)
    | Forest.NonEmpty (("device", attrs) , childs , Forest.Empty) ->
	let name  = List.assoc "name" attrs in
	let port_no = try int_of_string (List.assoc "eth" attrs) with _ -> Const.port_no_default in
	let kind = List.assoc "kind" attrs in
	(match kind with
	| "switch" ->
            Log.printf "Importing switch \"%s\" with %d ports...\n" name port_no;
	    let x = new User_level_switch.switch ~network ~name ~port_no () in
	    x#from_forest ("device", attrs) childs; (* Just for the label... *)
            Log.printf "This is an old project: we set the user port offset to 1...\n";
	    network#defects#change_port_user_offset ~device_name:name ~user_port_offset:1;
	    Log.printf "Switch \"%s\" successfully imported.\n" name;
	    true
	| _ -> false
	)
   | _ -> false
   )
  with _ -> false

end (* module Eval_forest_child *)


(*-----*)
  WHERE
(*-----*)


module User_level_switch = struct

class switch =

 fun ~network
     ~name
     ?label
     ~port_no
     ?(show_vde_terminal=false)
     () ->
  object (self) inherit OoExtra.destroy_methods ()

  inherit
    User_level.node_with_ledgrid_and_defects
      ~network
      ~name ?label ~devkind:`Switch
      ~port_no
      ~port_no_min:Const.port_no_min
      ~port_no_max:Const.port_no_max
      ~user_port_offset:1 (* in order to have a perfect mapping with VDE *)
      ~port_prefix:"port"
      ()
    as self_as_node_with_ledgrid_and_defects 
  method ledgrid_label = "Switch"
  method defects_device_type = "switch"
  method polarity = User_level.MDI_X
  method string_of_devkind = "switch"

  val mutable show_vde_terminal : bool = show_vde_terminal
  method get_show_vde_terminal = show_vde_terminal
  method set_show_vde_terminal x = show_vde_terminal <- x
  
  method dotImg iconsize =
   let imgDir = Initialization.Path.images in
   (imgDir^"ico.switch."^(self#string_of_simulated_device_state)^"."^iconsize^".png")

  method update_switch_with ~name ~label ~port_no ~show_vde_terminal =
   (* The following call ensure that the simulated device will be destroyed: *)
   self_as_node_with_ledgrid_and_defects#update_with ~name ~label ~port_no;
   self#set_show_vde_terminal show_vde_terminal;

  (** Create the simulated device *)
  method private make_simulated_device =
    let hublet_no = self#get_port_no in
    let show_vde_terminal = self#get_show_vde_terminal in
    let unexpected_death_callback = self#destroy_because_of_unexpected_death in
    ((new Simulation_level_switch.switch
       ~parent:self
       ~hublet_no          (* TODO: why not accessible from parent? *)
       ~show_vde_terminal  (* TODO: why not accessible from parent? *)
       ~unexpected_death_callback
       ()) :> User_level.node Simulation_level.device)

  method to_forest =
   Forest.leaf ("switch", [
                   ("name"     ,  self#get_name );
                   ("label"    ,  self#get_label);
                   ("port_no"  ,  (string_of_int self#get_port_no))  ;
                   ("show_vde_terminal" , string_of_bool (self#get_show_vde_terminal));
	           ])

  method eval_forest_attribute = function
  | ("name"     , x ) -> self#set_name x
  | ("label"    , x ) -> self#set_label x
  | ("port_no"  , x ) -> self#set_port_no (int_of_string x)
  | ("show_vde_terminal", x ) -> self#set_show_vde_terminal (bool_of_string x)
  | _ -> () (* Forward-comp. *)

end (* class switch *)

end (* module User_level *)

(*-----*)
  WHERE
(*-----*)

module Simulation_level_switch = struct

(** A switch: just a [hub_or_switch] with [hub = false] *)
class ['parent] switch =
  fun ~(parent:'parent)
      ~hublet_no
      ?(last_user_visible_port_index:int option)
      ?(show_vde_terminal=false)
      ~unexpected_death_callback
      () ->
object(self)
  inherit ['parent] Simulation_level.hub_or_switch
      ~parent
      ~hublet_no
      ?last_user_visible_port_index
      ~hub:false
      ~management_socket:()
      ~unexpected_death_callback
      ()
      as super
  method device_type = "switch"

  initializer

  match show_vde_terminal with
  | false -> ()
  | true ->
    let name = parent#get_name in
    self#add_accessory_process
      (new Simulation_level.unixterm_process
        ~xterm_title:(name^" terminal")
        ~management_socket_name:(Option.extract self#get_management_socket_name)
 	~unexpected_death_callback:
 	   (fun i _ ->
 	      Death_monitor.stop_monitoring i;
 	      Log.printf "Terminal of switch %s closed (pid %d).\n" name i)
	())

end;;

end (* module Simulation_level_switch *)

(** Just for testing: *)
let test = Dialog_add_or_update.make
