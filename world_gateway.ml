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


(** "world gateway" component implementation. *)

open Gettext;;

(* Switch related constants: *)
(* TODO: make it configurable! *)
module Const = struct
 let port_no_default = 4
 let port_no_min = 4
 let port_no_max = 16

 let network_config_default = ((10,0,2,1),24)
 let network_address_default = "10.0.2.0"
end

#load "where_p4.cmo"
;;

(* The type of data exchanged with the dialog: *)
module Data = struct
type t = {
  name             : string;
  label            : string;
  network_config   : Ipv4.config;
  dhcp_enabled     : bool;
  port_no          : int;
  old_name         : string;
  }

let to_string t = "<obj>" (* TODO? *)
end (* Data *)

module Tool = struct

 let network_address_of_config (config:Ipv4.config) =
   let ((i1,i2,i3,_),_) = config in
   Printf.sprintf "%i.%i.%i.%i" i1 i2 i3 0

end (* module Tool *)

module Make_menus (Params : sig
  val st      : State.globalState
  val packing : [ `toolbar of GButton.toolbar | `menu_parent of Menu_factory.menu_parent ]
 end) = struct

  open Params

  module Toolbar_entry = struct
   let imagefile = "ico.world_gateway.palette.png"
   let tooltip   = (s_ "World gateway (router)")
   let packing   = Params.packing
  end

  module Add = struct
    include Data

    let key = Some GdkKeysyms._G

    let ok_callback t = Gui_bricks.Ok_callback.check_name t.name t.old_name st#network#name_exists t

    let dialog () =
      let name = st#network#suggestedName "G" in
      Dialog_add_or_update.make
        ~title:(s_ "Add world gateway") ~name ~ok_callback ()

    let reaction {
         name = name;
         label = label;
         network_config = network_config;
         dhcp_enabled = dhcp_enabled;
         port_no = port_no;
         old_name = _ ;
         }
      =
      let action () = ignore (
       new User_level_world_gateway.world_gateway
          ~network:st#network
          ~name
          ~label
          ~port_no
          ~network_address:(Tool.network_address_of_config network_config)
          ~dhcp_enabled
          ())
      in
      st#network_change action ();
  end

  module Properties = struct
    include Data

    let dynlist () = st#network#get_devices_that_can_startup ~devkind:`World_gateway ()

    let dialog name () =
     let d = (st#network#get_device_by_name name) in
     let g = ((Obj.magic d):> User_level_world_gateway.world_gateway) in
     let title = (s_ "Modify world gateway")^" "^name in
     let label = g#get_label in
     (* With the current version of slirpvde i4 is always 1 and cidr is 24 *)
     let network_config =
       let fixed_cidr = 24 in
       ((Ipv4.of_string g#get_network_address), fixed_cidr)
     in
     let dhcp_enabled = g#get_dhcp_enabled in
     let port_no = g#get_port_no in
     (* The user cannot remove receptacles used by a cable. *)
     let port_no_min = st#network#port_no_lower_of (g :> User_level.node) in
     Dialog_add_or_update.make
       ~title ~name ~label ~network_config ~dhcp_enabled ~port_no ~port_no_min
       ~ok_callback:Add.ok_callback ()


    let reaction {
         name = name;
         label = label;
         network_config = network_config;
         dhcp_enabled = dhcp_enabled;
         port_no = port_no;
         old_name = old_name ;
         }
      =
      let d = (st#network#get_device_by_name old_name) in
      let g = ((Obj.magic d):> User_level_world_gateway.world_gateway) in
      let action () = g#update_world_gateway_with ~name ~label ~network_config ~dhcp_enabled ~port_no in
      st#network_change action ();

  end

  module Remove = struct
    type t = string (* just the name *)
    let to_string = (Printf.sprintf "name = %s\n")

    let dynlist     = Properties.dynlist

    let dialog name () =
      Gui_bricks.Dialog.yes_or_cancel_question
        ~title:(s_ "Remove")
        ~markup:(Printf.sprintf (f_ "Are you sure that you want to remove %s\nand all the cables connected to this %s?") name (s_ "gateway"))
        ~context:name
        ()

    let reaction name =
      let d = (st#network#get_device_by_name name) in
      let g = ((Obj.magic d):> User_level_world_gateway.world_gateway) in
      let action () = g#destroy in
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
    let dynlist = st#network#get_devices_that_can_gracefully_shutdown ~devkind:`World_gateway
    let dialog = Menu_factory.no_dialog_but_simply_return_name
    let reaction name = (st#network#get_device_by_name name)#gracefully_shutdown

  end

  module Suspend = struct
    type t = string (* just the name *)
    let to_string = (Printf.sprintf "name = %s\n")
    let dynlist () = st#network#get_devices_that_can_suspend ~devkind:`World_gateway ()
    let dialog = Menu_factory.no_dialog_but_simply_return_name
    let reaction name = (st#network#get_device_by_name name)#suspend

  end

  module Resume = struct
    type t = string (* just the name *)
    let to_string = (Printf.sprintf "name = %s\n")
    let dynlist () = st#network#get_devices_that_can_resume ~devkind:`World_gateway ()
    let dialog = Menu_factory.no_dialog_but_simply_return_name
    let reaction name = (st#network#get_device_by_name name)#resume

  end

 module Create_entries =
  Gui_toolbar_COMPONENTS_layouts.Layout_for_network_node (Params) (Toolbar_entry) (Add) (Properties) (Remove) (Startup) (Stop) (Suspend) (Resume)

 (* Subscribe this kind of component to the network club: *)
 st#network#subscribe_a_try_to_add_procedure Eval_forest_child.try_to_add_world_gateway;

end

(*-----*)
  WHERE
(*-----*)

module Dialog_add_or_update = struct

(* This function may be useful for testing the widget creation without
   recompiling the whole project. *)
let make
 ?(title="Add a world gateway")
 ?(name="")
 ?label
 ?(network_config=Const.network_config_default)
 ?(dhcp_enabled=true)
 ?(port_no=Const.port_no_default)
 ?(port_no_min=Const.port_no_min)
 ?(port_no_max=Const.port_no_max)
 ?(help_callback=help_callback) (* defined backward with "WHERE" *)
 ?(ok_callback=(fun data -> Some data))
 ?(dialog_image_file=Initialization.Path.images^"ico.world_gateway.dialog.png")
 () :'result option =
  let old_name = name in
  let ((b1,b2,b3,b4),b5) = network_config in
  let (w,_,name,label) =
     Gui_bricks.Dialog_add_or_update.make_window_image_name_and_label
      ~title
      ~image_file:dialog_image_file
      ~image_tooltip:(s_ "World gateway")
      ~name
      ~name_tooltip:(s_ "World gateway name. This name must be unique in the virtual network. Suggested: G1, G2, ...")
      ?label
      ()
  in
  let ((s1,s2,s3,s4,s5), dhcp_enabled, port_no) =
    let vbox = GPack.vbox ~homogeneous:false ~border_width:20 ~spacing:10 ~packing:w#vbox#add () in
    let form =
      Gui_bricks.make_form_with_labels
        ~packing:vbox#add
        [(s_ "IPv4 address"); (s_ "DHCP service"); (s_ "Integrated switch ports")]
    in
    let network_config =
      Gui_bricks.spin_ipv4_address_with_cidr_netmask
        ~packing:(form#add_with_tooltip ~just_for_label:() "IPv4 address of the gateway")
        b1 b2 b3 b4 b5
    in
    let dhcp_enabled =
      GButton.check_button
        ~active:dhcp_enabled
        ~packing:(form#add_with_tooltip (s_ "Should the gateway provide a DHCP service?" )) ()
    in
    let port_no =
      Gui_bricks.spin_byte
        ~packing:(form#add_with_tooltip (s_ "The number of ports of the integrated switch" ))
        ~lower:port_no_min ~upper:port_no_max ~step_incr:2
        port_no
    in
    (network_config, dhcp_enabled, port_no)
  in
  s4#misc#set_sensitive false;
  s5#misc#set_sensitive false;

  let get_widget_data () :'result =
    let name = name#text in
    let label = label#text in
    let network_config =
      let s1 = int_of_float s1#value in
      let s2 = int_of_float s2#value in
      let s3 = int_of_float s3#value in
      let s4 = int_of_float s4#value in
      let s5 = int_of_float s5#value in
      ((s1,s2,s3,s4),s5)
    in
    let dhcp_enabled = dhcp_enabled#active in
    let port_no = int_of_float port_no#value in
      { Data.name = name;
        Data.label = label;
        Data.network_config = network_config;
        Data.dhcp_enabled = dhcp_enabled;
        Data.port_no = port_no;
        Data.old_name = old_name;
        }
  in
  (* The result of make is the result of the dialog loop (of type 'result option): *)
  Gui_bricks.Dialog_run.ok_or_cancel w ~ok_callback ~help_callback ~get_widget_data ()

(*-----*)
  WHERE
(*-----*)

 let help_callback =
   let title = (s_ "ADD OR MODIFY A WORLD GATEWAY") in
   let msg   = (s_ "\
In this dialog window you can define the name of a gateway \
to the real world (i.e. the world of the host machine) \
and set many parameters for it:\n\n\
- Label: a string appearing near the router icon in the network graph; \
this field is exclusively for graphic purposes, is not taken in consideration \
for the configuration.\n\n\
- Ipv4 address: the address of the gateway that will be used by the virtual \
machines connected to it.\n\n\
- DHCP service: enabling this option, machines will be able to use the world gateway \
as DHCP server, receiving leases in the range defined by the Ipv4 address. \
This service also provides a DNS proxy\n\n\
- Integrated switch ports: \
the number of ports of the integrated switch (default 4); this number must \
not be increased without a good reason, because the number of processes needed for the \
device emulation is proportional to its ports number.\n\n\
The emulation of this device is realised with the program 'slirpvde' derived from \
the project VDE.\n")
   in Simple_dialogs.help title msg ;;

end

(*-----*)
  WHERE
(*-----*)

module Eval_forest_child = struct
 let try_to_add_world_gateway (network:User_level.network) (f:Xforest.tree) =
  try
   (match f with
    | Forest.NonEmpty (("world_gateway", attrs) , childs , Forest.Empty) ->
    	let name  = List.assoc "name" attrs in
    	let port_no  =
	  try int_of_string (List.assoc "port_no" attrs) with _ -> Const.port_no_default
	in
        Log.printf "Importing world gateway \"%s\" with %d ports...\n" name port_no;
	let x = new User_level_world_gateway.world_gateway ~network ~name ~port_no () in
	x#from_forest ("world_gateway", attrs) childs;
        Log.printf "World gateway \"%s\" successfully imported.\n" name;
        true
   | _ ->
        false
   )
  with _ -> false
end (* module Eval_forest_child *)


(*-----*)
  WHERE
(*-----*)


module User_level_world_gateway = struct

(** A gateway has an associated network address
    and a dhcp server capability. *)
class world_gateway =

  fun ~(network:User_level.network)
      ~name
      ?label
      ?(port_no=4)
      ?(network_address=Const.network_address_default)
      ?(dhcp_enabled=true)
      () ->
  object (self) inherit OoExtra.destroy_methods ()

  inherit User_level.node_with_ledgrid_and_defects
    ~network
    ~name
    ?label
    ~devkind:`World_gateway
    ~port_no
    ~port_no_min:Const.port_no_min
    ~port_no_max:Const.port_no_max
    ~port_prefix:"port" (* because these ports are of the integrated switch *)
    ~user_port_offset:1 (* because is a switch *)
    ()
    as self_as_node_with_ledgrid_and_defects

  method ledgrid_label = "World gateway"
  method defects_device_type = "router"
  method polarity = User_level.Intelligent
  method string_of_devkind = "world_gateway"

  method dotImg (z:User_level.iconsize) =
    let imgDir = Initialization.Path.images in
    (imgDir^"ico.world_gateway."^(self#string_of_simulated_device_state)^"."^z^".png")

  method show = (self#name^" (world gateway)")

  val mutable network_address : string = network_address
  method get_network_address = network_address
  method set_network_address x = network_address <- self#check_network_address x
  method private check_network_address x = x (* TODO *)

  val mutable dhcp_enabled : bool = dhcp_enabled
  method get_dhcp_enabled = dhcp_enabled
  method set_dhcp_enabled x = dhcp_enabled <- x

  (** Redefined:*)
  method gw_ipv4_address =
    let (b1,b2,b3,_) = Ipv4.of_string self#get_network_address in
    let last_byte = 2 in
    (b1,b2,b3, last_byte)

  method gw_ipv4_address_as_string : string =
    Ipv4.to_string self#gw_ipv4_address

  (** Redefined:*)
  method label_for_dot =
    let ip_gw = Ipv4.string_of_config (self#gw_ipv4_address, 24) in
    match self#get_label with
    | "" -> ip_gw
    | _  -> Printf.sprintf "%s <br/> %s" ip_gw self#get_label

  method to_forest =
   Forest.leaf ("world_gateway", [
                  ("name",  self#get_name);
                  ("label", self#get_label);
                  ("network_address", self#get_network_address);
                  ("dhcp_enabled", (string_of_bool self#get_dhcp_enabled));
                  ("port_no", (string_of_int self#get_port_no));
                  ])

  (** A world_bridge has just attributes (no childs) in this version. *)
  method eval_forest_attribute = function
  | ("name"  , x ) -> self#set_name x
  | ("label" , x ) -> self#set_label x
  | ("network_address", x ) -> self#set_network_address x
  | ("dhcp_enabled", x) -> self#set_dhcp_enabled (bool_of_string x)
  | ("port_no", x) -> self#set_port_no (int_of_string x)
  | _ -> assert false

  (** Create the simulated device *)
  method private make_simulated_device =
    ((new Simulation_level_world_gateway.world_gateway
        ~parent:self
	~port_no:self#get_port_no
	~network_address
	~dhcp_enabled
	~unexpected_death_callback:self#destroy_because_of_unexpected_death
	()) :> User_level.node Simulation_level.device)

  method update_world_gateway_with ~name ~label ~port_no ~network_config ~dhcp_enabled =
    (* The following call ensure that the simulated device will be destroyed: *)
    self_as_node_with_ledgrid_and_defects#update_with ~name ~label ~port_no;
    self#set_network_address (Tool.network_address_of_config network_config);
    self#set_dhcp_enabled dhcp_enabled;

end (* class world_gateway *)

end (* module User_level_world_gateway *)

(*-----*)
  WHERE
(*-----*)

module Simulation_level_world_gateway = struct

class ['parent] world_gateway =
  fun ~(parent:'parent)
      ~port_no
      ~network_address (* default 10.0.2.0 *)
      ~dhcp_enabled
      ~unexpected_death_callback
      () ->
 (* an additional port will be used by the world *)
 let hublet_no = port_no + 1 in
 let last_user_visible_port_index = port_no - 1 in
 object (self)
  inherit ['parent] Switch.Simulation_level_switch.switch
    ~parent
    ~hublet_no
    ~last_user_visible_port_index
    ~unexpected_death_callback
    () as super

  method device_type = "world_gateway"

  initializer

    let last_reserved_port = port_no in
    let slirpvde_socket = (self#get_hublet_process_of_port last_reserved_port)#get_socket_name in

    self#add_accessory_process
      (new Simulation_level.slirpvde_process
 	~existing_socket_name:slirpvde_socket
 	~network:network_address
	?dhcp:(Option.of_bool dhcp_enabled)
 	~unexpected_death_callback:self#execute_the_unexpected_death_callback
	())

end;;

end (* module Simulation_level_world_gateway *)

(** Just for testing: *)
let test = Dialog_add_or_update.make
