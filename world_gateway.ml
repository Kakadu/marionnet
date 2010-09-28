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

open Gettext;;

(** Gui-related stuff for the user-level component "world gateway". *)

(* The module containing the add/update dialog is defined later,
   using the syntax extension "where" *)
#load "where_p4.cmo"
;;

(* The type of data exchanged with the dialog: *)
module Data = struct
type t = {
  name             : string;
  label            : string;
  network_config   : Ipv4.config;
  dhcp_enabled     : bool;
  user_port_no     : int;
  old_name         : string;
  old_user_port_no : int;
  }

let to_string t = "<obj>" (* TODO? *)
end (* Data *)

module Make_menus (State : sig val st:State.globalState end) = struct

  open State

  module Toolbar_entry = struct
   let imagefile = "ico.world_gateway.palette.png"
   let tooltip   = (s_ "World gateway (router)")
  end

  module Add = struct
    include Data

    let key = Some GdkKeysyms._G

    let ok_callback t = Gui_bricks.Ok_callback.check_name t.name st#network#name_exists t

    let dialog () =
      let name = st#network#suggestedName "G" in
      Dialog_add_or_update.make
        ~title:(s_ "Add world gateway") ~name ~ok_callback ()

    let network_address_of_config config =
      let ((i1,i2,i3,_),_) = config in
      Printf.sprintf "%i.%i.%i.%i" i1 i2 i3 0
   
    let reaction : t -> unit =
     function
      { name = name;
        label = label;
        network_config = network_config;
        dhcp_enabled = dhcp_enabled;
        user_port_no = user_port_no;
        old_name = _ ; old_user_port_no = _ }
      ->
      let g =
        new User_level.world_gateway
          ~network:st#network
          ~name
          ~label
          ~network_address:(network_address_of_config network_config)
          ~dhcp_enabled
          ~user_port_no
          ()
      in
      (* The "world" port is hidden for defects: *)
      let defects = Defects_interface.get_defects_interface () in
      defects#add_device name "router" user_port_no;
      st#network_change st#network#add_device (g :> Mariokit.Netmodel.device);
  end

  module Properties = struct
    include Data

    let dynlist =
     fun () -> List.filter
                  (fun x -> (st#network#get_device_by_name x)#can_startup)
                  (st#network#get_device_names ~devkind:Mariokit.Netmodel.World_gateway ())

    let dialog name () =
     let d = (st#network#get_device_by_name name) in
     let g = ((Obj.magic d):> User_level.world_gateway) in
     let title = (s_ "Modify world gateway")^" "^name in
     let label = g#get_label in
     (* With the current version of slirpvde i4 is always 1 and cidr is 24 *)
     let network_config =
       let fixed_cidr = 24 in
       ((Ipv4.ipv4_of_string g#get_network_address), fixed_cidr)
     in
     let dhcp_enabled = g#get_dhcp_enabled in
     let user_port_no = g#get_eth_number in
     Dialog_add_or_update.make
       ~title ~name ~label ~network_config ~dhcp_enabled ~user_port_no
       ~ok_callback:Add.ok_callback ()


    let reaction : t -> unit =
     function
      { name = name;
        label = label;
        network_config = network_config;
        dhcp_enabled = dhcp_enabled;
        user_port_no = user_port_no;
        old_name = old_name;
        old_user_port_no = old_user_port_no;
        }
      ->
      let defects = Defects_interface.get_defects_interface () in
      let d = (st#network#get_device_by_name old_name) in
      let d = ((Obj.magic d):> User_level.world_gateway) in
      (match (name = old_name) && (user_port_no = old_user_port_no)  with
      | true -> ()
      | false ->
          begin
            d#destroy; (* VERIFICARE *)
            d#set_eth_number ~prefix:"port" user_port_no;
            defects#update_port_no name user_port_no;
            st#network#ledgrid_manager#destroy_device_ledgrid ~id:(d#id) ();
            st#network#change_node_name old_name name  ;
            Filesystem_history.rename_device old_name name;
            defects#rename_device old_name name;
            st#network#make_device_ledgrid (d :> Mariokit.Netmodel.device);
          end
      );
      d#set_label label;
      d#set_network_address (Add.network_address_of_config network_config);
      d#set_dhcp_enabled dhcp_enabled;
      st#update_cable_sensitivity ();
      st#refresh_sketch ();

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
      let defects = Defects_interface.get_defects_interface () in
      st#network_change st#network#del_device name;
      defects#remove_device name;

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

    let dynlist () =
      List.filter
       (fun x -> (st#network#get_device_by_name x)#can_gracefully_shutdown)
       (st#network#get_device_names ~devkind:Mariokit.Netmodel.World_gateway ())

    let dialog = Menu_factory.no_dialog_but_simply_return_name
    let reaction name = (st#network#get_device_by_name name)#gracefully_shutdown

  end

  module Suspend = struct
    type t = string (* just the name *)
    let to_string = (Printf.sprintf "name = %s\n")

    let dynlist () =
      List.filter
       (fun x -> (st#network#get_device_by_name x)#can_suspend)
       (st#network#get_device_names ~devkind:Mariokit.Netmodel.World_gateway ())

    let dialog = Menu_factory.no_dialog_but_simply_return_name
    let reaction name = (st#network#get_device_by_name name)#suspend

  end

  module Resume = struct
    type t = string (* just the name *)
    let to_string = (Printf.sprintf "name = %s\n")

    let dynlist () =
      List.filter
       (fun x -> (st#network#get_device_by_name x)#can_resume)
       (st#network#get_device_names ~devkind:Mariokit.Netmodel.World_gateway ())

    let dialog = Menu_factory.no_dialog_but_simply_return_name
    let reaction name = (st#network#get_device_by_name name)#resume

  end

 module Create_entries =
  Gui_toolbar_COMPONENTS_layouts.Layout_for_network_node (State) (Toolbar_entry) (Add) (Properties) (Remove) (Startup) (Stop) (Suspend) (Resume)

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
 ?(network_config:Ipv4.config option)
 ?(dhcp_enabled=true)
 ?(user_port_no=4)
 ?(help_callback=help_callback) (* defined backward with "WHERE" *)
 ?(ok_callback=(fun data -> Some data))
 ?(dialog_image_file=Initialization.Path.images^"ico.world_gateway.dialog.png")
 () :'result option =
  let old_name = name in
  let old_user_port_no = user_port_no in
  let ((b1,b2,b3,b4),b5) = match network_config with
   | Some x -> x
   | None   -> ((10,0,2,1),24)
  in
  let w = GWindow.dialog ~destroy_with_parent:true ~title ~modal:true ~position:`CENTER () in
  Gui_bricks.set_marionnet_icon w;
  let tooltips = Gui_bricks.make_tooltips_for_container w in

  let (name,label) =
    let hbox = GPack.hbox ~homogeneous:true ~border_width:20 ~spacing:10 ~packing:w#vbox#add () in
    let image = GMisc.image ~file:dialog_image_file ~xalign:0.5 ~packing:hbox#add () in
    tooltips image#coerce (s_ "Gateway");
    let vbox = GPack.vbox ~spacing:10 ~packing:hbox#add () in
    let name  =
      let tooltip = (s_ "Gateway name. This name must be unique in the virtual network. Suggested: G1, G2, ...") in
      Gui_bricks.entry_with_label ~tooltip ~packing:vbox#add ~entry_text:name  (s_ "Name")
    in
    let label =
      let tooltip = (s_ "Label to be written in the network sketch, next to the element icon." ) in
      Gui_bricks.entry_with_label ~tooltip ~packing:vbox#add ?entry_text:label (s_ "Label")
    in
    (name,label)
  in

  ignore (GMisc.separator `HORIZONTAL ~packing:w#vbox#add ());

  let ((s1,s2,s3,s4,s5), dhcp_enabled, user_port_no) =
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
    let user_port_no =
      Gui_bricks.spin_byte
        ~packing:(form#add_with_tooltip (s_ "The number of ports of the integrated switch" ))
        ~lower:2 ~upper:16 ~step_incr:2
        user_port_no
    in
    (network_config, dhcp_enabled, user_port_no)
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
    let user_port_no = int_of_float user_port_no#value in
      { Data.name = name;
        Data.label = label;
        Data.network_config = network_config;
        Data.dhcp_enabled = dhcp_enabled;
        Data.user_port_no = user_port_no;
        Data.old_name = old_name;
        Data.old_user_port_no = old_user_port_no; }
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
 let try_to_add_world_gateway (network:Mariokit.Netmodel.network) (f:Xforest.tree) =
  try
   (match f with
    | Forest.NonEmpty (("world_gateway", attrs) , childs , Forest.Empty) ->
	let x = new User_level.world_gateway ~network () in
	x#from_forest ("world_gateway", attrs) childs  ;
	network#add_device ((Obj.magic x) :> Mariokit.Netmodel.device);
        true
   | _ ->
        false
   )
  with _ -> false
end (* module Eval_forest_child *)


(*-----*)
  WHERE
(*-----*)


module User_level = struct

(** A gateway has an associated network address
    and a dhcp server capability. *)
class world_gateway =

  fun ~network
      ?(name="world_gatewayname")
      ?(label="")
      ?(network_address="10.0.2.0")
      ?(dhcp_enabled=true)
      ?(user_port_no=4)
      () ->
  object (self)
  inherit Mariokit.Netmodel.device ~network ~name ~label ~devkind:Mariokit.Netmodel.World_gateway ~port_no:user_port_no ()
  as super

  method show = (self#name^" (world gateway)")

  val mutable network_address : string = network_address
  method get_network_address = network_address
  method set_network_address x = network_address <- self#check_network_address x
  method private check_network_address x = x (* TODO *)

  val mutable dhcp_enabled : bool = dhcp_enabled
  method get_dhcp_enabled = dhcp_enabled
  method set_dhcp_enabled x = dhcp_enabled <- self#check_dhcp_enabled x
  method private check_dhcp_enabled x = x (* TODO *)

  (** Redefined:*)
  method gw_ipv4_address =
    let (b1,b2,b3,_) = Ipv4.ipv4_of_string self#get_network_address in
    let last_byte = 2 in
    (b1,b2,b3, last_byte)

  method gw_ipv4_address_as_string : string =
    Ipv4.string_of_ipv4 self#gw_ipv4_address

  method dotImg (z:Mariokit.Netmodel.iconsize) =
    let imgDir = Initialization.Path.images in
    (imgDir^"ico.world_gateway."^(self#string_of_simulated_device_state)^"."^z^".png")

  (** Redefined:*)
  method label_for_dot =
    let ip_gw = Ipv4.string_of_ipv4 ~cidr:24 self#gw_ipv4_address in
    match self#get_label with
    | "" -> ip_gw
    | _  -> Printf.sprintf "%s <br/> %s" ip_gw self#get_label

  method to_forest =
   Forest.leaf ("world_gateway", [
                  ("name",  self#get_name);
                  ("label", self#get_label);
                  ("network_address", self#get_network_address);
                  ("dhcp_enabled", (string_of_bool self#get_dhcp_enabled));
                  ])

  (** A world_bridge has just attributes (no childs) in this version. *)
  method eval_forest_attribute = function
  | ("name"  , x ) -> self#set_name x
  | ("label" , x ) -> self#set_label x
  | ("network_address", x ) -> self#set_network_address x
  | ("dhcp_enabled", x) -> self#set_dhcp_enabled (bool_of_string x)
  | _ -> assert false

  (** Create the simulated device *)
  method private make_simulated_device =
    new Simulation_level.world_gateway
      ~name:self#get_name
      ~user_port_no:self#get_eth_number
      ~network_address
      ~dhcp_enabled
      ~unexpected_death_callback:self#destroy_because_of_unexpected_death
      ()
end (* class world_gateway *)

end (* module User_level *)

(*-----*)
  WHERE
(*-----*)

module Simulation_level = struct

class world_gateway =
  fun ~name
      ~user_port_no
      ~network_address (* default 10.0.2.0 *)
      ~dhcp_enabled
      ~unexpected_death_callback
      () ->
 (* an additional port will be used by the world *)
 let hublet_no = user_port_no + 1 in
 let last_user_visible_port_index = user_port_no - 1 in
 object(self)
  inherit Simulated_network.switch
    ~name ~hublet_no ~last_user_visible_port_index ~unexpected_death_callback () as super

  method device_type = "world_gateway"

  val mutable slirpvde_process = None
  method private get_slirpvde_process =
    match slirpvde_process with
    | Some p -> p
    | None -> assert false

  method private terminate_slirpvde_process =
   (try self#get_slirpvde_process#terminate with _ -> ())

  (* Redefined: *)
  method destroy =
   self#terminate_slirpvde_process;
   super#destroy

  (* Redefined: *)
  method gracefully_shutdown =
   self#terminate_slirpvde_process;
   super#gracefully_shutdown

  (* Redefined: *)
  method shutdown =
   self#terminate_slirpvde_process;
   super#shutdown

  (* Redefined: *)
  method startup =
   super#startup;
   self#get_slirpvde_process#spawn

  initializer

    let last_reserved_port = user_port_no in
    let slirpvde_socket = (self#get_hublet_process last_reserved_port)#get_socket_name in

    slirpvde_process <-
      Some (new Simulated_network.slirpvde_process
              ~existing_socket_name:slirpvde_socket
              ~network:network_address
              ?dhcp:(Option.of_bool dhcp_enabled)
              ~unexpected_death_callback:self#execute_the_unexpected_death_callback
             ())

end;;

end (* module Simulation_level *)

(** Just for testing: *)
let test = Dialog_add_or_update.make
