(* This file is part of Marionnet, a virtual network laboratory
(*    Copyright (C) 2010  Jean-Vincent Loddo *)

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

(** Gui-related stuff for the user-level component "router". *)

(* The module containing the add/update dialog is defined later,
   using the syntax extension "where" *)
#load "where_p4.cmo"
;;

(* The type of data returned by the dialog: *)
module Data = struct
type t = {
  name               : string;
  label              : string;
  port_0_ip_config   : Ipv4.config;
  port_no            : int;
  distribution       : string;          (* epithet *)
  variant_name       : string option;
  variant_realpath   : string option;   
  kernel             : string;          (* epithet *)
  show_unix_terminal : bool;
  oldname            : string;
  }

let to_string t = "<obj>" (* TODO? *)
end (* Data *)

module Make_menus (State : sig val st:State.globalState end) = struct

  open State

  module Toolbar_entry = struct
   let imagefile = "ico.router.palette.png"
   let tooltip   = (s_ "Router")
  end

  module Add = struct
    include Data

    let key = Some GdkKeysyms._R

    let ok_callback t =
      if not (StrExtra.wellFormedName t.name)
        then None   (* refused *)
        else Some t (* accepted *)

    let dialog () =
      let name = st#network#suggestedName "R" in
      Dialog_add_or_update.make
        ~title:(s_ "Add router") ~name ~ok_callback ()

   
    let reaction : t -> unit =
     function
      { name = name;
        label = label;
        port_0_ip_config = port_0_ip_config;
        port_no = port_no;
        distribution = distribution;
        variant_name = variant_name;
	variant_realpath = variant_realpath;
	kernel = kernel;
        show_unix_terminal = show_unix_terminal;
        oldname = _ }
      ->
      let details = Network_details_interface.get_network_details_interface () in
      let defects = Defects_interface.get_defects_interface () in
      let port_row_completions =
        let (ipv4,cidr) = port_0_ip_config in
        let netmask_string = (Ipv4.string_of_ipv4 (Ipv4.netmask_of_cidr cidr)) in
        [ ("port0",
              [ "IPv4 address", Row_item.String (Ipv4.string_of_ipv4 ipv4);
                "IPv4 netmask", Row_item.String netmask_string; ])
        ]
      in
      details#add_device ~port_row_completions name "router" port_no;
      defects#add_device name "router" port_no;
      let router =
        new Mariokit.Netmodel.router
          ~network:st#network
          ~name
          ~label
          ~epithet:distribution
          ?variant:variant_name
          ~kernel
	  ~port_no
 	  ~show_unix_terminal
          ()
      in
      st#network_change st#network#add_device (router :> Mariokit.Netmodel.device);
      if (Filesystem_history.number_of_states_with_name name) > 0
      then ()
      else begin (* not after load *)
       Filesystem_history.add_device
          ~name
          ~prefixed_filesystem:("router-"^distribution)
          ?variant:variant_name
          ?variant_realpath
          ~icon:"router"
          ()
       end



  end (* Add *)

  module Properties = struct
    include Data

    let dynlist =
     fun () -> List.filter
                  (fun x -> (st#network#get_device_by_name x)#can_startup)
                  (st#network#get_router_names)

    let dialog name () =
     let details = Network_details_interface.get_network_details_interface () in
     let r = (st#network#get_device_by_name name) in
     let r = ((Obj.magic r):> Mariokit.Netmodel.router) in
     let title = (s_ "Modify router")^" "^name in
     let label = r#get_label in
     let distribution = r#get_epithet in
     let variant = r#get_variant in
     let kernel = r#get_kernel in
     let show_unix_terminal = r#get_show_unix_terminal in
     let port_no = r#get_eth_number in
     let ipv4 = Ipv4.ipv4_of_string
        (details#get_port_attribute_by_index name 0 "IPv4 address")
     in
     let (_, cidr ) = Ipv4.netmask_with_cidr_of_string
        (details#get_port_attribute_by_index name 0 "IPv4 netmask")
     in
     let port_0_ip_config = (ipv4,cidr) in
     (* The user cannot remove receptacles used by a cable. *)
     let port_no_lower = st#network#port_no_lower_of r#get_name
     in
     Dialog_add_or_update.make
       ~title ~name ~label ~distribution ?variant ~show_unix_terminal ~port_no
       ~port_0_ip_config
       ~port_no_lower
       ~kernel
       ~updating:() (* the user cannot change the distrib & variant *)
       ~ok_callback:Add.ok_callback  ()


    let reaction : t -> unit =
     function
      { name = name;
        label = label;
        port_0_ip_config = (ipv4,cidr);
        port_no = port_no;
	kernel = kernel;
        show_unix_terminal = show_unix_terminal;
        oldname = oldname }
      ->
      let details = Network_details_interface.get_network_details_interface () in
      let defects = Defects_interface.get_defects_interface () in
      (* name *)
      let d = st#network#get_device_by_name oldname in
      let r = ((Obj.magic d):> Mariokit.Netmodel.router) in
      (match name = oldname with
      | true -> ()
      | false ->
          begin
           d#destroy;
           st#network#ledgrid_manager#destroy_device_ledgrid ~id:(d#id) ();
           st#network#change_node_name oldname name;
           details#rename_device oldname name;
           defects#rename_device oldname name;
           Filesystem_history.rename_device oldname name;
           st#network#make_device_ledgrid d;
          end
       );
      (* label *)
      d#set_label label;
      (* port_0_ip_config *)
      let netmask_string = (Ipv4.string_of_ipv4 (Ipv4.netmask_of_cidr cidr)) in
      details#set_port_string_attribute_by_index name 0 "IPv4 address" (Ipv4.string_of_ipv4 ipv4);
      details#set_port_string_attribute_by_index name 0 "IPv4 netmask" netmask_string;
      (* port_no *)
      d#set_eth_number ~prefix:"port" port_no;
      details#update_port_no name port_no;
      defects#update_port_no name port_no;
      (* kernel *)
      r#set_kernel kernel;
      (* show_terminal *)
      r#set_show_unix_terminal show_unix_terminal;
      (* refresh and return *)
      st#refresh_sketch () ;
      st#update_cable_sensitivity ()

  end (* Properties *)

  module Remove = struct
    type t = string (* just the name *)
    let to_string = (Printf.sprintf "name = %s\n")

    let dynlist = Properties.dynlist

    let dialog name () =
      Gui_bricks.Dialog.yes_or_cancel_question
        ~title:(s_ "Remove")
        ~markup:(Printf.sprintf (f_ "Are you sure that you want to remove %s\nand all the cables connected to this %s?") name (s_ "router"))
        ~context:name
        ()

    let reaction name =
      let details = Network_details_interface.get_network_details_interface () in
      let defects = Defects_interface.get_defects_interface () in
      st#network_change st#network#del_device name;
      details#remove_device name;
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
       (st#network#get_router_names)

    let dialog = Menu_factory.no_dialog_but_simply_return_name
    let reaction name = (st#network#get_device_by_name name)#gracefully_shutdown

  end

  module Suspend = struct
    type t = string (* just the name *)
    let to_string = (Printf.sprintf "name = %s\n")

    let dynlist () =
      List.filter
       (fun x -> (st#network#get_device_by_name x)#can_suspend)
       (st#network#get_router_names)

    let dialog = Menu_factory.no_dialog_but_simply_return_name
    let reaction name = (st#network#get_device_by_name name)#suspend

  end

  module Resume = struct
    type t = string (* just the name *)
    let to_string = (Printf.sprintf "name = %s\n")

    let dynlist () =
      List.filter
       (fun x -> (st#network#get_device_by_name x)#can_resume)
       (st#network#get_router_names)

    let dialog = Menu_factory.no_dialog_but_simply_return_name
    let reaction name = (st#network#get_device_by_name name)#resume

  end

 module Create_entries =
  Gui_toolbar_COMPONENTS_layouts.Layout_for_network_node (State) (Toolbar_entry) (Add) (Properties) (Remove) (Startup) (Stop) (Suspend) (Resume)

end

(*-----*)
  WHERE
(*-----*)

module Dialog_add_or_update = struct

(* This function may be useful for testing the widget creation without
   recompiling the whole project. *)
let make
 ?(title="Add a router")
 ?(name="")
 ?label
 ?(port_0_ip_config:Ipv4.config option)
 ?(port_no=4)
 ?(port_no_lower=4)
 ?distribution
 ?variant
 ?kernel
 ?(updating:unit option)
 ?(show_unix_terminal=false)
 ?(help_callback=help_callback) (* defined backward with "WHERE" *)
 ?(ok_callback=(fun data -> Some data))
 ?(dialog_image_file=Initialization.Path.images^"ico.router.dialog.png")
 () :'result option =
  let oldname = name in
  let ((b1,b2,b3,b4),b5) = match port_0_ip_config  with
   | Some x -> x
   | None   -> ((192,168,1,254), 24)
  in
  let vm_installations =  Disk.get_router_installations () in
  let w = GWindow.dialog ~destroy_with_parent:true ~title ~modal:true ~position:`CENTER () in
  Gui_bricks.set_marionnet_icon w;
  let tooltips = Gui_bricks.make_tooltips_for_container w in

  let (name,label) =
    let hbox = GPack.hbox ~homogeneous:true ~border_width:20 ~spacing:10 ~packing:w#vbox#add () in
    let image = GMisc.image ~file:dialog_image_file ~xalign:0.5 ~packing:hbox#add () in
    tooltips image#coerce (s_ "Router");
    let vbox = GPack.vbox ~spacing:10 ~packing:hbox#add () in
    let name  =
      let tooltip = (s_ "Router name. This name must be unique in the virtual network. Suggested: R1, R2, ...") in
      Gui_bricks.entry_with_label ~tooltip ~packing:vbox#add ~entry_text:name  (s_ "Name")
    in
    let label =
      let tooltip = (s_ "Label to be written in the network sketch, next to the element icon." ) in
      Gui_bricks.entry_with_label ~tooltip ~packing:vbox#add ?entry_text:label (s_ "Label")
    in
   (name,label)
  in

  ignore (GMisc.separator `HORIZONTAL ~packing:w#vbox#add ());

  let ((s1,s2,s3,s4,s5), port_no, distribution, variant, kernel, show_unix_terminal) =
    let vbox = GPack.vbox ~homogeneous:false ~border_width:20 ~spacing:10 ~packing:w#vbox#add () in
    let form =
      Gui_bricks.make_form_with_labels
        ~packing:vbox#add
        [(s_ "Port 0 address");
         (s_ "Ports number");
         (s_ "Distribution");
         (s_ "Variant");
         (s_ "Kernel");
         (s_ "Show unix terminal");
         ]
    in
    let port_0_ip_config =
      Gui_bricks.spin_ipv4_address_with_cidr_netmask
        ~packing:(form#add_with_tooltip (s_ "IPv4 configuration of the first router port (0)"))
        b1 b2 b3 b4 b5
    in
    let port_no =
      Gui_bricks.spin_byte ~lower:port_no_lower ~upper:16 (* TODO? *) ~step_incr:4
      ~packing:(form#add_with_tooltip (s_ "Number of router ports" )) port_no
    in
    let (distribution, kernel) =
      let packing_distribution =
        form#add_with_tooltip
          (s_ "GNU/Linux distribution installed on the router." )
      in
      let packing_variant      =
        form#add_with_tooltip
          (s_ "Initial hard disk state. The router will start by default with this variant of the chosen distribution." )
      in
      let packing_kernel =
        form#add_with_tooltip
          (s_ "Linux kernel version used for this router." )
      in
      let packing = (packing_distribution, packing_variant, packing_kernel) in 
      Gui_bricks.make_combo_boxes_of_vm_installations
        ?distribution ?variant ?kernel ?updating
        ~packing
        vm_installations
    in
    let show_unix_terminal =
      GButton.check_button
        ~active:show_unix_terminal
        ~packing:(form#add_with_tooltip (s_ "Do you want access the router also by a Unix terminal?" ))
        ()
    in
    tooltips form#coerce (s_ "Router configuration" );
    (port_0_ip_config, port_no, distribution, variant, kernel, show_unix_terminal)
  in
  let () =
    tooltips s1#coerce (s_ "First octet of the IPv4 address" );
    tooltips s2#coerce (s_ "Second octet of the IPv4 address" );
    tooltips s3#coerce (s_ "Third octet of the IPv4 address" );
    tooltips s4#coerce (s_ "Fourth octet of the IPv4 address" );
    tooltips s5#coerce (s_ "Netmask (CIDR notation)" );
  in
  let get_widget_data () :'result =
    let name = name#text in
    let label = label#text in
    let port_0_ip_config =
      let s1 = int_of_float s1#value in
      let s2 = int_of_float s2#value in
      let s3 = int_of_float s3#value in
      let s4 = int_of_float s4#value in
      let s5 = int_of_float s5#value in
      ((s1,s2,s3,s4),s5)
    in
    let port_no = int_of_float port_no#value in
    let variant       = distribution#slave#selected in
    let distribution  = distribution#selected in
    let (variant_name, variant_realpath) = match variant with
    | "none" -> (None  , None)
    | v      -> (Some v, Some ((vm_installations#variants_of distribution)#realpath_of_epithet v))
    in
    let kernel = kernel#selected in
    let show_unix_terminal = show_unix_terminal#active in
      { Data.name = name;
        Data.label = label;
        Data.port_0_ip_config = port_0_ip_config;
        Data.port_no = port_no;
        Data.distribution = distribution;
        Data.variant_name = variant_name;
        Data.variant_realpath = variant_realpath;
        Data.kernel = kernel;
        Data.show_unix_terminal = show_unix_terminal;
        Data.oldname = oldname;
        }
        
  in
  (* The result of make is the result of the dialog loop (of type 'result option): *)
  Gui_bricks.Dialog_run.ok_or_cancel w ~ok_callback ~help_callback ~get_widget_data ()


(*-----*)
  WHERE
(*-----*)

 let help_callback =
   let title = (s_ "ADD OR MODIFY A ROUTER") in
   let msg   = (s_ "\
In this dialog window you can define the name of an IP router \
and set many parameters for it:\n\n\
- Label: a string appearing near the router icon in the network graph; \
this field is exclusively for graphic purposes, is not taken in consideration \
for the configuration.\n\
- Nb of Ports: the number of ports of the router (default 4); this number must \
not be increased without a reason, because the number of processes needed for the \
device emulation is proportional to his ports number.\n\n\
The emulation of this device is realised with the program 'quagga' derived from \
the project 'zebra'.\n\n\
Every interface of the router can be configured in the tab \
'Interfaces'. Once started, the router will answer to the telnet \
protocol on every configured interface, on the following tcp ports:\n\n\
zebra\t\t2601/tcp\t\t# zebra vty\n\
ripd\t\t\t2602/tcp\t\t# RIPd vty\n\
ripngd\t\t2603/tcp\t\t# RIPngd vty\n\
ospfd\t\t2604/tcp\t\t# OSPFd vty\n\
bgpd\t\t2605/tcp\t\t# BGPd vty\n\
ospf6d\t\t2606/tcp\t\t# OSPF6d vty\n\
isisd\t\t\t2608/tcp\t\t# ISISd vty\n\n\
Password: zebra")
   in Simple_dialogs.help title msg ;;

end

(** Just for testing: *)
let test = Dialog_add_or_update.make
