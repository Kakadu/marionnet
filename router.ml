(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2009, 2010  Jean-Vincent Loddo
   Copyright (C) 2009, 2010  Université Paris 13

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

(* Router related constants: *)
(* TODO: make it configurable! *)
module Const = struct
 let port_no_default = 4
 let port_no_min = 4
 let port_no_max = 16

 let port_0_ip_config_default = Initialization.router_port0_default_ipv4_config
 let memory_default = 48
end


(* The type of data returned by the dialog: *)
module Data = struct
type t = {
  name               : string;
  label              : string;
  port_0_ip_config   : Ipv4.config;
  port_no            : int;
  distribution       : string;          (* epithet *)
  variant            : string option;
  kernel             : string;          (* epithet *)
  show_unix_terminal : bool;
  old_name           : string;
  }

let to_string t = "<obj>" (* TODO? *)
end (* Data *)

module Make_menus (Params : sig
  val st      : State.globalState
  val packing : [ `toolbar of GButton.toolbar | `menu_parent of Menu_factory.menu_parent ]
 end) = struct

  open Params

  module Toolbar_entry = struct
   let imagefile = "ico.router.palette.png"
   let tooltip   = (s_ "Router")
   let packing   = Params.packing
  end

  module Add = struct
    include Data

    let key = Some GdkKeysyms._R

    let ok_callback t = Gui_bricks.Ok_callback.check_name t.name t.old_name st#network#name_exists t

    let dialog () =
      let name = st#network#suggestedName "R" in
      Dialog_add_or_update.make
        ~title:(s_ "Add router") ~name ~ok_callback ()

    let reaction {
         name = name;
         label = label;
         port_0_ip_config = port_0_ip_config;
         port_no = port_no;
         distribution = distribution;
         variant = variant;
	 kernel = kernel;
         show_unix_terminal = show_unix_terminal;
         old_name = _ ;
         }
      =
      let action () = ignore (
        new User_level_router.router (* defined later with WHERE *)
          ~network:st#network
          ~name
          ~label
          ~port_0_ip_config
          ~epithet:distribution
          ?variant:variant
          ~kernel
	  ~port_no
 	  ~show_unix_terminal
          ())
      in
      st#network_change action ();

  end (* Add *)

  module Properties = struct
    include Data
    let dynlist () = st#network#get_devices_that_can_startup ~devkind:`Router ()

    let dialog name () =
     let r = (st#network#get_device_by_name name) in
     let r = ((Obj.magic r):> User_level_router.router) in
     let title = (s_ "Modify router")^" "^name in
     let label = r#get_label in
     let distribution = r#get_epithet in
     let variant = r#get_variant in
     let kernel = r#get_kernel in
     let show_unix_terminal = r#get_show_unix_terminal in
     let port_no = r#get_port_no in
     let port_0_ip_config = r#get_port_0_ip_config in
     (* The user cannot remove receptacles used by a cable. *)
     let port_no_min = st#network#port_no_lower_of (r :> User_level.node)
     in
     Dialog_add_or_update.make
       ~title ~name ~label ~distribution ?variant ~show_unix_terminal
       ~port_no ~port_no_min
       ~port_0_ip_config
       ~kernel
       ~updating:() (* the user cannot change the distrib & variant *)
       ~ok_callback:Add.ok_callback  ()

    let reaction {
         name = name;
         label = label;
         port_0_ip_config = port_0_ip_config;
         port_no = port_no;
	 kernel = kernel;
         show_unix_terminal = show_unix_terminal;
         old_name = old_name;
         }
      =
      let d = (st#network#get_device_by_name old_name) in
      let r = ((Obj.magic d):> User_level_router.router) in
      let action () =
        r#update_router_with
          ~name ~label ~port_0_ip_config ~port_no ~kernel ~show_unix_terminal
      in
      st#network_change action ();

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
      let d = (st#network#get_device_by_name name) in
      let r = ((Obj.magic d):> User_level_router.router) in
      let action () = r#destroy in
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
    let dynlist () = st#network#get_devices_that_can_gracefully_shutdown ~devkind:`Router ()
    let dialog = Menu_factory.no_dialog_but_simply_return_name
    let reaction name = (st#network#get_device_by_name name)#gracefully_shutdown

  end

  module Suspend = struct
    type t = string (* just the name *)
    let to_string = (Printf.sprintf "name = %s\n")
    let dynlist () = st#network#get_devices_that_can_suspend ~devkind:`Router ()
    let dialog = Menu_factory.no_dialog_but_simply_return_name
    let reaction name = (st#network#get_device_by_name name)#suspend

  end

  module Resume = struct
    type t = string (* just the name *)

    let to_string = (Printf.sprintf "name = %s\n")
    let dynlist () = st#network#get_devices_that_can_resume ~devkind:`Router ()
    let dialog = Menu_factory.no_dialog_but_simply_return_name
    let reaction name = (st#network#get_device_by_name name)#resume

  end

 module Create_entries =
  Gui_toolbar_COMPONENTS_layouts.Layout_for_network_node (Params) (Toolbar_entry) (Add) (Properties) (Remove) (Startup) (Stop) (Suspend) (Resume)

 (* Subscribe this kind of component to the network club: *)
 st#network#subscribe_a_try_to_add_procedure Eval_forest_child.try_to_add_router;

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
 ?(port_0_ip_config=Const.port_0_ip_config_default)
 ?(port_no=Const.port_no_default)
 ?(port_no_min=Const.port_no_min)
 ?(port_no_max=Const.port_no_max)
 ?distribution
 ?variant
 ?kernel
 ?(updating:unit option)
 ?(show_unix_terminal=false)
 ?(help_callback=help_callback) (* defined backward with "WHERE" *)
 ?(ok_callback=(fun data -> Some data))
 ?(dialog_image_file=Initialization.Path.images^"ico.router.dialog.png")
 () :'result option =
  let old_name = name in
  let ((b1,b2,b3,b4),b5) = port_0_ip_config  in
  let vm_installations =  Disk.get_router_installations () in
  let (w,_,name,label) =
    Gui_bricks.Dialog_add_or_update.make_window_image_name_and_label
      ~title
      ~image_file:dialog_image_file
      ~image_tooltip:(s_ "Router")
      ~name
      ~name_tooltip:(s_ "Router name. This name must be unique in the virtual network. Suggested: R1, R2, ...")
      ?label
      ()
  in
  let ((s1,s2,s3,s4,s5), port_no, distribution, variant, kernel, show_unix_terminal) =
    let vbox = GPack.vbox ~homogeneous:false ~border_width:20 ~spacing:10 ~packing:w#vbox#add () in
    let form =
      Gui_bricks.make_form_with_labels
        ~packing:vbox#add
        [(s_ "Ports number");
         (s_ "Port 0 address");
         (s_ "Distribution");
         (s_ "Variant");
         (s_ "Kernel");
         (s_ "Show unix terminal");
         ]
    in
    form#add_section ~no_line:() "Hardware";
    let port_no =
      Gui_bricks.spin_byte ~lower:port_no_min ~upper:port_no_max ~step_incr:2
      ~packing:(form#add_with_tooltip (s_ "Number of router ports" )) port_no
    in
    let port_0_ip_config =
      Gui_bricks.spin_ipv4_address_with_cidr_netmask
        ~packing:(form#add_with_tooltip
                    ~just_for_label:()
                    (s_ "IPv4 configuration of the first router port (0)"))
        b1 b2 b3 b4 b5
    in
    form#add_section "Software";
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
    form#add_section "Access";
    let show_unix_terminal =
      GButton.check_button
        ~active:show_unix_terminal
        ~packing:(form#add_with_tooltip (s_ "Do you want access the router also by a Unix terminal?" ))
        ()
    in
    (port_0_ip_config, port_no, distribution, variant, kernel, show_unix_terminal)
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
    let variant = match variant with
    | "none" -> None
    | x      -> Some x
    in
    let kernel = kernel#selected in
    let show_unix_terminal = show_unix_terminal#active in
      { Data.name = name;
        Data.label = label;
        Data.port_0_ip_config = port_0_ip_config;
        Data.port_no = port_no;
        Data.distribution = distribution;
        Data.variant = variant;
        Data.kernel = kernel;
        Data.show_unix_terminal = show_unix_terminal;
        Data.old_name = old_name;
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

(*-----*)
  WHERE
(*-----*)

module Eval_forest_child = struct
 let try_to_add_router (network:User_level.network) ((root,childs):Xforest.tree) =
  try
   (match root with
    | ("router", attrs) ->
    	let name  = List.assoc "name" attrs in
	let port_no = int_of_string (List.assoc "port_no" attrs) in
        Log.printf "Importing router \"%s\" with %d ports...\n" name port_no;
	let x = new User_level_router.router ~network ~name ~port_no () in
	x#from_tree ("router", attrs) childs;
        Log.printf "Router \"%s\" successfully imported.\n" name;
        true

   (* backward compatibility *)
   | ("device", attrs) ->
      let name  = List.assoc "name" attrs in
      let port_no = int_of_string (List.assoc "eth" attrs) in
      let kind = List.assoc "kind" attrs in
      (match kind with
      | "router" ->
          Log.printf "Importing router \"%s\" with %d ports...\n" name port_no;
	  let r = new User_level_router.router ~network ~name ~port_no () in
	  let x = (r :> User_level.node_with_ledgrid_and_defects) in
	  x#from_tree ("device", attrs) childs ;
          Log.printf "Router \"%s\" successfully imported.\n" name;
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


module User_level_router = struct

class router
  ~(network:User_level.network)
  ~name
  ?(port_0_ip_config=Const.port_0_ip_config_default)
  ?label
  ?epithet
  ?variant
  ?kernel
  ?(show_unix_terminal=false)
  ?terminal
  ~port_no
  ()
  =
  let vm_installations = Disk.get_router_installations () in
  let network_alias = network in
  (* The ifconfig treeview wants a port 0 configuration at creation time:*)
  let ifconfig_port_row_completions =
     let (ipv4,cidr) = port_0_ip_config in (* the class parameter *)
     let netmask_string = (Ipv4.to_string (Ipv4.netmask_of_cidr cidr)) in
     [ ("port0",
	    [ "IPv4 address", Treeview.Row_item.String (Ipv4.to_string ipv4);
	      "IPv4 netmask", Treeview.Row_item.String netmask_string; ])
     ]
  in

  object (self) inherit OoExtra.destroy_methods ()

  inherit User_level.node_with_ledgrid_and_defects
    ~network
    ~name ?label ~devkind:`Router
    ~port_no
    ~port_no_min:Const.port_no_min
    ~port_no_max:Const.port_no_max
    ~port_prefix:"port"
    ()
    as self_as_node_with_ledgrid_and_defects

  inherit User_level.virtual_machine_with_history_and_ifconfig
    ~network:network_alias
    ?epithet ?variant ?kernel ?terminal
    ~history_icon:"router"
    ~ifconfig_device_type:"router"
    ~ifconfig_port_row_completions
    ~vm_installations
    ()
    as self_as_virtual_machine_with_history_and_ifconfig

  method polarity = User_level.MDI
  method string_of_devkind = "router"
  method ledgrid_label = "Router"
  method defects_device_type = "router"

  method dotImg iconsize =
   let imgDir = Initialization.Path.images in
   (imgDir^"ico.router."^(self#string_of_simulated_device_state)^"."^iconsize^".png")

  (** Get the full host pathname to the directory containing the guest hostfs
      filesystem: *)
  method hostfs_directory_pathname =
    let d = ((Option.extract !simulated_device) :> User_level.node Simulation_level.router) in
    d#hostfs_directory_pathname

  val mutable show_unix_terminal : bool = show_unix_terminal
  method get_show_unix_terminal = show_unix_terminal
  method set_show_unix_terminal x = show_unix_terminal <- x

  (** Create the simulated device *)
  method private make_simulated_device =
    let id = self#id in
    let cow_file_name, dynamically_get_the_cow_file_name_source =
      self#create_cow_file_name_and_thunk_to_get_the_source
    in
    let () =
     Log.printf
       "About to start the router %s\n  with filesystem: %s\n  cow file: %s\n  kernel: %s\n"
       self#name
       self#get_filesystem_file_name
       cow_file_name
       self#get_kernel_file_name
    in
    new Simulation_level.router
      ~parent:self
      ~kernel_file_name:self#get_kernel_file_name
      ~filesystem_file_name:self#get_filesystem_file_name
      ~dynamically_get_the_cow_file_name_source
      ~cow_file_name
      ~states_directory:(self#get_states_directory)
      ~ethernet_interface_no:self#get_port_no
      ~umid:self#get_name
      ~id
      ~show_unix_terminal:self#get_show_unix_terminal
      ~unexpected_death_callback:self#destroy_because_of_unexpected_death
      ()


  (** Here we also have to manage cow files... *)
  method private gracefully_shutdown_right_now =
    self_as_node_with_ledgrid_and_defects#gracefully_shutdown_right_now;
    (* We have to manage the hostfs stuff (when in exam mode) and
       destroy the simulated device, so that we can use a new cow file the next time: *)
    Log.printf "Calling hostfs_directory_pathname on %s...\n" self#name;
    let hostfs_directory_pathname = self#hostfs_directory_pathname in
    Log.printf "Ok, we're still alive\n";
    (* If we're in exam mode then make the report available in the texts treeview: *)
    (if Command_line.are_we_in_exam_mode then begin
      let treeview_documents = Treeview_documents.extract () in
      Log.printf "Adding the report on %s to the texts interface\n" self#name;
      treeview_documents#import_report
	~machine_or_router_name:self#name
	~pathname:(hostfs_directory_pathname ^ "/report.html")
	();
      Log.printf "Added the report on %s to the texts interface\n" self#name;
    end);
    (* ...And destroy, so that the next time we have to re-create the process command line
	can use a new cow file (see the make_simulated_device method) *)
    self#destroy_right_now


  (** Here we also have to manage LED grids and, for routers, cow files: *)
  method private poweroff_right_now =
    self_as_node_with_ledgrid_and_defects#poweroff_right_now;
    (* Destroy, so that the next time we have to re-create a simulated device,
       and we start with a new cow: *)
    self#destroy_right_now

  method to_tree =
   Forest.tree_of_leaf ("router", [
      ("name"     ,  self#get_name );
      ("label"   ,   self#get_label);
      ("distrib"  ,  self#get_epithet  );
      ("variant"  ,  self#get_variant_as_string);
      ("kernel"   ,  self#get_kernel   );
      ("show_unix_terminal" , string_of_bool (self#get_show_unix_terminal));
      ("terminal" ,  self#get_terminal );
      ("port_no"  ,  (string_of_int self#get_port_no))  ;
      ])

 (** A machine has just attributes (no childs) in this version. *)
 method eval_forest_attribute = function
  | ("name"     , x ) -> self#set_name x
  | ("label"    , x ) -> self#set_label x
  | ("distrib"  , x ) -> self#set_epithet x
  | ("variant"  , "") -> self#set_variant None
  | ("variant"  , x ) -> self#set_variant (Some x)
  | ("kernel"   , x ) -> self#set_kernel x
  | ("show_unix_terminal", x ) -> self#set_show_unix_terminal (bool_of_string x)
  | ("terminal" , x ) -> self#set_terminal x
  | ("port_no"  , x ) -> self#set_port_no  (int_of_string x)
  | _ -> () (* Forward-comp. *)

 method private get_assoc_list_from_ifconfig ~key =
   List.map
     (fun i -> (i,network#ifconfig#get_port_attribute_by_index self#get_name i key))
     (ListExtra.range 0 (self#get_port_no - 1))

 method get_mac_addresses  = self#get_assoc_list_from_ifconfig ~key:"MAC address"
 method get_ipv4_addresses = self#get_assoc_list_from_ifconfig ~key:"IPv4 address"
(* other: "MTU", "IPv4 netmask", "IPv4 broadcast", "IPv6 address" *)

 method get_port_0_ip_config =
  let name = self#get_name in
  let ipv4 =
    Ipv4.of_string
      (network#ifconfig#get_port_attribute_by_index
         name 0 "IPv4 address")
  in
  let cidr =
    Ipv4.cidr_of_netmask
      (Ipv4.netmask_of_string
	 (network#ifconfig#get_port_attribute_by_index
	  name 0 "IPv4 netmask"))
    in
  (ipv4, cidr)


 method set_port_0_ipv4_address (ipv4:Ipv4.t) =
   network#ifconfig#set_port_string_attribute_by_index
     self#get_name 0 "IPv4 address"
     (Ipv4.to_string ipv4);

 method set_port_0_ipv4_netmask_by_cidr cidr =
   let netmask_as_string = Ipv4.to_string (Ipv4.netmask_of_cidr cidr) in
   network#ifconfig#set_port_string_attribute_by_index
     self#get_name 0 "IPv4 netmask"
     netmask_as_string

 method set_port_0_ip_config port_0_ip_config =
   let (ipv4,cidr) = port_0_ip_config in
   self#set_port_0_ipv4_address ipv4;
   self#set_port_0_ipv4_netmask_by_cidr cidr;

 method update_router_with ~name ~label ~port_0_ip_config ~port_no ~kernel ~show_unix_terminal =
   (* first action: *)
   self_as_virtual_machine_with_history_and_ifconfig#update_virtual_machine_with ~name ~port_no kernel;
   (* then we can set the object property "name" (read by #get_name): *)
   self_as_node_with_ledgrid_and_defects#update_with ~name ~label ~port_no;
   self#set_port_0_ip_config port_0_ip_config;
   self#set_show_unix_terminal show_unix_terminal;

end;;

end (* module User_level_router *)

(*-----*)
  WHERE
(*-----*)

module Simulation_level = struct
(** A router: just a [machine_or_router] with [router = true] *)
class ['parent] router =
  fun ~(parent:'parent)
      ~dynamically_get_the_cow_file_name_source
      ~(cow_file_name)
      ~states_directory
      ~(kernel_file_name)
      ~(filesystem_file_name)
      ~(ethernet_interface_no)
      ?umid
      ~id
      ~show_unix_terminal
      ~unexpected_death_callback
      () ->
object(self)
  inherit ['parent] Simulation_level.machine_or_router
      ~parent
      ~router:true
      ~filesystem_file_name(* :"/usr/marionnet/filesystems/router.debian.lenny.sid.fs" *)
      ~kernel_file_name
      ~dynamically_get_the_cow_file_name_source
      ~cow_file_name
      ~states_directory
      ~ethernet_interface_no
      ~memory:Const.memory_default
      ?umid
      (* Change this when debugging the router device *)
      ~console:"none" (* To do: this should be "none" for releases and "xterm" for debugging *)
      ~id
      ~show_unix_terminal
      ~xnest:false
      ~unexpected_death_callback
      ()
      as super
  method device_type = "router"
end

end (* module Simulation_level *)


(** Just for testing: *)
let test = Dialog_add_or_update.make
