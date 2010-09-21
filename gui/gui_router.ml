(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2009  Jean-Vincent Loddo

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

(** Toolbar entry for the component 'router' *)

(* Shortcuts *)
type env  = string Environment.string_env
let mkenv = Environment.make_string_env
let env_to_string t = t#to_string (fun s->s)

module Make_menus (State : sig val st:State.globalState end) = struct

  open State

  module Toolbar_entry = struct
   let imagefile = "ico.router.palette.png"
   let tooltip   = (s_ "Router")
  end

  module Add = struct
    type t = env
    let to_string = env_to_string
    let key      = Some GdkKeysyms._R

    let dialog   = let module M = Gui_dialog_ROUTER.Make (State) in
                   M.dialog ~title:(s_ "Add router") ~update:None

    let reaction r =
      let details = Network_details_interface.get_network_details_interface () in
      let defects = Defects_interface.get_defects_interface () in
      let (name,port_no) = (r#get "name"), (int_of_string (r#get "port_no")) in
      let port_row_completions =
       [ ("port0",
              [ "IPv4 address", Row_item.String (r#get "ip");
                "IPv4 netmask", Row_item.String (r#get "nmask"); ])
       ] in
      details#add_device ~port_row_completions name "router" port_no;
      defects#add_device name "router" port_no;
      let router =
        new Mariokit.Netmodel.router
          ~network:st#network
          ~name
          ~label:(r#get "label")
          ~epithet:  (r#get "distrib")
          ?variant:  (Option.of_fallible_application r#get "variant_name")
          ~kernel:   (r#get "kernel")
	  ~port_no
          ()
      in
(*    d#resolve_variant; (* don't store the variant as a symlink *) *)
      st#network_change st#network#add_device (router :> Mariokit.Netmodel.device);
      if (Filesystem_history.number_of_states_with_name name) > 0
      then ()
      else begin (* not after load *)
       Filesystem_history.add_device
          ~name
          ~prefixed_filesystem:("router-"^(router#get_epithet))
          ~root_export_dirname:(r#get "root_export_dirname")
          ~user_export_dirname:(r#get "user_export_dirname")
          ?variant:
             (Option.of_fallible_application r#get "variant_name")
          ?variant_realpath:
             (Option.of_fallible_application r#get "variant_realpath")
          ~icon:"router"
          ()
       end 

  end

  module Properties = struct
    type t = env
    let to_string = env_to_string

    let dynlist =
     fun () -> List.filter
                  (fun x -> (st#network#get_device_by_name x)#can_startup)
                  (st#network#get_router_names)

    let dialog name =
     let m = (st#network#get_device_by_name name) in
     let title = (s_ "Modify router") in
     let module M = Gui_dialog_ROUTER.Make (State) in M.dialog ~title:(title^" "^name) ~update:(Some m)

    let reaction r =
      let details = Network_details_interface.get_network_details_interface () in
      let defects = Defects_interface.get_defects_interface () in
      let (name,oldname,port_no) = (r#get "name"),(r#get "oldname"), (int_of_string (r#get "port_no")) in
      let d = st#network#get_device_by_name oldname in
      d#destroy;
      st#network#ledgrid_manager#destroy_device_ledgrid ~id:(d#id) ();
      st#network#change_node_name oldname name  ;
      d#set_label (r#get "label");
      d#set_eth_number ~prefix:"port" port_no;
      st#refresh_sketch () ;
      st#network#make_device_ledgrid d;
      Filesystem_history.rename_device oldname name;
      details#rename_device oldname name;
      details#update_port_no name port_no;
      details#set_port_string_attribute_by_index name 0 "IPv4 address" (r#get "ip");
      details#set_port_string_attribute_by_index name 0 "IPv4 netmask" (r#get "nmask");
      defects#rename_device oldname name;
      defects#update_port_no name port_no;
      st#update_cable_sensitivity ()

  end

  module Remove = struct
    type t = env
    let to_string = env_to_string

    let dynlist     = Properties.dynlist
    let dialog name = 
      Talking.EDialog.ask_question ~help:None ~cancel:false
        ~enrich:(mkenv [("name",name)])
        ~gen_id:"answer"
        ~title:(s_ "Remove")
        ~question:(Printf.sprintf (f_ "Are you sure that you want to remove %s\nand all the cables connected to this %s?") name (s_ "router"))

    let reaction r =
      let details = Network_details_interface.get_network_details_interface () in
      let defects = Defects_interface.get_defects_interface () in
      if (r#get "answer")="yes" then
        let name   = r#get("name") in
        st#network_change st#network#del_device name;
        details#remove_device name     ;
        defects#remove_device name     ;
      else ()

  end

  module Startup = struct
    type t = env
    let to_string = env_to_string

    let dynlist    = Properties.dynlist
    let dialog     = Menu_factory.no_dialog
    let reaction r = (st#network#get_device_by_name (r#get "name"))#startup

  end

  module Stop = struct
    type t = env
    let to_string = env_to_string

    let dynlist () =
      List.filter
       (fun x -> (st#network#get_device_by_name x)#can_gracefully_shutdown)
       (st#network#get_router_names)

    let dialog = Menu_factory.no_dialog
    let reaction r = (st#network#get_device_by_name (r#get "name"))#gracefully_shutdown
  
  end

  module Suspend = struct
    type t = env
    let to_string = env_to_string

    let dynlist () =
      List.filter
       (fun x -> (st#network#get_device_by_name x)#can_suspend)
       (st#network#get_router_names)

    let dialog = Menu_factory.no_dialog
    let reaction r = (st#network#get_device_by_name (r#get "name"))#suspend

  end

  module Resume = struct
    type t = env
    let to_string = env_to_string

    let dynlist () =
      List.filter
       (fun x -> (st#network#get_device_by_name x)#can_resume)
       (st#network#get_router_names)

    let dialog = Menu_factory.no_dialog
    let reaction r = (st#network#get_device_by_name (r#get "name"))#resume

  end

  module Ungracefully_stop = struct
    type t = env
    let to_string = env_to_string

    let dynlist =
     fun () -> List.filter
                  (fun x -> (st#network#get_device_by_name x)#can_poweroff)
                  (st#network#get_router_names)

    let dialog name =
      Talking.EDialog.ask_question ~help:None ~cancel:false
        ~enrich:(mkenv [("name",name)])
        ~gen_id:"answer"
        ~title:(s_ "Power-off")
        ~question:(Printf.sprintf (f_ "WARNING: you asked to brutally stop the router,\nlike in a power-off;\n however you can also gracefully stop routers.\n\nDo you want to power-off %s anyway?\n") name)

    let reaction r =
      if (r#get "answer") = "yes"
       then (st#network#get_device_by_name (r#get "name"))#poweroff
       else ()

  end

  module Create_entries =
  Gui_toolbar_COMPONENTS_layouts.Layout_for_network_node_with_state (State) (Toolbar_entry) (Add) (Properties) (Remove) (Startup) (Stop) (Suspend) (Resume) (Ungracefully_stop)

end
