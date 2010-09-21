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

(** Toolbar entry for the component 'gateway' *)

(* Shortcuts *)
type env  = string Environment.string_env
let mkenv = Environment.make_string_env
let env_to_string t = t#to_string (fun s->s)

module Make_menus (State : sig val st:State.globalState end) = struct

  open State

  module Toolbar_entry = struct
   let imagefile = "ico.world_gateway.palette.png"
   let tooltip   = (s_ "World gateway (router)")
  end

  module Add = struct
    type t = env
    let to_string = env_to_string
    let key      = Some GdkKeysyms._G

    let dialog   = let module M = Gui_world_gateway_dialog.Make (State) in
                   M.dialog ~title:(s_ "Add world gateway") ~update:None

    let reaction r =
      let defects = Defects_interface.get_defects_interface () in
      let name  = r#get "name" in
      let label = r#get "label" in
      let network_address = r#get "network_address" in
      let dhcp_enabled = bool_of_string (r#get "dhcp_enabled") in
      let user_port_no = int_of_string (r#get "user_port_no") in
      let g =
        new Mariokit.Netmodel.world_gateway
          ~network:st#network
          ~name
          ~label
          ~network_address
          ~dhcp_enabled
          ~user_port_no
          ()
      in
      (* The "world" port is hidden for defects: *)
      (** VERIFICARE il tipo "router"!! **)
      defects#add_device name "router" user_port_no;
      st#network_change st#network#add_world_gateway g;
  end

  module Properties = struct
    type t = env
    let to_string = env_to_string

    let dynlist =
     fun () -> List.filter
                  (fun x -> (st#network#get_world_gateway_by_name x)#can_startup)
                  (st#network#get_world_gateway_names)

    let dialog name =
     let m = (st#network#get_world_gateway_by_name name) in
     let title = (s_ "Modify world gateway") in
     let module M = Gui_world_gateway_dialog.Make (State) in M.dialog ~title:(title^" "^name) ~update:(Some m)

    let reaction r =
      let defects = Defects_interface.get_defects_interface () in
      let (name, oldname) = (r#get "name"), (r#get "oldname") in
      let d = st#network#get_world_gateway_by_name oldname in
      d#destroy; (* VERIFICARE *)
      st#network#ledgrid_manager#destroy_device_ledgrid ~id:(d#id) ();
      st#network#change_node_name oldname name  ;
      d#set_label (r#get "label");
      d#set_network_address (r#get "network_address");
      d#set_dhcp_enabled (bool_of_string (r#get "dhcp_enabled"));
      let user_port_no = int_of_string (r#get "user_port_no") in
      d#set_eth_number ~prefix:"port" user_port_no;
      st#refresh_sketch () ;
      st#network#make_device_ledgrid (d :> Mariokit.Netmodel.device);
      Filesystem_history.rename_device oldname name;
      defects#rename_device oldname name;
      defects#update_port_no name user_port_no;
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
        ~question:(Printf.sprintf (f_ "Are you sure that you want to remove %s\nand all the cables connected to this %s?") name (s_ "gateway"))

    let reaction r =
      let defects = Defects_interface.get_defects_interface () in
      if (r#get "answer")="yes" then
        let name   = r#get("name") in
        st#network_change st#network#del_world_gateway name;
        defects#remove_device name;
      else ()

  end

  module Startup = struct
    type t = env
    let to_string = env_to_string

    let dynlist    = Properties.dynlist
    let dialog     = Menu_factory.no_dialog
    let reaction r = (st#network#get_world_gateway_by_name (r#get "name"))#startup

  end

  module Stop = struct
    type t = env
    let to_string = env_to_string

    let dynlist () =
      List.filter
       (fun x -> (st#network#get_world_gateway_by_name x)#can_gracefully_shutdown)
       (st#network#get_world_gateway_names)

    let dialog = Menu_factory.no_dialog
    let reaction r = (st#network#get_world_gateway_by_name (r#get "name"))#gracefully_shutdown

  end

  module Suspend = struct
    type t = env
    let to_string = env_to_string

    let dynlist () =
      List.filter
       (fun x -> (st#network#get_world_gateway_by_name x)#can_suspend)
       (st#network#get_world_gateway_names)

    let dialog = Menu_factory.no_dialog
    let reaction r = (st#network#get_world_gateway_by_name (r#get "name"))#suspend

  end

  module Resume = struct
    type t = env
    let to_string = env_to_string

    let dynlist () =
      List.filter
       (fun x -> (st#network#get_world_gateway_by_name x)#can_resume)
       (st#network#get_world_gateway_names)

    let dialog = Menu_factory.no_dialog
    let reaction r = (st#network#get_world_gateway_by_name (r#get "name"))#resume

  end

 module Create_entries =
  Gui_toolbar_COMPONENTS_layouts.Layout_for_network_node (State) (Toolbar_entry) (Add) (Properties) (Remove) (Startup) (Stop) (Suspend) (Resume)

end
