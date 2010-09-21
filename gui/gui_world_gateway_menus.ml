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

module Make (State : sig val st:State.globalState end) = struct

  open State

  module Toolbar_entry = struct
   let imagefile = "ico.world_gateway.palette.png"
   let tooltip   = (s_ "World gateway (router)")
  end

  module Add = struct
    type t = [ `name of string                  ] *
	     [ `label of string                 ] *
	     [ `network_config of Ipv4.config   ] *
	     [ `dhcp_enabled   of bool          ] *
	     [ `user_port_no   of int           ] *
	     [ `oldname        of string        ]

    let to_string t = "<obj>" (* TODO *)
    let key      = Some GdkKeysyms._G

    let ok_callback ((((`name name),_,_,_,_,_) as g):t) =
      if not (StrExtra.wellFormedName name)
        then None   (* refused *)
        else Some g (* accepted *)

    let help_callback () = () (* TODO *)

    let dialog () =
      let name = st#network#suggestedName "G" in
      let s_ = Gettext.s_ in
      Gui_world_gateway_dialog.make
        ~title:(s_ "Add world gateway") ~s_ ~name ~ok_callback ~help_callback ()

    let network_address_of_config config =
      let ((i1,i2,i3,_),_) = config in
      Printf.sprintf "%i.%i.%i.%i" i1 i2 i3 0
   
    let reaction : t -> unit =
     function
      (`name name,
       `label label,
       `network_config network_config,
       `dhcp_enabled dhcp_enabled,
       `user_port_no user_port_no,
       `oldname _)
      ->
      let g =
        new Mariokit.Netmodel.world_gateway
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
      st#network_change st#network#add_world_gateway g;
  end

  module Properties = struct
    type t = Add.t
    let to_string = Add.to_string

    let dynlist =
     fun () -> List.filter
                  (fun x -> (st#network#get_world_gateway_by_name x)#can_startup)
                  (st#network#get_world_gateway_names)

    let dialog name () =
     let g = (st#network#get_world_gateway_by_name name) in
     let title = (s_ "Modify world gateway")^" "^name in
     let label = g#get_label in
     (* With the current version of slirpvde i4 is always 1 and cidr is 24 *)
     let network_config =
       let fixed_cidr = 24 in
       ((Ipv4.ipv4_of_string g#get_network_address), fixed_cidr)
     in
     let dhcp_enabled = g#get_dhcp_enabled in
     let user_port_no = g#get_eth_number in
     Gui_world_gateway_dialog.make
       ~title ~s_ ~name ~label ~network_config ~dhcp_enabled ~user_port_no
       ~ok_callback:Add.ok_callback ~help_callback:Add.help_callback ()


    let reaction : t -> unit =
     function
      (`name name,
       `label label,
       `network_config network_config,
       `dhcp_enabled dhcp_enabled,
       `user_port_no user_port_no,
       `oldname oldname)
      ->
      let defects = Defects_interface.get_defects_interface () in
      let d = st#network#get_world_gateway_by_name oldname in
      (match name = oldname with
      | true -> ()
      | false ->
          begin
            d#destroy; (* VERIFICARE *)
            st#network#ledgrid_manager#destroy_device_ledgrid ~id:(d#id) ();
            st#network#change_node_name oldname name  ;
            Filesystem_history.rename_device oldname name;
            defects#rename_device oldname name;
            st#network#make_device_ledgrid (d :> Mariokit.Netmodel.device);
          end
      | _ -> ()
      );
      d#set_label label;
      d#set_network_address (Add.network_address_of_config network_config);
      d#set_dhcp_enabled dhcp_enabled;
      d#set_eth_number ~prefix:"port" user_port_no;
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
