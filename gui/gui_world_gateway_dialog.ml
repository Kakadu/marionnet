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

(** Gui completion for a widget created by Gui_dialog_GATEWAY_widget.make *)

(* Shortcuts *)
let mkenv   = Environment.make_string_env

module Make (State:sig val st:State.globalState end) = struct

 open State

 (* User handler for dialog completion. *)
 let dialog ~title ~(update:Mariokit.Netmodel.world_gateway option) () =

   let (name, label, network, dhcp_enabled, user_port_no, action, oldname) =
     match update with
     | None -> ((st#network#suggestedName "G"), None, None, (Some true), None, "add", "")
     | Some g ->
        begin
        let name = g#get_name in
        let label = Some g#get_label in
        let (i1, i2, i3, i4) = Ipv4.ipv4_of_string g#get_network_address in
        (* With the current version of slirpvde i4 is always 1 and cidr is 24 *)
        let cidr = 24 in
        let network = Some (i1, i2, i3, i4, cidr) in
        let dhcp_enabled = Some g#get_dhcp_enabled in
        let user_port_no = Some g#get_eth_number in
        (name, label, network, dhcp_enabled, user_port_no, "update", name)
        end
   in
   let ok_callback ((name, label, (i1,i2,i3,i4,cidr), dhcp_enabled, user_port_no) as g) =
     if not (StrExtra.wellFormedName name)
      then None   (* refused *)
      else Some g (* accepted *)
   in
   let help_callback () = () in (* TODO *)
   let s_ = Gettext.s_ in

   (* Call the dialog: *)
   match
     Gui_world_gateway_dialog_widget.make
       ~title ~s_ ~name ?label ?network ?dhcp_enabled ?user_port_no
       ~ok_callback ~help_callback ()
   with
   | None -> None
   | Some (name, label, (i1,i2,i3,i4,cidr), dhcp_enabled, user_port_no) ->
      begin
	(* Return the result of the dialog converted in a string environment: *)
	let network_address = Printf.sprintf "%i.%i.%i.%i" i1 i2 i3 0 in
	let netmask = Ipv4.string_of_ipv4 (Ipv4.netmask_of_cidr cidr) in
	let ports_no = 4 in (* TODO *)
	let result =
	  mkenv [
	    ("name",name);
	    ("action",action);
	    ("oldname",oldname);
	    ("label",label);
	    ("network_address",network_address);
	    ("netmask",netmask);
	    ("dhcp_enabled", (string_of_bool dhcp_enabled));
	    ("user_port_no", (string_of_int user_port_no));
	    ]
	in Some result
      end

end (* Make *)
