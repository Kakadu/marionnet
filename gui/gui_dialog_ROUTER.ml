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

(** Gui completion for the dialog_ROUTER widget defined with glade. *)

(* Shortcuts *)
let mkenv   = Environment.make_string_env

module Make (State:sig val st:State.globalState end) = struct

  open State

  (* User handler for dialog completion. *)
  let dialog ~title ~(update:Mariokit.Netmodel.device option) () =

   let dialog=new Gui.dialog_ROUTER () in
   dialog#toplevel#set_title title;
   let d = dialog in (* Convenient alias *)
   let module Tk = Gui_dialog_toolkit.Make (struct let toplevel = d#toplevel end) in

   (* Labels *)
   let () = begin
     Tk.Label.set d#label_dialog_ROUTER_name (s_ "Name" );
     Tk.Label.set d#label_dialog_ROUTER_label (s_ "\nLabel" ); (* the newline is intentional *)
     Tk.Label.set d#label_dialog_ROUTER_ports (s_ "Ports number" );
     Tk.Label.set d#label_dialog_ROUTER_ip_port0 (s_ "Address of the first port" );
    end in

   (* Tooltips *)
   let () = begin
     Tk.Tooltip.set d#image_dialog_ROUTER (s_ "Router" );
     Tk.Tooltip.set_both d#label_dialog_ROUTER_name  d#router_name (s_ "Router name. This name must be unique in the virtual network. Suggested: R1, R2, ..." );
     Tk.Tooltip.set_both d#label_dialog_ROUTER_label d#router_label Tk.Tooltip.Text.component_label;
     Tk.Tooltip.set_both d#label_dialog_ROUTER_ports d#router_ports (s_ "Number of router ports" );
     Tk.Tooltip.set d#label_dialog_ROUTER_ip_port0 (s_ "IPv4 configuration of the first router port (0)" );
     Tk.Tooltip.set d#spin_dialog_ROUTER_ip_a (s_ "First octet of the IPv4 address" );
     Tk.Tooltip.set d#spin_dialog_ROUTER_ip_b (s_ "Second octet of the IPv4 address" );
     Tk.Tooltip.set d#spin_dialog_ROUTER_ip_c (s_ "Third octet of the IPv4 address" );
     Tk.Tooltip.set d#spin_dialog_ROUTER_ip_d (s_ "Fourth octet of the IPv4 address" );
     Tk.Tooltip.set d#spin_dialog_ROUTER_ip_netmask (s_ "Netmask (CIDR notation)" );
    end in

   let set_ip_config_widgets ((ip_a, ip_b, ip_c, ip_d),cidr) =
     (List.iter (fun (spin,x) -> spin#set_value (float_of_int x))
        [(d#spin_dialog_ROUTER_ip_a,ip_a);
         (d#spin_dialog_ROUTER_ip_b,ip_b);
         (d#spin_dialog_ROUTER_ip_c,ip_c);
         (d#spin_dialog_ROUTER_ip_d,ip_d);
         (d#spin_dialog_ROUTER_ip_netmask, cidr)]) in

   begin
   match update with
   | None   -> let prefix = "R" in
               d#router_name#set_text (st#network#suggestedName prefix);
               d#router_name#misc#grab_focus ();
               set_ip_config_widgets Initialization.router_port0_default_ipv4_config;

   | Some r -> begin
                let details = Network_details_interface.get_network_details_interface () in
                let name = r#get_name in
                d#router_name #set_text name                    ;
                d#router_label#set_text r#get_label             ;
                d#router_ports#set_value (float_of_int r#get_eth_number);
                let (ip_a, ip_b, ip_c, ip_d) = Ipv4.ipv4_of_string              (details#get_port_attribute_by_index name 0 "IPv4 address") in
                let (_, cidr )               = Ipv4.netmask_with_cidr_of_string (details#get_port_attribute_by_index name 0 "IPv4 netmask") in
                set_ip_config_widgets ((ip_a, ip_b, ip_c, ip_d),cidr);
                (* The user cannot remove receptacles used by a cable. *)
                let min_eth = (st#network#max_busy_receptacle_index r#get_name Mariokit.Netmodel.Eth)+1 in
                let min_multiple_of_4 = (ceil ((float_of_int min_eth) /. 4.0)) *. 4.0 in
                dialog#router_ports#adjustment#set_bounds ~lower:(max min_multiple_of_4 4.0) () ;
               end
   end;


   (* router dialog parser *)
   let env_of_dialog () =
     begin
     let n     = dialog#router_name #text                                                in
     let l     = dialog#router_label#text                                                in
     let ip_a  = int_of_float dialog#spin_dialog_ROUTER_ip_a#value                       in
     let ip_b  = int_of_float dialog#spin_dialog_ROUTER_ip_b#value                       in
     let ip_c  = int_of_float dialog#spin_dialog_ROUTER_ip_c#value                       in
     let ip_d  = int_of_float dialog#spin_dialog_ROUTER_ip_d#value                       in
     let ip    = Printf.sprintf "%i.%i.%i.%i" ip_a ip_b ip_c ip_d                        in
     let cidr  = int_of_float dialog#spin_dialog_ROUTER_ip_netmask#value            in
     let (nm_a, nm_b, nm_c, nm_d) = Ipv4.netmask_of_cidr cidr in
     let nmask = Printf.sprintf "%i.%i.%i.%i" nm_a nm_b nm_c nm_d                        in
     let (c,o) = match update with None -> ("add","") | Some h -> ("update",h#name)      in
     let eth   = (string_of_int dialog#router_ports#value_as_int)                        in

     if not (StrExtra.wellFormedName n) then raise Talking.EDialog.IncompleteDialog
     else mkenv [("name",n); ("action",c); ("oldname",o); ("label",l); ("eth",eth);
                 ("ip",ip);  ("nmask",nmask);]
     end in

   (* Call the dialog loop *)
   Tk.dialog_loop ~help:(Some (Talking.Msg.help_device_insert_update Mariokit.Netmodel.Router)) dialog env_of_dialog st

end
