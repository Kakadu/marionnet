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
let mkenv = Environment.make_string_env

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
     Tk.Label.set d#label_dialog_ROUTER_label (s_ "\nLabel" );
     Tk.Label.set d#label_dialog_ROUTER_ports (s_ "Ports number" );
     Tk.Label.set d#label_dialog_ROUTER_ip_port0 (s_ "IP port0" );
    end in

   (* Tooltips *)
   let () = begin
     Tk.Tooltip.set d#image_dialog_ROUTER (s_ "Router" );
     Tk.Tooltip.set_both d#label_dialog_ROUTER_name  d#router_name (s_ "Router name. This name must be unique in the virtual network. Suggested : R1, R2, ..." );
     Tk.Tooltip.set_both d#label_dialog_ROUTER_label d#router_label Tk.Tooltip.Text.component_label;
     Tk.Tooltip.set_both d#label_dialog_ROUTER_ports d#router_ports (s_ "Number of router ports" );
     Tk.Tooltip.set d#label_dialog_ROUTER_ip_port0 (s_ "IPv4 configuration of the first router port (0)" );
     Tk.Tooltip.set d#router_ip_a (s_ "First octet of the IPv4 address" );
     Tk.Tooltip.set d#router_ip_b (s_ "Second octet of the IPv4 address" );
     Tk.Tooltip.set d#router_ip_c (s_ "Third octet of the IPv4 address" );
     Tk.Tooltip.set d#router_ip_d (s_ "Fourth octet of the IPv4 address" );
     Tk.Tooltip.set d#router_ip_netmask (s_ "Netmask (CIDR notation)" );
    end in

   begin
   match update with
   | None   -> let prefix = "R" in
               dialog#router_name#set_text (st#network#suggestedName prefix);
               dialog#router_name#misc#grab_focus ()
   | Some h -> begin
                dialog#router_name #set_text   h#get_name                  ;
                dialog#router_label#set_text   h#get_label                 ;
                dialog#router_ports#set_value (float_of_int h#get_eth_number);
                (* The user cannot remove receptacles used by a cable. *)
                let min_eth = (st#network#maxBusyReceptacleIndex h#get_name Mariokit.Netmodel.Eth)+1 in
                let min_multiple_of_4 = (ceil ((float_of_int min_eth) /. 4.0)) *. 4.0 in
                dialog#router_ports#adjustment#set_bounds ~lower:(max min_multiple_of_4 4.0) () ;
               end
   end;


   (* router dialog parser *)
   let env_of_dialog () =
     begin
     let n     = dialog#router_name #text                                                in
     let l     = dialog#router_label#text                                                in
     let (c,o) = match update with None -> ("add","") | Some h -> ("update",h#name)      in
     let eth   = (string_of_int dialog#router_ports#value_as_int)                        in

     if not (StrExtra.wellFormedName n) then raise Talking.EDialog.IncompleteDialog
     else mkenv [("name",n); ("action",c); ("oldname",o); ("label",l); ("eth",eth)]
     end in

   (* Call the dialog loop *)
   Tk.dialog_loop ~help:(Some (Talking.Msg.help_device_insert_update Mariokit.Netmodel.Router)) dialog env_of_dialog st

end
