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


(** Gui completion for the dialog_ROUTER widget defined with glade. *)

(* Shortcuts *)
module Str = StrExtra.Str
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
     Tk.Label.set d#label_dialog_ROUTER_name "Nom";
     Tk.Label.set d#label_dialog_ROUTER_label "\nÉtiquette";
     Tk.Label.set d#label_dialog_ROUTER_ports "Nombre de Ports";
     Tk.Label.set d#label_dialog_ROUTER_ip_port0 "IP port0";
    end in

   (* Tooltips *)
   let () = begin
     Tk.Tooltip.set d#image_dialog_ROUTER "Routeur";
     Tk.Tooltip.set_both d#label_dialog_ROUTER_name  d#router_name "Le nom du routeur. Ce nom doit être unique dans le réseau virtuel. Suggestion : R1, R2,...";
     Tk.Tooltip.set_both d#label_dialog_ROUTER_label d#router_label Tk.Tooltip.Text.component_label;
     Tk.Tooltip.set_both d#label_dialog_ROUTER_ports d#router_ports "Nombre de ports du routeur";
     Tk.Tooltip.set d#label_dialog_ROUTER_ip_port0 "Configuration IPv4 du premier port (0) du routeur";
     Tk.Tooltip.set d#router_ip_a "Premier octet de l'adresse IPv4";
     Tk.Tooltip.set d#router_ip_b "Deuxième octet de l'adresse IPv4";
     Tk.Tooltip.set d#router_ip_c "Troisième octet de l'adresse IPv4";
     Tk.Tooltip.set d#router_ip_d "Quatrième octet de l'adresse IPv4";
     Tk.Tooltip.set d#router_ip_netmask "Netmask (en notation CIDR)";
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

     if not (Str.wellFormedName n) then raise Talking.EDialog.IncompleteDialog
     else mkenv [("name",n); ("action",c); ("oldname",o); ("label",l); ("eth",eth)]
     end in

   (* Call the dialog loop *)
   Tk.dialog_loop ~help:(Some (Talking.Msg.help_device_insert_update Mariokit.Netmodel.Router)) dialog env_of_dialog st

end
