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


(** Gui completion for the dialog_HUB widget defined with glade. *)

(* Shortcuts *)
module Str = StrExtra.Str
let mkenv = Environment.make_string_env

module Make (State:sig val st:State.globalState end) = struct

  open State

  (* User handler for dialog completion. *)
  let dialog ~title ~(update:Mariokit.Netmodel.device option) () =

   let dialog = new Gui.dialog_HUB () in
   dialog#toplevel#set_title title;
   let d = dialog in (* Convenient alias *)
   let module Tk = Gui_dialog_toolkit.Make (struct let toplevel = d#toplevel end) in

   (* Labels *)
   let () = begin
     Tk.Label.set d#label_dialog_HUB_name "Nom";
     Tk.Label.set d#label_dialog_HUB_label "\nÉtiquette";
     Tk.Label.set d#label_dialog_HUB_ports "Nombre de Ports";
    end in

   (* Tooltips *)
   let () = begin
     Tk.Tooltip.set d#image_dialog_HUB "Répéteur (hub)";
     Tk.Tooltip.set_both d#label_dialog_HUB_name  d#hub_name "Le nom du répéteur (hub). Ce nom doit être unique dans le réseau virtuel. Suggestion : H1, H2,...";
     Tk.Tooltip.set_both d#label_dialog_HUB_label d#hub_label Tk.Tooltip.Text.component_label_with_suggestion;
     Tk.Tooltip.set_both d#label_dialog_HUB_ports d#hub_ports "Nombre de ports du répéteur (hub)";
end in

   (* Set defaults. If we are updating, defaults are the old values. *)
   begin
   match update with
   | None   -> let prefix = "H" in
               dialog#hub_name#set_text (st#network#suggestedName prefix);
               dialog#hub_name#misc#grab_focus ()
   | Some h -> begin
                dialog#hub_name #set_text    h#get_name                  ;
                dialog#hub_label#set_text    h#get_label                 ;
                dialog#hub_ports#set_value  (float_of_int h#get_eth_number);
                (* The user cannot remove receptacles used by a cable. *)
                let min_eth = (st#network#maxBusyReceptacleIndex h#get_name Mariokit.Netmodel.Eth)+1 in
                let min_multiple_of_4 = (ceil ((float_of_int min_eth) /. 4.0)) *. 4.0 in
                dialog#hub_ports#adjustment#set_bounds ~lower:(max min_multiple_of_4 4.0) () ;
               end
   end;

   (* Parse the widget in order to generate the environment. *)
   let env_of_dialog () =
     begin
     let n     = dialog#hub_name #text                                                   in
     let l     = dialog#hub_label#text                                                   in
     let (c,o) = match update with None -> ("add","") | Some h -> ("update",h#name)      in
     let eth   = (string_of_int dialog#hub_ports#value_as_int)                           in
     if not (Str.wellFormedName n) then raise Talking.EDialog.IncompleteDialog
     else mkenv [("name",n); ("action",c); ("oldname",o); ("label",l); ("eth",eth)]
     end in

   (* Call the dialog loop *)
   Tk.dialog_loop ~help:(Some (Talking.Msg.help_device_insert_update Mariokit.Netmodel.Hub)) dialog env_of_dialog st

end

