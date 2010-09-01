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

(** Gui completion for the dialog_WORLD_BRIDGE widget defined with glade. *)

(* Shortcuts *)
let mkenv = Environment.make_string_env

module Make (State:sig val st:State.globalState end) = struct

  open State

  (* User handler for dialog completion. *)
  let dialog ~title ~(update:Mariokit.Netmodel.world_bridge option) () =

   let dialog = new Gui.dialog_WORLD_BRIDGE () in
   dialog#toplevel#set_title title;
   let d = dialog in (* Convenient alias *)
   let module Tk = Gui_dialog_toolkit.Make (struct let toplevel = d#toplevel end) in

   (* Labels *)
   let () = begin
     Tk.Label.set d#label_dialog_WORLD_BRIDGE_name (s_ "Name" );
     Tk.Label.set d#label_dialog_WORLD_BRIDGE_label (s_ "\nLabel" ); (* the newline is intentional *)
    end in

   (* Tooltips *)
   let () = begin
     Tk.Tooltip.set d#image_dialog_WORLD_BRIDGE (s_ "World bridge" );
     Tk.Tooltip.set_both d#label_dialog_WORLD_BRIDGE_name  d#world_bridge_name (s_ "World bridge name. This name must be unique in the virtual network. Suggested: B1, B2, ..." );
     Tk.Tooltip.set_both d#label_dialog_WORLD_BRIDGE_label d#world_bridge_label Tk.Tooltip.Text.component_label;
    end in


   (match update with
   | None   -> dialog#world_bridge_name#set_text (st#network#suggestedName "B");
               dialog#world_bridge_name#misc#grab_focus ()
   | Some c -> dialog#world_bridge_name#set_text  c#get_name  ;
               dialog#world_bridge_label#set_text c#get_label
   );

   (* Socket dialog parser *)
   let env_of_dialog () =
     begin
     let n     = dialog#world_bridge_name#text                                               in
     let l     = dialog#world_bridge_label#text                                              in
     let (c,o) = match update with None -> ("add","") | Some c -> ("update",c#name)    in
     (* The following informations are currently unused (the socket leads to a bridge) *)
     let ip_1 = string_of_float dialog#world_bridge_ip_a   #value                            in
     let ip_2 = string_of_float dialog#world_bridge_ip_b   #value                            in
     let ip_3 = string_of_float dialog#world_bridge_ip_c   #value                            in
     let ip_4 = string_of_float dialog#world_bridge_ip_d   #value                            in
     let nm   = string_of_float dialog#world_bridge_ip_netmask#value                         in
     if not (StrExtra.wellFormedName n) then raise Talking.EDialog.IncompleteDialog  else
            mkenv [("name",n)  ; ("label",l)  ; ("action",c)   ; ("oldname",o)  ;
                   ("ip_1",ip_1); ("ip_2",ip_2); ("ip_3",ip_3);  ("ip_4",ip_4); ("nm",nm) ]
     end in

   (* Call the Dialog loop *)
   Tk.dialog_loop ~help:(Some Talking.Msg.help_world_bridge_insert_update) dialog env_of_dialog st

end
