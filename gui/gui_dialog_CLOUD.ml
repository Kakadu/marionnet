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


(** Gui completion for the dialog_CLOUD widget defined with glade. *)

(* Shortcuts *)
module Str = StrExtra.Str
let mkenv = Environment.make_string_env

module Make (State:sig val st:State.globalState end) = struct

  open State

  (* User handler for dialog completion. *)
  let dialog ~title ~(update:Mariokit.Netmodel.cloud option) () =

   let dialog = new Gui.dialog_CLOUD () in
   dialog#toplevel#set_title title;
   let d = dialog in (* Convenient alias *)
   let module Tk = Gui_dialog_toolkit.Make (struct let toplevel = d#toplevel end) in

   (* Labels *)
   let () = begin
     Tk.Label.set d#label_dialog_CLOUD_name "Nom";
     Tk.Label.set d#label_dialog_CLOUD_label "\nÉtiquette";
    end in

   (* Tooltips *)
   let () = begin
     Tk.Tooltip.set d#image_dialog_CLOUD "Nuage L2 (sous-réseau indéterminé de niveau 2)";
     Tk.Tooltip.set_both d#label_dialog_CLOUD_name  d#cloud_name "Le nom du sous-réseau. Ce nom doit être unique dans le réseau virtuel. Suggestion : N1, N2,...";
     Tk.Tooltip.set_both d#label_dialog_CLOUD_label d#cloud_label Tk.Tooltip.Text.component_label;
    end in

   begin
   match update with
   | None   -> dialog#cloud_name#set_text (st#network#suggestedName "N");
               dialog#cloud_name#misc#grab_focus ()
   | Some c -> begin
                dialog#cloud_name#set_text  c#get_name  ;
                dialog#cloud_label#set_text c#get_label ;
               end
   end;

   (* Cloud dialog parser *)
   let env_of_dialog () =
     begin
     let n     = dialog#cloud_name#text                                                  in
     let l     = dialog#cloud_label#text                                                 in
     let (c,o) = match update with None -> ("add","") | Some c -> ("update",c#name)      in
     if not (Str.wellFormedName n) then raise Talking.EDialog.IncompleteDialog
     else mkenv [("name",n)  ; ("label",l)  ; ("action",c)   ; ("oldname",o)  ; ]
     end in

   (* Call the dialog loop *)
   Tk.dialog_loop ~help:(Some Talking.Msg.help_cloud_insert_update) dialog env_of_dialog st

end
