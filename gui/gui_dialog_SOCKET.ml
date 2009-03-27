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


(** Gui completion for the dialog_SOCKET widget defined with glade. *)

(* Shortcuts *)
module Str = StrExtra.Str
let mkenv = Environment.make_string_env

module Make (State:sig val st:State.globalState end) = struct

  open State

  (* User handler for dialog completion. *)
  let dialog ~title ~(update:Mariokit.Netmodel.gateway option) () =

   let dialog = new Gui.dialog_SOCKET () in
   dialog#toplevel#set_title title;
   let d = dialog in (* Convenient alias *)
   let module Tk = Gui_dialog_toolkit.Make (struct let toplevel = d#toplevel end) in

   (* Labels *)
   let () = begin
     Tk.Label.set d#label_dialog_SOCKET_name "Nom";
     Tk.Label.set d#label_dialog_SOCKET_label "\nÉtiquette";
    end in

   (* Tooltips *)
   let () = begin
     Tk.Tooltip.set d#image_dialog_SOCKET "Prise RJ45 vers l'extérieur";
     Tk.Tooltip.set_both d#label_dialog_SOCKET_name  d#socket_name "Le nom de la prise RJ45. Ce nom doit être unique dans le réseau virtuel. Suggestion : E1, E2,...";
     Tk.Tooltip.set_both d#label_dialog_SOCKET_label d#socket_label Tk.Tooltip.Text.component_label;
    end in


   (match update with
   | None   -> dialog#socket_name#set_text (st#network#suggestedName "E"); (* E stands for extern *)
               dialog#socket_name#misc#grab_focus ()
   | Some c -> dialog#socket_name#set_text  c#get_name  ;
               dialog#socket_label#set_text c#get_label 
   );

   (* Socket dialog parser *)
   let env_of_dialog () =
     begin
     let n     = dialog#socket_name#text                                               in
     let l     = dialog#socket_label#text                                              in
     let (c,o) = match update with None -> ("add","") | Some c -> ("update",c#name)    in
     (* The following informations are currently unused (the socket leads to a bridge) *)
     let ip_1 = string_of_float dialog#socket_ip_a   #value                            in
     let ip_2 = string_of_float dialog#socket_ip_b   #value                            in
     let ip_3 = string_of_float dialog#socket_ip_c   #value                            in
     let ip_4 = string_of_float dialog#socket_ip_d   #value                            in
     let nm   = string_of_float dialog#socket_ip_netmask#value                         in
     if not (Str.wellFormedName n) then raise Talking.EDialog.IncompleteDialog  else
            mkenv [("name",n)  ; ("label",l)  ; ("action",c)   ; ("oldname",o)  ;
                   ("ip_1",ip_1); ("ip_2",ip_2); ("ip_3",ip_3);  ("ip_4",ip_4); ("nm",nm) ]
     end in

   (* Call the Dialog loop *)
   Tk.dialog_loop ~help:(Some Talking.Msg.help_socket_insert_update) dialog env_of_dialog st

end
