n(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2009, 2010  Jean-Vincent Loddo
   Copyright (C) 2009  Luca Saiu
   Copyright (C) 2009, 2010  Université Paris 13

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

(** Common tools for setting labels and tips in a dialog. *)
module Make (Toplevel : sig val toplevel : GWindow.dialog_any end) = struct

 open Toplevel

 module Label = struct

  let set label text =
    label#set_use_markup true;
    label#set_label text

 end

 module Tooltip = struct

  let setter =
    let result = (GData.tooltips ()) in
    let _ = toplevel#connect#destroy ~callback:(fun _ -> result#destroy ())
    in result

  let set w text = setter#set_tip w#coerce ~text

  let set_both w1 w2 text = List.iter (fun w -> setter#set_tip w ~text) [w1#coerce;w2#coerce]

  (* Common text for dialog's tooltips *)
  module Text = struct

   let component_label = (s_ "Label to be written in the network sketch, next to the element icon." )

   let component_label_with_suggestion =
    component_label^" "^(s_ "It is advisable to use as label the IP address of the element (for example \"192.168.1.0/24\")." )

   let append_label_suggestion_to msg =
     msg^" "^(s_ "It is advisable to use as label the IP address of the element (for example \"192.168.1.0/24\")." )

  end (* Tooltip.Text *)

 end (* Tooltip *)


 type env  = string Environments.string_env

 (* Moved from talking.ml. Generic dialog loop for component INSERT/UPDATE. The inserted or updated name must be unique in the network. *)
 let dialog_loop ?(help=None) dialog (scan_dialog:unit->env) (st:State.globalState) =

   let result = (ref None) in
   let cont   =  ref true in
   begin
   while (!cont = true) do
     begin match dialog#toplevel#run () with
     | `OK   -> begin
                 try
                 let r = scan_dialog () in

                 let (action,name,oldname) = (r#get("action"),r#get("name"),r#get("oldname")) in

                 (* OK only if the name is not already used in the network (and not empty). *)
                 if ((action="add")    && (st#network#name_exists name)) or
                    ((action="update") && (not (name=oldname)) && (st#network#name_exists name))

                 then
                   (Simple_dialogs.error (s_ "Name conflict" )
                              (Printf.sprintf(f_ "The name '%s' is already used in the virtual network. The names of virtual network elements must be unique." ) name) ())

                 else
                   (result := Some r ; cont := false)

                 with
                 | Talking.EDialog.IncompleteDialog -> cont := true
                 | (Talking.EDialog.BadDialog     (title,msg))   -> (Simple_dialogs.error   title   msg ())
                 | (Talking.EDialog.StrangeDialog (title,msg,r)) -> (*(Msg.warning title msg ()); *)
                       begin
                       match Talking.EDialog.ask_question ~gen_id:"answer" ~title:(s_ "CONFIRM")
                       ~question:(msg^(s_ "\nDo you confirm this connection?" )) ~help:None ~cancel:false ()
                       with
                       | Some e -> if (e#get("answer")="yes")
                                   then (result := Some r ; cont := false)
                                   else cont := true
                       | None   -> (*raise (Failure "Unexpected result of dialog ask_question")*)
				   cont := true (* Consider as the answer "no" *)
                       end
                end

     | `HELP -> (match help with
                 | Some f -> f ();
                 | None -> ()
                 )

     |  _    -> result := None ; cont := false

     end
   done;

   (* Close the dialog and return its result. *)
   dialog#toplevel#destroy ();
   !result
   end

end (* Make *)
