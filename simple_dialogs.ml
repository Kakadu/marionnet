(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007  Jean-Vincent Loddo
   Copyright (C) 2007  Luca Saiu

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

open Initialization;;
open Gettext;;

(** Convert ocaml (ISO-8859-1) string in UTF-8 format *)
(* let utf8 x = Glib.Convert.convert x "UTF-8" "ISO-8859-1";;  *)
let utf8 x = x;; (* We currently don't use this. It works better :-) *)

(** Generic constructor for message dialog *)
let message win_title msg_title msg_content img_file () = 
  let d=new Gui.dialog_MESSAGE () in
  let _ = d#closebutton_MESSAGE#connect#clicked ~callback:(d#toplevel#destroy) in
  d#toplevel#set_icon (Some Icon.icon_pixbuf);
  d#toplevel#set_title (utf8 win_title);
  d#title#set_use_markup true; 
  d#title#set_label     ("<b>"^msg_title^"</b>");
  d#content#set_label msg_content;
  d#image#set_file (marionnet_home_images ^ img_file);
  ()
;;

(** Specific constructor for help messages *)
let help title msg () = 
  message (s_ "Help") title msg "ico.help.orig.png" ();; 

(** Specific constructor for error messages *)
let error title msg () = 
  message (s_ "Error") title msg "ico.error.orig.png" ();; 

(** Specific constructor for warning messages *)
let warning title msg () = 
  message (s_ "Warning") title msg "ico.warning.orig.png" ();; 

(** Specific constructor for info messages *)
let info title msg () = 
  message (s_ "Information") title msg "ico.info.orig.png" ();;

(** Show a new dialog displaying a progress bar *)
let make_progress_bar_dialog =
  Progress_bar.make_progress_bar_dialog;;

(** Destroy a dialog which was previously created by make_progress_bar_dialog *)
let destroy_progress_bar_dialog dialog =
  Progress_bar.destroy_progress_bar_dialog dialog;;

let confirm_dialog 
    ~question
    ?(cancel = false)
    () =
  let dialog = new Gui.dialog_QUESTION () in 
  dialog#toplevel#set_icon (Some Icon.icon_pixbuf);
  dialog#toplevel#set_title (utf8 "Confirmation");
  dialog#title_QUESTION#set_use_markup true; 
  dialog#title_QUESTION#set_label question;
  ignore
    (dialog#toplevel#event#connect#delete
       ~callback:(fun _ ->
         Log.print_string "Sorry, no, you can't close the dialog. Please make a decision.\n";
         true));
  (if cancel then dialog#toplevel#add_button_stock `CANCEL `CANCEL);
  let result = (ref None) in
  let cont   = ref true in
  while (!cont = true) do  
    begin match dialog#toplevel#run () with
    | `YES  -> begin
        cont := false; 
        result := Some true;
      end
    | `NO   -> begin
        cont := false;
        result := Some false
      end
    | `CANCEL ->
        cont := false;
        result := None
    |  _ ->
        (* The user tried to close the dialog. No, we refuse: let him/her try again *)
        (*assert false*)
        ()
    end 
  done;
  dialog#toplevel#destroy ();
  !result;

(** Only internally used: *)
exception TheUserCanceled;;

(** Show a modal dialog prompting the user for a text, and return the text as entered
    by the user. A predicate checking that the text supplied by the user is valid and a
    callback to be automatically invoked at each text update can be optionally supplied.
    Two callbacks should be supplied, to be called in case of success or cancel. *)
let ask_text_dialog
    ~title
    ~label
    ?(initial_text="")
    ?(constraint_predicate=(fun _ -> true))
    ?(invalid_text_message=(s_ "Sorry, the size is invalid."))
    ?(changed_callback=(fun _ -> ()))
    ?max_length
    ?(enable_cancel=false)
    ?(cancel_callback=(fun () -> ()))
    ~ok_callback
    () =
  let window =
    GWindow.window
      ~title
      ~modal:true
      ~position:`CENTER
      ~type_hint:`DIALOG
      ~icon:Icon.icon_pixbuf
      ~resizable:false
      () in
  let vbox = GPack.vbox ~packing:window#add () in
  let _ = GMisc.label ~text:label ~packing:vbox#add ~line_wrap:true () in
  let entry = GEdit.entry ~text:initial_text ?max_length ~packing:vbox#add () in
  ignore (entry#connect#changed
            ~callback:(fun () -> changed_callback entry#text));
  let hbox = GPack.hbox ~packing:vbox#add ~homogeneous:true () in
  let button_ok = GButton.button ~stock:`OK ~packing:hbox#add () in
  (if enable_cancel then
    let button_cancel = GButton.button ~stock:`CANCEL ~packing:hbox#add () in
    ignore
      (button_cancel#connect#clicked
         ~callback:(fun () ->
                      window#destroy ();
                      cancel_callback ())));
  let ok_callback window entry () =
    let text = entry#text in
    if constraint_predicate text then begin
      window#destroy ();
      ok_callback text
    end
    else begin
      error (s_ "Invalid size") invalid_text_message ()
    end
  in
  ignore (button_ok#connect#clicked ~callback:(ok_callback window entry));
  button_ok#misc#set_can_default true;
  button_ok#misc#grab_default ();
  window#show ();;
