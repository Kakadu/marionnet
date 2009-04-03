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


(** Gui completion for the widget window_MARIONNET (main window) defined with glade. *)

module Make (State : sig val st:State.globalState end) = struct

open State
let w = st#mainwin

(* Labels in main window *)
let () = begin
 w#label_VIRTUAL_NETWORK#set_text "Réseau virtuel";
 w#label_TAB_DOCUMENTS#set_text   "Documents du projet"
end

(* ***************************************** *
         MENUS Project, Options, ...
 * ***************************************** *)

module Created_menubar_MARIONNET = Gui_menubar_MARIONNET. Make (State)

(* ***************************************** *
             notebook_CENTRAL 
 * ***************************************** *)

(* Tool -> ocamlbricks widget.ml ? *)
let get_tab_labels_of notebook =
 let mill widget = (GMisc.label_cast (notebook#get_tab_label widget))
 in List.map mill notebook#children

let tuple2_of_list = function [l1;l2]       -> (l1,l2)       | _ -> assert false 
let tuple4_of_list = function [l1;l2;l3;l4] -> (l1,l2,l3,l4) | _ -> assert false 

let () = begin
 let labels = get_tab_labels_of w#notebook_CENTRAL in
 let (l1,l2) = tuple2_of_list labels in
 List.iter (fun l -> l#set_use_markup true) labels ;
 l1#set_label "<i>Composants</i>"   ;
 l2#set_label "<i>Documents</i>"  ;
end

(* ***************************************** *
             notebook_INTERNAL 
 * ***************************************** *)

let () = begin
 let labels = get_tab_labels_of w#notebook_INTERNAL in
 let (l1,l2,l3,l4) = tuple4_of_list labels in
 List.iter (fun l -> l#set_use_markup true) labels ;
 let set l text = l#set_label ("<i>"^text^"</i>") in
 set l1 "Image"       ;
 set l2 "Interfaces"  ;
 set l3 "Anomalies"   ;
 set l4 "Disques"     ;
end

(* ***************************************** *
             toolbar_DOT_TUNING 
 * ***************************************** *)

module Created_toolbar_DOT_TUNING = Gui_toolbar_DOT_TUNING. Make (State)

(* ***************************************** *
                BASE BUTTONS 
 * ***************************************** *)

(* Labels for base buttons *)
let () =
 let set label text =
 label#set_use_markup true;
 label#set_label text in
(set w#label_button_BASE_STARTUP_EVERYTHING "Tout démarrer" ;
set w#label_button_BASE_BROADCAST "Diffuser" ;
set w#label_button_BASE_POWEROFF_EVERYTHING "Tout éteindre" ;
set w#label_button_BASE_SHUTDOWN_EVERYTHING "Tout arrêter" )

(* Tooltips for base buttons *)
let () = 
 let set = (GData.tooltips ())#set_tip in begin
  set w#button_BASE_STARTUP_EVERYTHING#coerce  ~text:"Démarrer le réseau virtuel (machines, switch, hub, etc) en local sur cette machine.";
  set w#button_BASE_BROADCAST#coerce           ~text:"Diffuser la spécification du réseau virtuel sur un réseau réél.";
  set w#button_BASE_POWEROFF_EVERYTHING#coerce ~text:"Éteindre brusquement tous les éléments du réseau comme lors d'une panne de courant (power off)";
  set w#button_BASE_SHUTDOWN_EVERYTHING#coerce ~text:"Éteindre proprement tous les éléments du réseau (shutdown)"
 end

(* Connections *)
let () =

  let _ = w#button_BASE_STARTUP_EVERYTHING#connect#clicked ~callback:(fun () -> st#startup_everything ()) in

  let _ = w#button_BASE_SHUTDOWN_EVERYTHING#connect#clicked
    ~callback:(fun () ->
      match Simple_dialogs.confirm_dialog
          ~question:"Etes-vous sûr de vouloir arrêter\ntous les composants en exécution ?"
          () with
        Some true  -> st#shutdown_everything ()
      | Some false -> ()
      | None -> ()) in

  let _ = w#button_BASE_POWEROFF_EVERYTHING#connect#clicked
    ~callback:(fun () ->
      match Simple_dialogs.confirm_dialog
          ~question:("Etes-vous sûr de vouloir débrancher le courant\nà tous les composants en exécution (power off) ?\n\n"^
                     "Il est aussi possible de les arrêter gracieusement (shutdown)...")
          () with
        Some true -> st#poweroff_everything ()
      | Some false -> ()
      | None -> () ) in

  let _ =
    let callback = (fun _ -> Created_menubar_MARIONNET.Created_entry_project_quit.callback (); true) in
    w#toplevel#event#connect#delete ~callback

  in ()


end
