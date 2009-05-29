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

(** Gui completion for the widget window_MARIONNET (main window) defined with glade. *)

module Make (State : sig val st:State.globalState end) = struct

open State
let w = st#mainwin

(* Labels in main window *)
let () = begin
 w#label_VIRTUAL_NETWORK#set_label (s_ "Virtual network");
 w#label_TAB_DOCUMENTS#set_label   (s_ "Project documents")
end

(* ***************************************** *
             Gui motherboard
 * ***************************************** *)

module Motherboard = Gui_motherboard. Make (State)


(* ***************************************** *
         MENUS Project, Options, ...
 * ***************************************** *)

module Created_menubar_MARIONNET = Gui_menubar_MARIONNET. Make (State) (Motherboard)


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
 l1#set_label (s_ "<i>Components</i>");
 l2#set_label (s_ "<i>Documents</i>");
end

(* ***************************************** *
             notebook_INTERNAL 
 * ***************************************** *)

let () = begin
 let labels = get_tab_labels_of w#notebook_INTERNAL in
 let (l1,l2,l3,l4) = tuple4_of_list labels in
 List.iter (fun l -> l#set_use_markup true) labels ;
 let set l text = l#set_label ("<i>"^text^"</i>") in
 set l1 (s_ "Image")       ;
 set l2 (s_ "Interfaces")  ;
 set l3 (s_ "Defects")     ;
 set l4 (s_ "Disks")       ;
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
(set w#label_button_BASE_STARTUP_EVERYTHING (s_ "Start all");
set w#label_button_BASE_BROADCAST (s_ "Broadcast") ;
set w#label_button_BASE_POWEROFF_EVERYTHING (s_ "Power-off all");
set w#label_button_BASE_SHUTDOWN_EVERYTHING (s_ "Shutdown all"))

(* Tooltips for base buttons *)
let () = 
 let set = (GData.tooltips ())#set_tip in begin
  set w#button_BASE_STARTUP_EVERYTHING#coerce  ~text:(s_ "Start the virtual network (machines, switch, hub, etc) locally on this machine");
  set w#button_BASE_BROADCAST#coerce           ~text:(s_ "Broadcast the specification of the virtual network on a real network");
  set w#button_BASE_POWEROFF_EVERYTHING#coerce ~text:(s_ "(Ungracefully) shutdown every element of the network, as in a power-off");
  set w#button_BASE_SHUTDOWN_EVERYTHING#coerce ~text:(s_ "Gracefully stop every element of the network")
 end

(* Connections *)
let () =

  let _ = w#button_BASE_STARTUP_EVERYTHING#connect#clicked ~callback:(fun () -> st#startup_everything ()) in

  let _ = w#button_BASE_SHUTDOWN_EVERYTHING#connect#clicked
    ~callback:(fun () ->
      match Simple_dialogs.confirm_dialog
          ~question:(s_ "Are you sure that you want to stop\nall the running components?")
          () with
        Some true  -> st#shutdown_everything ()
      | Some false -> ()
      | None -> ()) in

  let _ = w#button_BASE_POWEROFF_EVERYTHING#connect#clicked
    ~callback:(fun () ->
      match Simple_dialogs.confirm_dialog
          ~question:(s_ "Are you sure that you want to power off\nall the running components? It is also possible to shut them down graciously...")
          () with
        Some true -> st#poweroff_everything ()
      | Some false -> ()
      | None -> () ) in

  let _ =
    let callback = (fun _ -> Created_menubar_MARIONNET.Created_entry_project_quit.callback (); true) in
    w#toplevel#event#connect#delete ~callback

  in ()

end
