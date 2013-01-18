(* This file is part of Marionnet, a virtual network laboratory
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

module Motherboard = Motherboard_builder. Make (State)


(* ***************************************** *
         MENUS Project, Options, ...
 * ***************************************** *)

module Created_menubar_MARIONNET = Gui_menubar_MARIONNET.Make (State)


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

let () = begin
    w#hbox_BASE#set_homogeneous true;
    w#hbox_BASE#set_spacing 5;
    w#hbox_BASE#set_border_width 0;
  end

let button_BASE_STARTUP_EVERYTHING =
  Gui_bricks.button_image ~label:(s_ "Start all") ~stock:`MEDIA_PLAY
    ~tooltip:(s_ "Start the virtual network (machines, switch, hub, etc) locally on this machine")
    ~label_position:`BOTTOM ~stock_size:`DND ~packing:w#hbox_BASE#add ()

let (menu_BASE_PAUSE_SOMETHING, button_BASE_PAUSE_SOMETHING, box_BASE_PAUSE_SOMETHING) =
  let renewer = 
    let get_label_active_callback_list () = 
      let name_kind_suspended_list : (string * [`Node|`Cable] * bool) list = 
        st#network#get_component_names_that_can_suspend_or_resume () 
      in
      List.map 
        (fun (name, kind, suspended) ->
           let callback b = 
             if b = suspended then () else 
             match suspended with
             | true  -> (st#network#get_component_by_name ~kind name)#resume
             | false -> (st#network#get_component_by_name ~kind name)#suspend
           in
           (name, suspended, callback)
        )
        name_kind_suspended_list
    in        
    Gui_bricks.make_check_items_renewer_v1 ~get_label_active_callback_list ()
    (* end of renewer () *)
  in
  Gui_bricks.button_image_popuping_a_menu ~label:(s_ "Suspend") ~stock:`MEDIA_PAUSE
    ~renewer
    ~tooltip:(s_ "Suspend the activity of a network component")
    ~label_position:`BOTTOM ~stock_size:`DND ~packing:w#hbox_BASE#add ()

let button_BASE_SHUTDOWN_EVERYTHING =
  Gui_bricks.button_image ~label:(s_ "Shutdown all") ~stock:`MEDIA_STOP
    ~tooltip:(s_ "Gracefully stop every element of the network")
    ~label_position:`BOTTOM ~stock_size:`DND ~packing:w#hbox_BASE#add ()

let button_BASE_POWEROFF_EVERYTHING =
  Gui_bricks.button_image ~label:(s_ "Power-off all")
    ~file:"ico.poweroff.24x24.png"
    ~tooltip:(s_ "(Ungracefully) shutdown every element of the network, as in a power-off")
    ~label_position:`BOTTOM ~packing:w#hbox_BASE#add ()

(* Just a thunk, the button is not really built. We leave this code
   in order to not remove the gettext key associated to this `tooltip' 
   and this `label': *)
let button_BASE_BROADCAST () =
  Gui_bricks.button_image ~label:(s_ "Broadcast")
    ~tooltip:(s_ "Broadcast the specification of the virtual network on a real network")
    ~file:"ico.diffuser.orig.png"
    ~label_position:`BOTTOM ~packing:w#hbox_BASE#add ()

(* Connections *)
let () =

  let _ = button_BASE_STARTUP_EVERYTHING#connect#clicked ~callback:(fun () -> st#startup_everything ()) in

  let _ = button_BASE_SHUTDOWN_EVERYTHING#connect#clicked
    ~callback:(fun () ->
      match Simple_dialogs.confirm_dialog
          ~question:(s_ "Are you sure that you want to stop\nall the running components?")
          () with
        Some true  -> st#shutdown_everything ()
      | Some false -> ()
      | None -> ()) in

  let _ = button_BASE_POWEROFF_EVERYTHING#connect#clicked
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
