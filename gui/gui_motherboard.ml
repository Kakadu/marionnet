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


(** Gui reactive system. *)

#load "include_type_definitions.p4.cmo";;
INCLUDE DEFINITIONS "gui/gui_motherboard.mli"

#load "chip_parser.p4.cmo";;

module Make (S : sig val st:State.globalState end) = struct

 open S
 let w = st#mainwin

 let system = Chip.get_or_initialize_current_system ()

 (* Main window title manager *)

  chip main_window_title_manager : (prj_filename) -> () =
    let title = match prj_filename with
    | None          ->  Command_line.window_title
    | Some filename -> (Command_line.window_title ^ " - " ^ filename)
    in
    w#window_MARIONNET#set_title title

  let main_window_title_manager = new main_window_title_manager ~prj_filename:st#prj_filename ()

  (* Sensitiveness manager *)

  chip virtual sensitiveness_incompleted_manager : (app_state) -> () =
    let l1 = self#sensitive_when_Active   in
    let l2 = self#sensitive_when_Runnable in
    let l3 = self#sensitive_when_NoActive in
    (match app_state with
    | State.NoActiveProject
     ->  List.iter (fun x->x#misc#set_sensitive false) (l1@l2) ;
         List.iter (fun x->x#misc#set_sensitive true)  l3

    | State.ActiveNotRunnableProject
     ->  List.iter (fun x->x#misc#set_sensitive true)  l1 ;
         List.iter (fun x->x#misc#set_sensitive false) (l2@l3)

    | State.ActiveRunnableProject
     ->  List.iter (fun x->x#misc#set_sensitive true)  (l1@l2) ;
         List.iter (fun x->x#misc#set_sensitive false) l3
    )

  class sensitiveness_manager () = object
    inherit [State.application_state] sensitiveness_incompleted_manager ~app_state:st#app_state ()

    val mutable sensitive_when_Active   : GObj.widget list = []
    val mutable sensitive_when_Runnable : GObj.widget list = []
    val mutable sensitive_when_NoActive : GObj.widget list = []

    method sensitive_when_Active   = sensitive_when_Active
    method sensitive_when_Runnable = sensitive_when_Runnable
    method sensitive_when_NoActive = sensitive_when_NoActive

    method add_sensitive_when_Active   x = sensitive_when_Active   <- x::sensitive_when_Active
    method add_sensitive_when_Runnable x = sensitive_when_Runnable <- x::sensitive_when_Runnable
    method add_sensitive_when_NoActive x = sensitive_when_NoActive <- x::sensitive_when_NoActive
    end

  let sensitiveness_manager = ((new sensitiveness_manager ()) :> sensitiveness_manager_interface)

end
