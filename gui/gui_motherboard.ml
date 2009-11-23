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

#load "include_type_definitions_p4.cmo";;
INCLUDE DEFINITIONS "gui/gui_motherboard.mli"

#load "chip_parser_p4.cmo";;

open Gettext;;

module Make (S : sig val st:State.globalState end) = struct

 open S
 let w = st#mainwin
 let system = st#system

 (* Main window title manager *)

  chip main_window_title_manager : (prj_filename) -> () =
    let title = match prj_filename with
    | None          ->  Command_line.window_title
    | Some filename -> (Command_line.window_title ^ " - " ^ filename)
    in
    w#window_MARIONNET#set_title title

  let main_window_title_manager =
    new main_window_title_manager
      ~name:"main_window_title_manager"
      ~prj_filename:st#prj_filename ()

  (* Sensitiveness manager *)

  chip sensitiveness_manager : (app_state) -> () =
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
    complement
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

  let sensitiveness_manager =
    ((new sensitiveness_manager
         ~name:"sensitiveness_manager"
         ~app_state:st#app_state ()) :> sensitiveness_manager_interface)

  (* Dot tuning manager. Is a toggling chip of arity 6. *)
  chip dot_tuning_manager :
    (iconsize      : string  ,
     rankdir       : string  ,
     shuffler      : int list,
     nodesep       : float   ,
     labeldistance : float   ,
     extrasize     : float   ,
     reverted_list : (int * bool) list
     ) -> (y)
     = () ;;

  let refresh_sketch_counter    = Chip.wcounter ~name:"refresh_sketch_counter" ()
  let reverted_rj45cables_cable = Chip.cable    ~name:"reverted_rj45cables_cable" ()

  let () =
   let m = (object method reverted_rj45cables_cable = reverted_rj45cables_cable end) in
   begin
    st#set_motherboard m;
    st#network#set_motherboard m;
   end

  let dot_tuning_manager =
   let d = st#network#dotoptions in
   new dot_tuning_manager
        ~name:"dot_tuning_manager"
   	~iconsize:d#iconsize
   	~rankdir:d#rankdir
   	~shuffler:d#shuffler
   	~nodesep:d#nodesep
   	~labeldistance:d#labeldistance
   	~extrasize:d#extrasize
   	~reverted_list:(reverted_rj45cables_cable :> (int * bool, (int * bool) list) Chip.wire)
   	~y:refresh_sketch_counter ()

  chip sketch_refresher : (app_state, x:int) -> () =
   (* Do not refresh if there isn't an active project. This is not correct in general. FIX IT *)
   if (app_state <> State.NoActiveProject) then
    begin
    self#tracing#message "refreshing...";
    (* Similar to State.globalState#refresh_sketch but without locking sketch. *)
    let fs = st#dotSketchFile in
    let ft = st#pngSketchFile in
    let ch = open_out fs in
    output_string ch (st#network#dotTrad ());
    close_out ch;
    let command_line =
      "dot -Efontname=FreeSans -Nfontname=FreeSans -Tpng -o "^ft^" "^fs in
    self#tracing#message "The dot command line is";
    self#tracing#message command_line;
    let exit_code = Sys.command command_line in
    self#tracing#message (Printf.sprintf "dot exited with exit code %i" exit_code);
    st#mainwin#sketch#set_file st#pngSketchFile ;
    (if not (exit_code = 0) then
      Simple_dialogs.error
        (s_ "dot failed")
        (Printf.sprintf
           (f_ "Invoking dot failed. Did you install graphviz?\n\
The command line is\n%s\nand the exit code is %i.\n\
Marionnet will work, but you will not see the network graph picture until you fix the problem.\n\
There is no need to restart the application.")
           command_line
           exit_code)
        ());
    end

  let sketch_refresher =
    new sketch_refresher ~name:"sketch_refresher" ~app_state:st#app_state ~x:refresh_sketch_counter ()

  (* Debugging: press F2 for printing the list of current components to stderr. *)
  let _ = st#mainwin#toplevel#event#connect#key_press ~callback:
             (fun k -> (if GdkEvent.Key.keyval k = GdkKeysyms._F2 then system#show_component_list ()); false)

  (* Debugging: press F3 to display the dot representation of the motherboard. *)
  let _ =
   let display () =
    begin
    let fs = "/tmp/gui_motherboard.dot" in
    let ft = "/tmp/gui_motherboard.png" in
    let ch = open_out fs in
    output_string ch (system#to_dot);
    close_out ch;
    ignore (Sys.command ("dot -Tpng -o "^ft^" "^fs^" && display "^ft^" &"))
    end
   in
   st#mainwin#toplevel#event#connect#key_press ~callback:
             (fun k -> (if GdkEvent.Key.keyval k = GdkKeysyms._F3 then display ()); false)

(*  let pippo = WGButton.button ~name:"button_pippo" system ~label:"PIPPO" ~packing:(st#mainwin#hbuttonbox_BASE#pack) ()

  chip f_class : (x:int) -> (y) = () ;;
  let f = new f_class ~x:pippo#wire#enter ~y:refresh_sketch_counter ()*)


end
