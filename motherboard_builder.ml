(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2009, 2010  Jean-Vincent Loddo
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


(** Gui reactive system. *)
open Gettext;;

#load "include_type_definitions_p4.cmo"
;;
INCLUDE DEFINITIONS "motherboard_builder.mli"
;;
#load "chip_parser_p4.cmo"
;;

module Make (S : sig val st:State.globalState end) = struct

 open S
 let w = st#mainwin
 let system = st#system

 (* Main window title manager *)

  chip main_window_title_manager : (project_filename) -> () =
    let title = match project_filename with
    | None          ->  Initialization.window_title
    | Some filename -> (Initialization.window_title ^ " - " ^ filename)
    in
    w#window_MARIONNET#set_title title

  let main_window_title_manager =
    new main_window_title_manager
      ~name:"main_window_title_manager"
      ~project_filename:st#project_filename ();;

  (* Sensitiveness manager *)

  chip sensitiveness_manager : (
    app_state,
    wa:(GObj.widget list),
    wr:(GObj.widget list),
    wn:(GObj.widget list))
    -> () =
    let print_log () =
      Log.printf4
           "sensitiveness_manager: the project is in the state: %s #wa=%d #wr=%d #wn=%d\n"
            st#app_state_as_string
            (List.length wa) (List.length wr) (List.length wn)
    in
    (match app_state with
    | State.NoActiveProject
     ->  print_log ();
         List.iter (fun x->x#misc#set_sensitive false) (wa@wr) ;
         List.iter (fun x->x#misc#set_sensitive true)  wn;

    | State.ActiveNotRunnableProject
     ->  print_log ();
         List.iter (fun x->x#misc#set_sensitive true)  wa ;
         List.iter (fun x->x#misc#set_sensitive false) (wr@wn);

    | State.ActiveRunnableProject
     ->  print_log ();
         List.iter (fun x->x#misc#set_sensitive true)  (wa@wr) ;
         List.iter (fun x->x#misc#set_sensitive false) wn;
    )

  let sensitiveness_manager =
    new sensitiveness_manager
      ~name:"sensitiveness_manager"
      ~app_state:st#app_state
      ~wa:st#sensitive_when_Active#coerce
      ~wr:st#sensitive_when_Runnable#coerce
      ~wn:st#sensitive_when_NoActive#coerce
      () ;;

  (* Dot tuning manager. Is a toggling chip of arity 6. *)
  chip dot_tuning_manager :
    (iconsize      : string  ,
     rankdir       : string  ,
     curved_lines  : bool    ,
     shuffler      : int list,
     nodesep       : float   ,
     labeldistance : float   ,
     extrasize     : float   ,
     reversed_list : (int * bool) list
     ) -> (y)
     = () ;;

  let refresh_sketch_counter = st#refresh_sketch_counter
  let reversed_rj45cables_cable = Chip.cable ~name:"reversed_rj45cables_cable" ()

  let dot_tuning_manager =
   let d = st#network#dotoptions in
   new dot_tuning_manager
        ~name:"dot_tuning_manager"
   	~iconsize:d#iconsize
   	~rankdir:d#rankdir
   	~curved_lines:d#curved_lines#as_wire
   	~shuffler:d#shuffler
   	~nodesep:d#nodesep
   	~labeldistance:d#labeldistance
   	~extrasize:d#extrasize
   	~reversed_list:(reversed_rj45cables_cable :> (int * bool, (int * bool) list) Chip.wire)
   	~y:refresh_sketch_counter ()

  chip sketch_refresher : (app_state, x:int) -> () =
   (* Do not refresh if there isn't an active project. This is not correct in general. FIX IT *)
   if (app_state <> State.NoActiveProject) then
    begin
    self#tracing#message "refreshing...";
    (* Similar to State.globalState#refresh_sketch but without locking sketch. *)
    let fs = st#dotSketchFile in
    let ft = st#pngSketchFile in
    try begin
      let ch = open_out fs in
      output_string ch (st#network#dotTrad ());
      close_out ch;
      let command_line =
        let splines = string_of_bool (st#network#dotoptions#curved_lines#get) in
	Printf.sprintf "dot -Gsplines=%s -Efontname=FreeSans -Nfontname=FreeSans -Tpng -o '%s' '%s'" splines ft fs
      in
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
      with e ->
        (Log.printf1
           "Warning: exception raised in sketch_refresher:\n%s\nIgnoring.\n"
           (Printexc.to_string e))
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
    ignore (Sys.command ("dot -Tpng -o '"^ft^"' '"^fs^"' && display '"^ft^"' &"))
    end
   in
   st#mainwin#toplevel#event#connect#key_press ~callback:
             (fun k -> (if GdkEvent.Key.keyval k = GdkKeysyms._F3 then display ()); false)

  (* Debugging: press F5 for immediately exiting the gtk main loop (only in the toplevel) *)
  let _ =
    if !Sys.interactive then 
      let stars = "*************************************" in
      Printf.kfprintf flush stdout
        "%s\nPress F5 to switch to the toplevel.\n%s\n\n" stars stars;
      ignore (st#mainwin#toplevel#event#connect#key_press ~callback:(fun k ->
         (match (GdkEvent.Key.keyval k) = GdkKeysyms._F5 with
         | true ->
             Printf.kfprintf flush stdout
               "%s\nYou are now in the toplevel.\nType:\nGMain.Main.main ();;\nto come back to the Marionnet window.\n%s\n\n" stars stars;
             GtkMain.Main.quit ()
         | false -> ()
         );
         false))
    else ()

  chip treeview_filenames : (pwd:string option, prn:string option) -> (h,i,d,t,d1,d2,d3,d4) =
    match pwd, prn with
    | (Some pwd), (Some prn) ->
	let prefix = FilenameExtra.concat_list [pwd; prn] in
	let concat = Filename.concat in
	let dir = Some (concat prefix "states/") in
	let h   = Some (concat prefix "states/states-forest") in
     (* let i   = Some (concat prefix "states/ports") in *) (* old project version (bzr revno <= 460) *)
	let i   = Some (concat prefix "states/ifconfig") in
	let d   = Some (concat prefix "states/defects") in
	let t   = Some (concat prefix "states/texts") in
	(h,i,d,t,dir,dir,dir,dir)
    | _,_ -> (None, None, None, None, None, None, None, None)
    ;;

   let () =
     let m =
       object
         method reversed_rj45cables_cable = reversed_rj45cables_cable
         method project_working_directory =
           Option.extract st#project_working_directory#get
       end
     in
     Motherboard.set m
  ;;

  (* Must be called only when treeview are made: *)
  let set_treeview_filenames_invariant () =
    let _ =
      new treeview_filenames
	~pwd:st#project_working_directory
	~prn:st#project_name
	~h:st#treeview#history#filename
	~i:st#treeview#ifconfig#filename
	~d:st#treeview#defects#filename
	~t:st#treeview#documents#filename
	~d1:st#treeview#history#directory
	~d2:st#treeview#ifconfig#directory
	~d3:st#treeview#defects#directory
	~d4:st#treeview#documents#directory
	()
   in ()
   ;;

(*  let pippo = WGButton.button ~name:"button_pippo" system ~label:"PIPPO" ~packing:(st#mainwin#hbuttonbox_BASE#pack) ()

  chip f_class : (x:int) -> (y) = () ;;
  let f = new f_class ~x:pippo#wire#enter ~y:refresh_sketch_counter ()*)


end
