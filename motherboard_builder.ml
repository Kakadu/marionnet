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


open Gettext;;

#load "include_type_definitions_p4.cmo"
;;
INCLUDE DEFINITIONS "motherboard_builder.mli"
;;

module Make (S : sig val st:State.globalState end) = struct

 open S
 let w = st#mainwin

  (* ---------------------------------------- 
              Reactive window title 
     ---------------------------------------- *)
     
  (* Reactive setting: st#project_filename -> w#window_MARIONNET#title *)
  let update_main_window_title : Thunk.id = 
    Cortex.on_commit_append 
      (st#project_paths#filename)
      (fun _ filename ->      (* previous and commited state *)
	let title = match filename with
	| None          ->  Initialization.window_title
	| Some filename ->  Printf.sprintf "%s - %s" (Initialization.window_title) (filename)
	in
        w#window_MARIONNET#set_title (title))
       
  (* ---------------------------------------- 
            Reactive sensitiveness 
     ---------------------------------------- *)
  
  (* Note: why the GC doesn't free this structure (and the related trigger)? *)
  let update_project_state_sensitiveness =
    Cortex.group_pair 
      ~on_commit:(fun (_,_) (filename, nodes) -> (* previous and commited state *)
         (* Convenient aliases: *)
         let wa = (st#sensitive_when_Active) in
         let wr = (st#sensitive_when_Runnable) in
         let wn = (st#sensitive_when_NoActive) in
         (* --- *)
         let active   = (filename <> None) in
         let runnable = active && (not (Queue.is_empty nodes)) in
	 let () = 
	   Log.printf2 "update_project_state_sensitiveness: state project is: active=%b runnable=%b\n" 
	     (active) (runnable)
	 in
	 match active, runnable with
	 | false, _ ->  
	     StackExtra.iter (fun x->x#misc#set_sensitive false) (wa);
	     StackExtra.iter (fun x->x#misc#set_sensitive false) (wr);
	     StackExtra.iter (fun x->x#misc#set_sensitive true)  (wn);
         (* --- *)
	 | true, false ->  
	     StackExtra.iter (fun x->x#misc#set_sensitive true)  (wa);
	     StackExtra.iter (fun x->x#misc#set_sensitive false) (wr);
	     StackExtra.iter (fun x->x#misc#set_sensitive false) (wn);
         (* --- *)
	 | true, true ->  
	     StackExtra.iter (fun x->x#misc#set_sensitive true)  (wa);
	     StackExtra.iter (fun x->x#misc#set_sensitive true)  (wr);
	     StackExtra.iter (fun x->x#misc#set_sensitive false) (wn);
         ) (* end of ~on_commit *)
      (* --- *)
      (st#project_paths#filename)  (*  first member of the group *)
      (st#network#nodes)           (* second member of the group *)

  
  (* Reactive setting: st#network#nodes -> cable's menu sensitiveness.
     Forbid cable additions if there are not enough free ports; explicitly enable
     them if free ports are enough: *)
  let update_cable_menu_entries_sensitiveness : unit = 
    (* The previous and commited state are ignored. 
       This kind of code (on_commit) is outside a critical section, 
       so we can comfortably re-call st#network methods: *)
    let reaction _ _ =      
         let () = Log.printf1 "update_cable_menu_entries_sensitiveness: updating %d widgets\n" 
           (StackExtra.length st#sensitive_cable_menu_entries)
         in
         let condition = st#network#are_there_almost_2_free_endpoints in
         (StackExtra.iter (fun x->x#misc#set_sensitive condition) st#sensitive_cable_menu_entries)
    in
    let _ = Cortex.on_commit_append (st#network#nodes)  (reaction) in
    let _ = Cortex.on_commit_append (st#network#cables) (reaction) in
    ()
    
  (* Called in marionnet.ml before entering the main loop: *)
  let sensitive_widgets_initializer () =
    let () = StackExtra.iter (fun x->x#misc#set_sensitive false) (st#sensitive_when_Active)   in
    let () = StackExtra.iter (fun x->x#misc#set_sensitive false) (st#sensitive_when_Runnable) in
    let () = StackExtra.iter (fun x->x#misc#set_sensitive true)  (st#sensitive_when_NoActive) in
    (* --- *)
    let () = StackExtra.iter (fun x->x#misc#set_sensitive false) (st#sensitive_cable_menu_entries) in
    ()
      
  (* ---------------------------------------- 
               Reactive sketch
     ---------------------------------------- *)
  
  (* --- *)
  let () = 
   let d = st#network#dotoptions in
   let update = (fun _ _ -> st#refresh_sketch) in
   let _ = Cortex.on_commit_append (d#iconsize)      (update) in
   let _ = Cortex.on_commit_append (d#rankdir)       (update) in
   let _ = Cortex.on_commit_append (d#curved_lines)  (update) in
   let _ = Cortex.on_commit_append (d#shuffler)      (update) in
   let _ = Cortex.on_commit_append (d#nodesep)       (update) in
   let _ = Cortex.on_commit_append (d#labeldistance) (update) in
   let _ = Cortex.on_commit_append (d#extrasize)     (update) in
   ()

  (* ---------------------------------------- 
                  Debugging
     ---------------------------------------- *)

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
    
end
