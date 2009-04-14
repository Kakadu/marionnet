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


(** Gui completion for the toolbar_DOT_TUNING widget defined with glade. *)
open Gettext;;
module List = ListExtra.List
open Sugar (* for '=>' and '||' *)

module Make (State : sig val st:State.globalState end)  = struct

open State
let w = st#mainwin

(* Labels *)
let () = begin
 let set label text =
  label#set_use_markup true;
  label#set_label ("<small><small>"^text^"</small></small>")
 in
 set w#label_DOT_TUNING_NODES  (s_ "Nodes")   ;
 set w#label_DOT_TUNING_EDGES  (s_ "Arcs" )   ;
 set w#label_DOT_TUNING_LABELS (s_ "Labels")  ;
 set w#label_DOT_TUNING_AREA   (s_ "Surface") ;
end


(* Tooltips *)
let () = begin
 let set w text = (GData.tooltips ())#set_tip w ~text
 in
 set w#label_DOT_TUNING_NODES#coerce       (s_ "Tuning of graph nodes")      ;
 set w#vscale_DOT_TUNING_ICONSIZE#coerce   (s_ "Tuning of icons size (machines, switch, hub, etc), without changing arrangement") ;
 set w#button_DOT_TUNING_SHUFFLE#coerce    (s_ "Randomly arrange nodes") ;
 set w#button_DOT_TUNING_UNSHUFFLE#coerce  (s_ "Go back to standard nodes arrangement (not random)") ;
 set w#label_DOT_TUNING_EDGES#coerce       (s_ "Tuning of graph arcs") ;
 set w#button_DOT_TUNING_RANKDIR_TB#coerce (s_ "Arrange arcs from top to bottom") ;
 set w#button_DOT_TUNING_RANKDIR_LR#coerce (s_ "Arrange arcs from lef to right") ;
 set w#vscale_DOT_TUNING_NODESEP#coerce    (s_ "Minimun arcs size") ;
 set w#menubar_DOT_TUNING_INVERT#coerce    (s_ "Invert arcs arrangement") ;
 set w#label_DOT_TUNING_LABELS#coerce      (s_ "Tuning of labels on arcs edges") ;
 set w#vscale_DOT_TUNING_LABELDISTANCE#coerce (s_ "Distance between labels and icons") ;
 set w#label_DOT_TUNING_AREA#coerce        (s_ "Tuning of the graph background size. The size may increase to double (100%) the original. In that case, elements are arranged to fill available space.") ;
 set w#vscale_DOT_TUNING_EXTRASIZE#coerce  (s_ "Size of background")
 end

(* Callbacks *)

let (opt,net) = (st#dotoptions, st#network)

(* Tool *)
let fold_lines = function [] -> "" | l-> List.fold_left (fun x y -> x^" "^y) (List.hd l) (List.tl l)

(** Reaction for the iconsize tuning *)
let iconsize_react () = if opt#are_gui_callbacks_disable then () else
  begin
   let size = opt#read_gui_iconsize () in
   st#flash ~delay:4000 (Printf.sprintf (f_ "Icons size is fixed to value %s (default=large)") size);
   st#refresh_sketch () ;
  end

(** Reaction for the shuffle tuning *)
let shuffle_react () =
  begin
   opt#set_shuffler (List.shuffleIndexes (net#nodes));
   let namelist = net#getNodeNames => ( (List.permute opt#get_shuffler) || fold_lines ) in
   st#flash ~delay:4000 ((s_ "Icons arranged randomly : ")^namelist);
   st#refresh_sketch () ;
  end

(** Reaction for the unshuffle tuning *)
let unshuffle_react () =
  begin
   opt#reset_shuffler ();
   let namelist = (net#getNodeNames => fold_lines) in
   st#flash ~delay:4000 ((s_ "Icons arranged by default : ")^namelist);
   st#refresh_sketch () ;
  end

(** Reaction for the rankdir tunings *)
let rankdir_react x () =
  begin
   let old = st#dotoptions#rankdir in
   st#dotoptions#set_rankdir x;
   let msg = match x with
    | "TB" -> (s_ "Arrange arcs from top to bottom (default)")
    | "LR" -> (s_ "Arrange arcs from left to right")
    | _    -> "Not valid Rankdir" in
   st#flash ~delay:4000 msg;
   if x<>old then st#refresh_sketch () ;
  end

(** Reaction for the nodesep tuning *)
let nodesep_react () = if opt#are_gui_callbacks_disable then () else
  begin
   let y = opt#read_gui_nodesep () in
   st#flash (Printf.sprintf (f_ "Minimum arcs size (distance between nodes) is fixed to value %s (default=0.5)") (string_of_float y));
   st#refresh_sketch () ;
  end

(** Reaction for the labeldistance tuning *)
let labeldistance_react () = if opt#are_gui_callbacks_disable then () else
  begin
   let y = opt#read_gui_labeldistance () in
   st#flash (Printf.sprintf (f_ "Distance between labels and icons fixed to value %s (default=1.6)") (string_of_float y));
   st#refresh_sketch () ;
  end

(** Reaction for the extrasize_x tuning *)
let extrasize_react () = if opt#are_gui_callbacks_disable then () else
  begin
   let x = () => (opt#read_gui_extrasize || int_of_float || string_of_int) in
   st#flash (Printf.sprintf (f_ "Background size fixed to %s%% of the minimun value to contain the graph (default=0%%)") x );
   st#refresh_sketch () ;
  end

(** Reaction for a rotate tuning *)
let rotate_callback x () =
  begin
   st#network#invertedCableToggle x ;
   st#flash (Printf.sprintf (f_ "Cable %s (re)inverted") x);
   st#refresh_sketch () ;
  end

(* Connections *)

let _ = w#vscale_DOT_TUNING_ICONSIZE#connect#value_changed        iconsize_react
let _ = w#button_DOT_TUNING_SHUFFLE#connect#clicked               shuffle_react
let _ = w#button_DOT_TUNING_UNSHUFFLE#connect#clicked             unshuffle_react
let _ = w#button_DOT_TUNING_RANKDIR_TB#connect#clicked           (rankdir_react "TB")
let _ = w#button_DOT_TUNING_RANKDIR_LR#connect#clicked           (rankdir_react "LR")
let _ = w#vscale_DOT_TUNING_NODESEP#connect#value_changed         nodesep_react
let _ = w#vscale_DOT_TUNING_LABELDISTANCE#connect#value_changed   labeldistance_react
let _ = w#vscale_DOT_TUNING_EXTRASIZE#connect#value_changed       extrasize_react

(** Generic connect function for rotate menus. *)
let connect_rotate_menu ~widget ~widget_menu ~dynList = begin
 let set_active cname = (List.mem cname st#network#invertedCables) in
 (Widget.DynamicSubmenu.make
   ~set_active
   ~submenu:widget_menu
   ~menu:widget
   ~dynList
   ~action:rotate_callback ()) ;   ()
 end

(* Connect INVERT_DIRECT *)
let _ =
  connect_rotate_menu
     ~widget:st#mainwin#imagemenuitem_DOT_TUNING_INVERT_DIRECT
     ~widget_menu:st#mainwin#imagemenuitem_DOT_TUNING_INVERT_DIRECT_menu
     ~dynList:(fun () -> st#network#getDirectCableNames)

(* Connect INVERT_CROSSOVER *)
let _ =
  connect_rotate_menu
     ~widget:st#mainwin#imagemenuitem_DOT_TUNING_INVERT_CROSSOVER
     ~widget_menu:st#mainwin#imagemenuitem_DOT_TUNING_INVERT_CROSSOVER_menu
     ~dynList:(fun () -> st#network#getCrossoverCableNames) 

end (* Make *)
