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
 set w#label_DOT_TUNING_NODES#coerce       "Réglage des noeuds du graphe"      ;
 set w#vscale_DOT_TUNING_ICONSIZE#coerce   "Augmenter ou diminuer la taille des icones (machines, switch, hub, etc), sans modifier l'agencement" ;
 set w#button_DOT_TUNING_SHUFFLE#coerce    "Réagencer les noeuds de façon aléatoire" ;
 set w#button_DOT_TUNING_UNSHUFFLE#coerce  "Revenir à l'agencement prédéfini (non aléatoire) des noeuds" ;
 set w#label_DOT_TUNING_EDGES#coerce       "Réglage des arcs du graphe" ;
 set w#button_DOT_TUNING_RANKDIR_TB#coerce "Dessiner les arcs du haut vers le bas" ;
 set w#button_DOT_TUNING_RANKDIR_LR#coerce "Dessiner les arcs de la gauche vers la droite" ;
 set w#vscale_DOT_TUNING_NODESEP#coerce    "Taille minimale des arcs" ;
 set w#menubar_DOT_TUNING_INVERT#coerce    "Dessiner un arc dans le sens inverse" ;
 set w#label_DOT_TUNING_LABELS#coerce      "Réglage des étiquettes aux extrémités des arcs" ;
 set w#vscale_DOT_TUNING_LABELDISTANCE#coerce "Distance entre les étiquettes et les sommets" ;
 set w#label_DOT_TUNING_AREA#coerce        "Réglage de la taille du canevas contenant le graphe. La taille peut être agrandi jusqu'au double (100%) de la taille d'origine. Dans ce cas, les éléments sont réagencés de façon à remplir l'espace disponible." ;
 set w#vscale_DOT_TUNING_EXTRASIZE#coerce  "Taille du canevas"
 end

(* Callbacks *)

let (opt,net) = (st#dotoptions, st#network)

(* Tool *)
let fold_lines = function [] -> "" | l-> List.fold_left (fun x y -> x^" "^y) (List.hd l) (List.tl l)

(** Reaction for the iconsize tuning *)
let iconsize_react () = if opt#are_gui_callbacks_disable then () else
  begin
   let size = opt#read_gui_iconsize () in
   st#flash ~delay:4000 ("Taille des icones fixée à la valeur "^size^" (default=large)");
   st#refresh_sketch () ;
  end

(** Reaction for the shuffle tuning *)
let shuffle_react () =
  begin
   opt#set_shuffler (List.shuffleIndexes (net#nodes));
   let namelist = net#getNodeNames => ( (List.permute opt#get_shuffler) || fold_lines ) in
   st#flash ~delay:4000 ("Icones réagencées de façon aléatoire : "^namelist);
   st#refresh_sketch () ;
  end

(** Reaction for the unshuffle tuning *)
let unshuffle_react () =
  begin
   opt#reset_shuffler ();
   let namelist = (net#getNodeNames => fold_lines) in
   st#flash ~delay:4000 ("Icones dans l'agencement prédéfini : "^namelist);
   st#refresh_sketch () ;
  end

(** Reaction for the rankdir tunings *)
let rankdir_react x () =
  begin
   let old = st#dotoptions#rankdir in
   st#dotoptions#set_rankdir x;
   let msg = match x with
    | "TB" -> "Tracer les arcs du haut vers le bas (default)"
    | "LR" -> "Tracer les arcs de la gauche vers la droite"
    | _    -> "Not valid Rankdir" in
   st#flash ~delay:4000 msg;
   if x<>old then st#refresh_sketch () ;
  end

(** Reaction for the nodesep tuning *)
let nodesep_react () = if opt#are_gui_callbacks_disable then () else
  begin
   let y = opt#read_gui_nodesep () in
   st#flash ("Taille minimale des arcs (distance entre noeuds) fixées à la valeur "^
                  (string_of_float y)^" (default=0.5)");
   st#refresh_sketch () ;
  end

(** Reaction for the labeldistance tuning *)
let labeldistance_react () = if opt#are_gui_callbacks_disable then () else
  begin
   let y = opt#read_gui_labeldistance () in
   st#flash ("Distance entre sommets et étiquettes fixée à la valeur "^
                  (string_of_float y)^" (default=1.6)");
   st#refresh_sketch () ;
  end

(** Reaction for the extrasize_x tuning *)
let extrasize_react () = if opt#are_gui_callbacks_disable then () else
  begin
   let x = () => (opt#read_gui_extrasize || int_of_float || string_of_int) in
   st#flash ("Taille du canevas fixée à +"^x^
                   "% de la valeur suffisante à contenir le graphe (default=0%)");
   st#refresh_sketch () ;
  end

(** Reaction for a rotate tuning *)
let rotate_callback x () =
  begin
   st#network#invertedCableToggle x ;
   st#flash ("Cable "^x^" (re)inversé");
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
