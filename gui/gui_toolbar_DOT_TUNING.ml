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


(** Gui completion for the toolbar_DOT_TUNING widget defined with glade. *)
open Gettext
open Sugar (* for '=>' and '||' *)

(* This functor defines the dot tuning toolbar driver of the global state. *)
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
 set w#label_DOT_TUNING_EDGES  (s_ "Edges" )  ;
 set w#label_DOT_TUNING_LABELS (s_ "Labels")  ;
 set w#label_DOT_TUNING_AREA   (s_ "Surface") ;
end


(* Tooltips *)
let () = begin
 let set w text = (GData.tooltips ())#set_tip w ~text
 in
 set w#label_DOT_TUNING_NODES#coerce       (s_ "Tuning of graph nodes")      ;
 set w#vscale_DOT_TUNING_ICONSIZE#coerce   (s_ "Tuning of icon size (machines, switch, hub, etc), without changing the icon arrangement") ;
 set w#button_DOT_TUNING_SHUFFLE#coerce    (s_ "Randomly arrange nodes") ;
 set w#button_DOT_TUNING_UNSHUFFLE#coerce  (s_ "Go back to the standard node arrangement (not random)") ;
 set w#label_DOT_TUNING_EDGES#coerce       (s_ "Tuning of graph edges") ;
 set w#button_DOT_TUNING_RANKDIR_TB#coerce (s_ "Arrange edges top-to-bottom") ;
 set w#button_DOT_TUNING_RANKDIR_LR#coerce (s_ "Arrange edges left-to-right") ;
 set w#vscale_DOT_TUNING_NODESEP#coerce    (s_ "Minimun edge size") ;
 set w#menubar_DOT_TUNING_INVERT#coerce    (s_ "Reverse an edge") ;
 set w#label_DOT_TUNING_LABELS#coerce      (s_ "Tuning edge endpoint labels") ;
 set w#vscale_DOT_TUNING_LABELDISTANCE#coerce (s_ "Distance between labels and icons") ;
 set w#label_DOT_TUNING_AREA#coerce        (s_ "Tuning of the graph size. The surface may increase up to double (100%) the original, in which case case elements are arranged to completely fill the available space.") ;
 set w#vscale_DOT_TUNING_EXTRASIZE#coerce  (s_ "Canvas size")
 end

(* ******************************* *
      High-level toolbar driver
 * ******************************* *)

(** Handlers for reading or setting related widgets in a more abstract way. *)
class high_level_toolbar_driver () =

  (* The iconsize converter float -> string *)
 let iconsize_of_float x =
     match (int_of_float x) with
     | 0 -> "small"
     | 1 -> "med"
     | 2 -> "large"
     | 3 -> "xxl"
     | default -> "large"
 in

 (* The iconsize converter string -> float *)
 let float_of_iconsize s =
    match s with
     | "small" -> 0.
     | "med"   -> 1.
     | "large" -> 2.
     | "xxl"   -> 3.
     | default -> 2.
 in

  object (self)

  (** iconsize tuning *)

  method get_iconsize : string =
    iconsize_of_float (w#vscale_DOT_TUNING_ICONSIZE#adjustment#value)

  method set_iconsize (x:string) =
    x => (float_of_iconsize || w#vscale_DOT_TUNING_ICONSIZE#adjustment#set_value)

  (** nodesep tuning *)

  (* Non-linear (quadratic) adjustment in the range [0,2] inches *)
  method get_nodesep : float =
   let formule = fun x -> (((x /. 20.) ** 2.) *. 2.) in
   w#vscale_DOT_TUNING_NODESEP#adjustment#value => formule

  method set_nodesep (y:float) =
    let inverse = fun y -> 20. *. sqrt (y /. 2.) in
    y => (inverse || w#vscale_DOT_TUNING_NODESEP#adjustment#set_value)

  (** labeldistance tuning *)

  (* Non-linear (quadratic) adjustment in the range [0,2] inches *)
  method get_labeldistance : float =
    let formule = fun x -> (((x /. 20.) ** 2.) *. 2.) in
    w#vscale_DOT_TUNING_LABELDISTANCE#adjustment#value => formule

  method set_labeldistance (y:float) =
    let inverse = fun y -> 20. *. sqrt (y /. 2.) in
    y => (inverse || w#vscale_DOT_TUNING_LABELDISTANCE#adjustment#set_value)

  (** extrasize tuning *)

  method get_extrasize  : float     =  w#vscale_DOT_TUNING_EXTRASIZE#adjustment#value
  method set_extrasize  (x:float)   =  w#vscale_DOT_TUNING_EXTRASIZE#adjustment#set_value x

  (** Handling the network image *)

  method get_image                =  w#sketch#pixbuf
  method get_image_current_width  = (GdkPixbuf.get_width  w#sketch#pixbuf)
  method get_image_current_height = (GdkPixbuf.get_height w#sketch#pixbuf)

  val mutable image_original_width  = None
  val mutable image_original_height = None

  (** Called in update_sketch: *)
  method reset_image_size () = image_original_width <- None; image_original_height <- None

  (* Get and affect if need (but only the first time) *)
  method get_image_original_width   = match image_original_width with
  | None    -> (let x = self#get_image_current_width in image_original_width <- Some x; x)
  | Some x  -> x

  (* Get and affect if need (but only the first time) *)
  method get_image_original_height   = match image_original_height with
  | None    -> (let x = self#get_image_current_height in image_original_height <- Some x; x)
  | Some x  -> x

end;; (* class high_level_toolbar_driver *)

(* Enrich the global state structure with a new toolbar driver. *)
st#dotoptions#set_toolbar_driver (new high_level_toolbar_driver ())


(* ******************************* *
        Callbacks definition
 * ******************************* *)

let (opt,net) = (st#dotoptions, st#network)

(* Tool *)
let fold_lines = function [] -> "" | l-> List.fold_left (fun x y -> x^" "^y) (List.hd l) (List.tl l)

(** Reaction for the iconsize tuning *)
let iconsize_react () = if opt#gui_callbacks_disable then () else
  begin
   let size = opt#toolbar_driver#get_iconsize in
   opt#iconsize#set size;
   st#flash ~delay:4000 (Printf.sprintf (f_ "The icon size is fixed to value %s (default=large)") size);
  end

(** Reaction for the shuffle tuning *)
let shuffle_react () =
  begin
   opt#shuffler#set (ListExtra.shuffleIndexes (net#nodes));
   let namelist = net#get_node_names => ( (ListExtra.permute opt#shuffler_as_function) || fold_lines ) in
   st#flash ~delay:4000 ((s_ "Icons randomly arranged: ")^namelist);
  end

(** Reaction for the unshuffle tuning *)
let unshuffle_react () =
  begin
   opt#reset_shuffler ();
   let namelist = (net#get_node_names => fold_lines) in
   st#flash ~delay:4000 ((s_ "Default icon arrangement: ")^namelist);
  end

(** Reaction for the rankdir tunings *)
let rankdir_react x () =
  begin
   st#dotoptions#rankdir#set x;
   let msg = match x with
    | "TB" -> (s_ "Arrange edges top-to-bottom (default)")
    | "LR" -> (s_ "Arrange edges left-to-right")
    | _    -> "Not valid Rankdir" in
   st#flash ~delay:4000 msg;
  end

(** Reaction for the nodesep tuning *)
let nodesep_react () = if opt#gui_callbacks_disable then () else
  begin
   let y = opt#toolbar_driver#get_nodesep in
   opt#nodesep#set y;
   st#flash (Printf.sprintf (f_ "The minimum edge size (distance between nodes) is fixed to the value %s (default=0.5)") (string_of_float y));
  end

(** Reaction for the labeldistance tuning *)
let labeldistance_react () = if opt#gui_callbacks_disable then () else
  begin
   let y = opt#toolbar_driver#get_labeldistance in
   opt#labeldistance#set y;
   st#flash (Printf.sprintf (f_ "The distance between labels and icons is fixed to the value %s (default=1.6)") (string_of_float y));
  end

(** Reaction for the extrasize_x tuning *)
let extrasize_react () = if opt#gui_callbacks_disable then () else
  begin
   let y = opt#toolbar_driver#get_extrasize in
   opt#extrasize#set y;
   st#flash (Printf.sprintf (f_ "The canvas size is fixed to %s%% of the minimun value to contain the graph (default=0%%)") (string_of_int (int_of_float y)) );
  end

(** Reaction for a rotate tuning *)
let reverse_edge_callback x () =
  begin
   let c = (st#network#get_cable_by_name x) in
   c#set_reversed (not c#is_reversed);
   st#flash (Printf.sprintf (f_ "Cable %s reversed") x);
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
 let set_active cname = (List.mem cname st#network#reversed_cables) in
 (Widget.DynamicSubmenu.make
   ~set_active
   ~submenu:widget_menu
   ~menu:widget
   ~dynList
   ~action:reverse_edge_callback ()) ;   ()
 end

(* Ensure that the image will be shown (in spite of a possibly opposite Gnome/Ubuntu global setting) *)
let () = st#mainwin#imagemenuitem_DOT_TUNING_INVERT#image#misc#show ()

(* Connect INVERT_DIRECT *)
let _ =
  connect_rotate_menu
     ~widget:st#mainwin#imagemenuitem_DOT_TUNING_INVERT_DIRECT
     ~widget_menu:st#mainwin#imagemenuitem_DOT_TUNING_INVERT_DIRECT_menu
     ~dynList:(fun () -> st#network#get_direct_cable_names)

(* Connect INVERT_CROSSOVER *)
let _ =
  connect_rotate_menu
     ~widget:st#mainwin#imagemenuitem_DOT_TUNING_INVERT_CROSSOVER
     ~widget_menu:st#mainwin#imagemenuitem_DOT_TUNING_INVERT_CROSSOVER_menu
     ~dynList:(fun () -> st#network#get_crossover_cable_names)

end (* Make *)
