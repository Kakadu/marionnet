(* This file is part of Marionnet
   Copyright (C) 2010 Jean-Vincent Loddo

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


(** Make a classic rectangular input form with field labels at the left side
    and input widgets at the right side of each line.
    Labels are get from the input string list while input widgets are
    added later using the method [add]. {b Example}:
{[...
let form =
  Gui_Bricks.make_form_with_labels
    ~packing:vbox#add
    ["IPv4 address"; "DHCP service"]
in
let ipv4address  = GEdit.entry ~text:"10.0.2.1" ~packing:form#add () in
let dhcp_enabled = GButton.check_button ~packing:form#add () in
...}]
*)
let make_form_with_labels ?(row_spacings=10) ?(col_spacings=10) ?packing string_list =
 let rows = List.length string_list in
 let table = GPack.table ~row_spacings ~col_spacings ~rows ~columns:2 ~homogeneous:false ?packing () in
 let () =
   Array.iteri
     (fun i label_text ->
        let label = GMisc.label ~xalign:0. ~text:label_text () in
        table#attach ~left:0 ~top:i label#coerce)
     (Array.of_list string_list)
 in
 object
   method table = table
   method coerce = table#coerce
   val mutable index = 0
   method add widget =
     let aligned_widget =
       let box = GBin.alignment ~xalign:0. ~yalign:0.5 ~xscale:0.0 ~yscale:0.0 () in
       box#add widget#coerce;
       box
     in
     table#attach ~left:1 ~top:index aligned_widget#coerce;
     index <- index+1
 end

(** Wrap the given widget with a label, using an hidden table which will be packaged
    in its container (if provided). The result is the input widget itself.
    {b Example}:
{[
let entry_with_label ?packing ?max_length ?entry_text ?labelpos label_text =
  let entry = GEdit.entry ?text:entry_text ?max_length () in
  Gui_Bricks.wrap_with_label ?packing ?labelpos label_text entry
]}
*)
let wrap_with_label ?packing ?(labelpos=`NORTH) label_text widget =
 let label = GMisc.label ~text:label_text () in
 let (rows, columns) =
   match labelpos with
   | `NORTH | `SOUTH -> 2,1
   | `EAST  | `WEST  -> 1,2
 in
 let table = GPack.table ~rows ~columns ~homogeneous:true ?packing () in
 let () = match labelpos with
   | `NORTH -> table#attach ~left:0 ~top:0 label#coerce;  table#attach ~left:0 ~top:1 widget#coerce
   | `SOUTH -> table#attach ~left:0 ~top:0 widget#coerce; table#attach ~left:0 ~top:1 label#coerce
   | `WEST  -> table#attach ~left:0 ~top:0 label#coerce;  table#attach ~left:1 ~top:0 widget#coerce
   | `EAST  -> table#attach ~left:0 ~top:0 widget#coerce; table#attach ~left:1 ~top:0 label#coerce
 in
 widget

(** A simple [GEdit.entry] equipped by a label specified as a string. *)
let entry_with_label ?packing ?max_length ?entry_text ?labelpos label_text =
  let entry = GEdit.entry ?text:entry_text ?max_length () in
  wrap_with_label ?packing ?labelpos label_text entry

(** Not in the interface.*)
let add_label_and_labelpos_parameters ?label ?labelpos ?packing maker =
  match label with
  | None   -> maker ?packing ()
  | Some label_text ->
      let result = maker ?packing:None () in
      let _ = wrap_with_label ?packing ?labelpos label_text result in
      result

(** A spin for bytes, i.e. for values in the range [0..255].  *)
let spin_byte ?label ?labelpos ?(lower=0) ?(upper=255) ?(step_incr=1) ?packing value =
  let lower = float_of_int lower in
  let upper = float_of_int upper in
  let step_incr = float_of_int step_incr in
  let maker ?packing () =
    let sb = GEdit.spin_button ?packing ~width:60 ~digits:0 ~numeric:true () in
    sb#adjustment#set_bounds ~lower ~upper ~step_incr ();
    sb#set_value (float_of_int value);
    sb
  in
  add_label_and_labelpos_parameters ?label ?labelpos ?packing maker
;;

(** Four spins for asking for an ipv4 address. *)
let spin_ipv4_address ?label ?labelpos ?packing v1 v2 v3 v4 =
  let maker ?packing () =
    let table = GPack.table ~rows:1 ~columns:7 ~homogeneous:false ?packing () in
    let dot ~left = GMisc.label ~packing:(table#attach ~left ~top:0) ~width:15 ~markup:"<b>.</b>" () in
    let s1 = spin_byte ~packing:(table#attach ~left:0 ~top:0) v1 in
    let _1 = dot ~left:1 in
    let s2 = spin_byte ~packing:(table#attach ~left:2 ~top:0) v2 in
    let _2 = dot ~left:3 in
    let s3 = spin_byte ~packing:(table#attach ~left:4 ~top:0) v3 in
    let _3 = dot ~left:5 in
    let s4 = spin_byte ~packing:(table#attach ~left:6 ~top:0) v4 in
    (table,(s1,s2,s3,s4))
  in
  match label with
  | None   -> snd (maker ?packing ())
  | Some label_text ->
      let (table,(s1,s2,s3,s4)) = maker ?packing:None () in
      let _ = wrap_with_label ?packing ?labelpos label_text table in
      (s1,s2,s3,s4)

(** Four spins for asking for an ipv4 address, and a fifth for
    the netmask (in CIDR notation).  *)
let spin_ipv4_address_with_cidr_netmask ?label ?labelpos ?packing v1 v2 v3 v4 v5 =
  let maker ?packing () =
    let table = GPack.table ~rows:1 ~columns:9 ~homogeneous:false ?packing () in
    let dot ~left = GMisc.label ~packing:(table#attach ~left ~top:0) ~width:15 ~markup:"<b>.</b>" () in
    let s1 = spin_byte ~packing:(table#attach ~left:0 ~top:0) v1 in
    let _1 = dot ~left:1 in
    let s2 = spin_byte ~packing:(table#attach ~left:2 ~top:0) v2 in
    let _2 = dot ~left:3 in
    let s3 = spin_byte ~packing:(table#attach ~left:4 ~top:0) v3 in
    let _3 = dot ~left:5 in
    let s4 = spin_byte ~packing:(table#attach ~left:6 ~top:0) v4 in
    let _slash = GMisc.label ~packing:(table#attach ~left:7 ~top:0) ~width:15 ~markup:"<b>/</b>" () in
    let s5 = spin_byte ~packing:(table#attach ~left:8 ~top:0) v5 in
    (table,(s1,s2,s3,s4,s5))
  in
  match label with
  | None   -> snd (maker ?packing ())
  | Some label_text ->
      let (table,(s1,s2,s3,s4,s5)) = maker ?packing:None () in
      let _ = wrap_with_label ?packing ?labelpos label_text table in
      (s1,s2,s3,s4,s5)

(** {b Example}:
{[
let tooltips = Gui_Bricks.make_tooltips_for_container window in
tooltips label#coerce "hello";
tooltips entry#coerce "salut";
]}
*)
let make_tooltips_for_container w =
  let result = (GData.tooltips ()) in
  let _ = w#connect#destroy ~callback:(fun _ -> result#destroy ()) in
  fun (widget:GObj.widget) text -> result#set_tip widget ~text
