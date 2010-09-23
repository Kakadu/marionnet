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

val make_form_with_labels :
  ?row_spacings:int ->
  ?col_spacings:int ->
  ?packing:(GObj.widget -> unit) ->
  string list ->
  < add : < coerce : GObj.widget; .. > -> unit;
    coerce : GObj.widget;
    table : GPack.table >

val wrap_with_label :
  ?packing:(GObj.widget -> unit) ->
  ?labelpos:[< `EAST | `NORTH | `SOUTH | `WEST > `NORTH ] ->
  string ->
  (< coerce : GObj.widget; .. > as 'a) -> 'a

val entry_with_label :
  ?packing:(GObj.widget -> unit) ->
  ?max_length:int ->
  ?entry_text:string ->
  ?labelpos:[< `EAST | `NORTH | `SOUTH | `WEST > `NORTH ] ->
  string -> GEdit.entry

val add_label_and_labelpos_parameters :
  ?label:string ->
  ?labelpos:[< `EAST | `NORTH | `SOUTH | `WEST > `NORTH ] ->
  ?packing:(GObj.widget -> unit) ->
  (?packing:(GObj.widget -> unit) ->
   unit ->
   (< coerce : GObj.widget; .. > as 'a)) -> 'a

val spin_byte :
  ?label:string ->
  ?labelpos:[< `EAST | `NORTH | `SOUTH | `WEST > `NORTH ] ->
  ?lower:int -> 
  ?upper:int -> 
  ?step_incr:int -> 
  ?packing:(GObj.widget -> unit) ->
  int -> GEdit.spin_button

val spin_ipv4_address :
  ?label:string ->
  ?labelpos:[< `EAST | `NORTH | `SOUTH | `WEST > `NORTH ] ->
  ?packing:(GObj.widget -> unit) ->
  int -> int -> int -> int ->
  GEdit.spin_button * GEdit.spin_button * GEdit.spin_button * GEdit.spin_button

val spin_ipv4_address_with_cidr_netmask :
  ?label:string ->
  ?labelpos:[< `EAST | `NORTH | `SOUTH | `WEST > `NORTH ] ->
  ?packing:(GObj.widget -> unit) ->
  int -> int -> int -> int -> int ->
  GEdit.spin_button * GEdit.spin_button * GEdit.spin_button * GEdit.spin_button * GEdit.spin_button

val make_tooltips_for_container :
  < connect : < destroy : callback:('a -> unit) -> 'b; .. >; .. > ->
  GObj.widget ->
  string -> unit

module Dialog_run : sig

  val ok_or_cancel :
    [ `CANCEL | `DELETE_EVENT | `HELP | `OK ] GWindow.dialog ->
    get_widget_data:(unit -> 'a) ->
    ok_callback:('a -> 'b option) ->
    ?help_callback:(unit -> unit) ->
    unit -> 'b option

  val yes_or_cancel :
    [ `CANCEL | `DELETE_EVENT | `HELP | `YES ] GWindow.dialog ->
    ?help_callback:(unit -> unit) ->
    context:'a ->
    unit -> 'a option

  val yes_no_or_cancel :
    [ `CANCEL | `DELETE_EVENT | `HELP | `NO | `YES ] GWindow.dialog ->
    ?help_callback:(unit -> unit) ->
    context:'a ->
    unit -> ('a * bool) option

end (* Dialog_loop *)

module Dialog : sig

  val yes_or_cancel_question :
    ?title:string ->
    ?help_callback:(unit -> unit) ->
    ?image_filename:string ->
    ?markup:string ->
    ?text:string ->
    context:'a ->
    unit -> 'a option

  val yes_no_or_cancel_question :
    ?title:string ->
    ?help_callback:(unit -> unit) ->
    ?image_filename:string ->
    ?markup:string ->
    ?text:string ->
    context:'a ->
    unit -> ('a * bool) option

end (* Dialog_loop *)


val set_marionnet_icon : [> ] GWindow.dialog -> unit
(*   < set_icon : GdkPixbuf.pixbuf option -> 'a; .. > -> 'a = <fun> *)
  
val test: unit -> char option
