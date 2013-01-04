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

type performed = Performed of bool
val max_number_of_iterations : int

type tracing_policy = Top_tracing_policy | Bot_tracing_policy
val tracing_enable : unit -> bool
val tracing_policy : tracing_policy

class type tracing_methods =
  object
    method set        : bool -> unit
    method is_enabled : bool
    method level      : int
    method lparen     : string -> unit
    method message    : string -> unit
    method rparen     : string -> unit
    method tab        : char -> string
  end

class ['a] container :
  list_name:string ->
  owner:string ->
  with_mutex: ((unit->unit) -> unit) ->
  object
    constraint 'a = < tracing : tracing_methods;  add_destroy_callback : (unit->unit) -> unit; .. >
    method content : 'a list
    method remove  : 'a -> unit
    method add     : 'a -> unit
  end

class mutex_methods : ?mutex:MutexExtra.Recursive.t -> user:string -> unit ->
  object
    method mutex      : MutexExtra.Recursive.t
    method lock       : unit
    method unlock     : unit
    method with_mutex : (unit -> 'a) -> 'a
  end

class virtual destroy_methods : unit ->
  object
    method add_destroy_callback : (unit -> unit) -> unit
    method destroy : unit -> unit
    method private virtual tracing : tracing_methods
  end

class common :
  ?id:int -> name:string -> ?parent:common -> unit ->
  object
    method id : int
    method name : string
    method parent : common option
    method set_parent : common option -> unit
    method parent_list : common list
    method as_common : common
    method dot_reference : string
  end

class virtual component :
  name:string ->  parent:common -> system ->
  object
    method id : int
    method name : string
    method parent : common option
    method set_parent : common option -> unit
    method parent_list : common list
    method as_common : common
    method dot_reference : string
    method system : system
    method destroy : unit -> unit
    method add_destroy_callback : (unit -> unit) -> unit
    method tracing : tracing_methods
    method virtual to_dot : string
  end

and virtual chip : ?name:string -> system ->
  object
    method id : int
    method name : string
    method parent : common option
    method set_parent : common option -> unit
    method parent_list : common list
    method as_common : common
    method dot_reference : string
    method system : system
    method destroy : unit -> unit
    method add_destroy_callback : (unit -> unit) -> unit
    method tracing : tracing_methods

    val virtual in_port_names : string list
    val virtual out_port_names : string list
    method in_port_names : string list
    method out_port_names : string list
    method port_names : string list
    method kind : [ `Relay | `Sink | `Source ]
    method is_reactive : bool
    method virtual input_wire_connections  : (string * (common option)) list
    method virtual output_wire_connections : (string * (common option)) list
    method to_dot : string
  end

and virtual source : ?name:string -> system ->
  object
    method id : int
    method name : string
    method parent : common option
    method set_parent : common option -> unit
    method parent_list : common list
    method as_common : common
    method dot_reference : string
    method system : system
    method destroy : unit -> unit
    method add_destroy_callback : (unit -> unit) -> unit
    method tracing : tracing_methods

    val virtual in_port_names : string list
    val virtual out_port_names : string list
    method in_port_names : string list
    method out_port_names : string list
    method port_names : string list
    method kind : [ `Relay | `Sink | `Source ]
    method is_reactive : bool
    method virtual input_wire_connections  : (string * (common option)) list
    method virtual output_wire_connections : (string * (common option)) list
    method to_dot : string
  end

and virtual reactive : ?name:string -> system ->
  object
    method id : int
    method name : string
    method parent : common option
    method set_parent : common option -> unit
    method parent_list : common list
    method as_common : common
    method dot_reference : string
    method system : system
    method destroy : unit -> unit
    method add_destroy_callback : (unit -> unit) -> unit
    method tracing : tracing_methods

    val virtual in_port_names : string list
    val virtual out_port_names : string list
    method in_port_names : string list
    method out_port_names : string list
    method port_names : string list
    method kind : [ `Relay | `Sink | `Source ]
    method is_reactive : bool
    method virtual input_wire_connections  : (string * (common option)) list
    method virtual output_wire_connections : (string * (common option)) list
    method to_dot : string

    method virtual stabilize : performed
    method traced_stabilize : performed
  end

and virtual relay : ?name:string -> system ->
  object
    method id : int
    method name : string
    method parent : common option
    method set_parent : common option -> unit
    method parent_list : common list
    method as_common : common
    method dot_reference : string
    method system : system
    method destroy : unit -> unit
    method add_destroy_callback : (unit -> unit) -> unit
    method tracing : tracing_methods

    val virtual in_port_names : string list
    val virtual out_port_names : string list
    method in_port_names : string list
    method out_port_names : string list
    method port_names : string list
    method kind : [ `Relay | `Sink | `Source ]
    method is_reactive : bool
    method virtual input_wire_connections  : (string * (common option)) list
    method virtual output_wire_connections : (string * (common option)) list
    method to_dot : string

    method virtual stabilize : performed
    method traced_stabilize : performed
  end

and virtual sink : ?name:string -> system ->
  object
    method id : int
    method name : string
    method parent : common option
    method set_parent : common option -> unit
    method parent_list : common list
    method as_common : common
    method dot_reference : string
    method system : system
    method destroy : unit -> unit
    method add_destroy_callback : (unit -> unit) -> unit
    method tracing : tracing_methods

    val virtual in_port_names : string list
    val virtual out_port_names : string list
    method in_port_names : string list
    method out_port_names : string list
    method port_names : string list
    method kind : [ `Relay | `Sink | `Source ]
    method is_reactive : bool
    method virtual input_wire_connections  : (string * (common option)) list
    method virtual output_wire_connections : (string * (common option)) list
    method to_dot : string

    method virtual stabilize : performed
    method traced_stabilize : performed
  end

and system :
  ?father:system ->
  ?name:string ->
  ?max_number_of_iterations:int ->
  ?tracing:(unit->bool) ->
  ?set_default_system:bool ->
  unit ->
  object
    method id : int
    method name : string
    method parent : common option
    method set_parent : common option -> unit
    method parent_list : common list
    method as_common : common
    method dot_reference : string
    method mutex_methods : mutex_methods
    method tracing : tracing_methods

    method reactive_list  : reactive container
    method component_list : component container
    method relay_list     : relay list
    method sink_list      : sink list
    method show_component_list : unit -> unit
    method to_dot : string

    method stabilize : performed
  end

and virtual ['a, 'b] wire : ?name:string -> ?parent:common -> system ->
  object
    method id : int
    method name : string
    method parent : common option
    method set_parent : common option -> unit
    method parent_list : common list
    method as_common : common
    method dot_reference : string
    method system : system
    method destroy : unit -> unit
    method add_destroy_callback : (unit -> unit) -> unit
    method tracing : tracing_methods
    method to_dot : string

    method set : 'a -> unit
    method get : 'b
    method virtual set_alone : 'a -> unit
    method virtual get_alone : 'b
    method virtual reset : unit -> unit
  end

class ['a] wref : ?name:string -> ?parent:common -> system -> 'a ->
  object
    method id : int
    method name : string
    method parent : common option
    method set_parent : common option -> unit
    method parent_list : common list
    method as_common : common
    method dot_reference : string
    method system : system
    method destroy : unit -> unit
    method add_destroy_callback : (unit -> unit) -> unit
    method tracing : tracing_methods
    method to_dot : string

    method set : 'a -> unit
    method get : 'a
    method set_alone : 'a -> unit
    method get_alone : 'a
    method reset : unit -> unit
  end

class wcounter : ?name:string -> ?parent:common -> system ->
  object
    method id : int
    method name : string
    method parent : common option
    method set_parent : common option -> unit
    method parent_list : common list
    method as_common : common
    method dot_reference : string
    method system : system
    method destroy : unit -> unit
    method add_destroy_callback : (unit -> unit) -> unit
    method tracing : tracing_methods
    method to_dot : string

    method set : unit -> unit
    method get : int
    method set_alone : unit -> unit
    method get_alone : int
    method reset : unit -> unit
  end

class wswitch : ?name:string -> ?parent:common -> system -> bool ->
  object
    method id : int
    method name : string
    method parent : common option
    method set_parent : common option -> unit
    method parent_list : common list
    method as_common : common
    method dot_reference : string
    method system : system
    method destroy : unit -> unit
    method add_destroy_callback : (unit -> unit) -> unit
    method tracing : tracing_methods
    method to_dot : string

    method set : unit -> unit
    method get : bool
    method set_alone : unit -> unit
    method get_alone : bool
    method reset : unit -> unit
    method set_to : bool -> unit
    method as_wire : (unit, bool) wire

  end
  
class ['a] wlist : ?name:string -> ?parent:common -> system -> 'a list ->
  object
    method id : int
    method name : string
    method parent : common option
    method set_parent : common option -> unit
    method parent_list : common list
    method as_common : common
    method dot_reference : string
    method system : system
    method destroy : unit -> unit
    method add_destroy_callback : (unit -> unit) -> unit
    method tracing : tracing_methods
    method to_dot : string

    method set : 'a list -> unit
    method get : 'a list
    method set_alone : 'a list -> unit
    method get_alone : 'a list
    method reset : unit -> unit

    method add : 'a -> unit
    method update_with : ('a list -> 'a list) -> unit

    method coerce : ('a list, 'a list) wire
  end

class ['a, 'b] cable :
  ?name:string   ->
  ?tracing:(unit->bool)  ->
  ?parent:common ->
  system ->
  object
    method id : int
    method name : string
    method parent : common option
    method set_parent : common option -> unit
    method parent_list : common list
    method as_common : common
    method dot_reference : string
    method system : system
    method destroy : unit -> unit
    method add_destroy_callback : (unit -> unit) -> unit
    method tracing : tracing_methods
    method to_dot : string

    method wire_list : ('a,'b) wire container

    method set : int * 'a -> unit
    method get : (int * 'b) list
    method set_alone : int * 'a -> unit
    method get_alone : (int * 'b) list
    method reset : unit -> unit
end

class type ['a] writeonly_wire =
  object
    method id : int
    method name : string
    method parent : common option
    method set_parent : common option -> unit
    method parent_list : common list
    method as_common : common
    method dot_reference : string
    method system : system
    method destroy : unit -> unit
    method add_destroy_callback : (unit -> unit) -> unit
    method tracing : tracing_methods
    method to_dot : string
    method set : 'a -> unit
    method set_alone : 'a -> unit
  end

class type ['b] readonly_wire =
  object
    method id : int
    method name : string
    method parent : common option
    method set_parent : common option -> unit
    method parent_list : common list
    method as_common : common
    method dot_reference : string
    method system : system
    method destroy : unit -> unit
    method add_destroy_callback : (unit -> unit) -> unit
    method tracing : tracing_methods
    method to_dot : string
    method get : 'b
    method get_alone : 'b
  end

class ['a] option_obj : ?name:string -> ?owner:string -> 'a option ->
  object
    method get : 'a option
    method set : 'a option -> unit
    method extract : 'a
    method map  : 'b. ('a -> 'b) -> 'b option
    method bind : 'b. ('a -> 'b option) -> 'b option
  end

class ['a] in_port_connection : equality:('a -> 'a -> bool) -> ('b, 'a) wire ->
  object
    method read_and_compare : bool -> 'a * bool
    method readonly_wire : 'a readonly_wire
  end

class ['a] in_port :
  ?sensitiveness:bool ->
  ?equality:('a -> 'a -> bool) ->
  name:string ->
  ?wire:('b, 'a) wire ->
  unit ->
  object
    method name : string
    method connect : 'c. ('c, 'a) wire -> unit
    method connection : ('a in_port_connection) option_obj
    method disconnect : unit
    method sensitiveness : bool
    method equality : 'a -> 'a -> bool
    method read_and_compare : 'a * bool
    method set_equality : ('a -> 'a -> bool) -> unit
    method set_sensitiveness : bool -> unit
  end

class ['a] out_port_connection : ('a, 'b) wire ->
 object
  method writeonly_wire : 'a writeonly_wire
 end

class ['a] out_port :
  name:string ->
  ?wire:('a, 'b) wire ->
  unit ->
  object
    method name : string
    method connect : 'c. ('a, 'c) wire -> unit
    method connection : ('a out_port_connection) option_obj
    method disconnect : unit
  end

val teach_ocamldep : unit
val initialize : unit -> unit
val stabilize : unit -> performed
val get_current_system : unit -> system
val get_or_initialize_current_system : unit -> system

val wref     : ?name:string -> ?parent:common -> ?system:system -> 'a -> 'a wref
val wlist    : ?name:string -> ?parent:common -> ?system:system -> 'a list -> 'a wlist
val wcounter : ?name:string -> ?parent:common -> ?system:system -> unit -> wcounter
val wswitch  : ?name:string -> ?parent:common -> ?system:system -> bool -> wswitch

val cable : ?name:string -> ?parent:common -> ?system:system -> unit -> ('a, 'a) cable
val wref_in_cable : ?name:string -> cable:('a,'a) cable -> 'a -> 'a wref

val current_system : system option ref
val wire_of_accessors : ?name:string -> ?parent:common -> ?system:system -> reset_with:'a -> get:(unit -> 'b) -> set:('a -> unit) -> unit -> ('a,'b) wire

val fresh_chip_name   : string -> string
val fresh_source_name : string -> string
val fresh_relay_name  : string -> string
val fresh_sink_name   : string -> string
val fresh_wire_name   : string -> string
val fresh_system_name : string -> string

val dot_record_of_port_names :string list -> string
