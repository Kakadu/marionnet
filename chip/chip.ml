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


(** Runtime support for the "chip" syntax extension. *)

module Recursive_mutex = MutexExtra.Recursive ;;

(* Fresh name generators. *)

let fresh_id = let x = ref 0 in
 fun () -> (incr x); !x

let fresh_name_generator () = let x = ref 0 in
 fun prefix -> (incr x); Printf.sprintf "%s%i" prefix !x

let fresh_chip_name   = fresh_name_generator ()
let fresh_source_name = fresh_name_generator ()
let fresh_relay_name  = fresh_name_generator ()
let fresh_sink_name   = fresh_name_generator ()
let fresh_wire_name   = fresh_name_generator ()
let fresh_system_name = fresh_name_generator ()

type performed = Performed of bool
let max_number_of_iterations = 1024
let current_system = ref None

type tracing_policy =
 | Top_tracing_policy (** If the system is traced, all components are traced. *)
 | Bot_tracing_policy (** Each component decides individually. *)

(* let tracing_enable = Global_options.Debug_level.get *)
let tracing_enable = Log.Tuning.is_log_enabled ~v:2

let tracing_policy = Top_tracing_policy

class type tracing_methods =
  object
    method is_enabled : bool
    method set : bool -> unit
    method level : int
    method lparen : string -> unit
    method message : string -> unit
    method rparen : string -> unit
    method tab : char -> string
  end

(** Tool for dot traduction. *)
let dot_record_of_port_names (xs:string list) =
 if xs = [] then "" else
 let args = (List.map (fun x-> Printf.sprintf "<%s> %s" x x) xs) in
 List.fold_left (fun x y -> x^" | "^y) (List.hd args) (List.tl args)

(** Containers of components. When a component which belongs a container is destroyed,
    it is automatically removed from the container. *)
class ['a] container ~(list_name:string) ~(owner:string) ~(with_mutex: (unit->unit) -> unit) =
  object (self)

  constraint 'a = < tracing : tracing_methods;
                    add_destroy_callback : (unit->unit) -> unit;
                    .. >

  val mutable content : 'a list = []
  method content = content

  method add (x:'a) =
   x#tracing#lparen ("subscribing to the "^owner^"'s list of "^list_name^"...");
   let action () = begin
     content <- x :: content;
     x#add_destroy_callback (fun () -> self#remove x);
     x#tracing#rparen "done."
    end in
   with_mutex action

  method remove (x:'a) =
   x#tracing#lparen ("unsubscribing from the "^owner^"'s list of "^list_name^"...");
   let action () = begin
     content <- List.filter (fun y -> y!=x) content;
     x#tracing#rparen "done."
    end in
   with_mutex action

 end

(** Wires perform their operations (set/get) preventing from effects of other threads.
    In a single-thread application this support is unnecessary. *)
class mutex_methods ?(mutex=Recursive_mutex.create ()) ~(user:string) () =
  object (self)
  method mutex  = mutex
  method lock   = Recursive_mutex.lock   mutex
  method unlock = Recursive_mutex.unlock mutex
  method with_mutex : 'a. ( unit -> 'a) -> 'a =
    Recursive_mutex.with_mutex mutex
  end (* mutex_methods *)

(** Support for managing the component garbage. *)
class virtual destroy_methods () =
  object (self)
  val mutable destroy_callbacks = []
  method add_destroy_callback f = (destroy_callbacks <- f::destroy_callbacks)
  method destroy () =
   let () = self#tracing#lparen "destroying me..." in
   let () = List.iter (fun f -> f ()) destroy_callbacks in
   let () = self#tracing#rparen "destroyed." in ()
  method private virtual tracing : tracing_methods
  end (* destroy_methods *)

(** Support for tracing system's activities. *)
class tracing_methods_for_systems ~(name:string) ~(tracing:unit->bool) =
  object (self)

  method is_enabled = tracing ()
  method set x = Global_options.Debug_level.set_from_bool x

  val mutable level = 0
  method level = level
  method tab prompt =
   let n = self#level in
   (String.make n ' ')^(String.make (n+1) prompt)^" "

  method lparen msg =
   if not self#is_enabled then () else
    begin
     self#message_with_prompt '+' msg;
     level <- level + 1 ;
    end

  method rparen msg =
   if not self#is_enabled then () else
    begin
     level <- level - 1 ;
     self#message_with_prompt '-' msg;
    end

  method private message_with_prompt prompt msg =
   if not self#is_enabled then () else
    begin
     Printf.eprintf "%s%s: %s\n" (self#tab prompt) name msg;
     flush stderr
    end

  method message msg =
   if not self#is_enabled then () else self#message_with_prompt '*' msg

  end (* object *)

(** Support for tracing component's activities. *)
class tracing_methods_for_components ~(id:int) ~(name:string) system =
  let format_message msg = (Printf.sprintf "%s (%d): %s" name id msg) in
  object (self)
   inherit tracing_methods_for_systems ~name ~tracing:(fun () -> system#tracing#is_enabled) as super
   method is_enabled = match tracing_policy with
    | Top_tracing_policy -> super#is_enabled || system#tracing#is_enabled
    | Bot_tracing_policy -> super#is_enabled
   method level = system#tracing#level + level
   method lparen  msg = system#tracing#lparen  (format_message msg)
   method rparen  msg = system#tracing#rparen  (format_message msg)
   method message msg = system#tracing#message (format_message msg)
  end

class

 (** Common features for chips, wires and systems. Components have a parent,
     systems may not. *)
 common ?(id=fresh_id ()) ~(name:string) ?parent () =
  object (self)
   method id = id
   method name = name

   val mutable parent = parent
   method parent : common option = parent
   method set_parent x = parent <- x
   method parent_list =
     match self#parent with
     | None   -> []
     | Some p -> p :: (p#parent_list)

   method as_common = (self :> common)

   (* parent_list=[y;z] => dot_reference=y:x (where x is the self#id and z is the system) *)
   method dot_reference : string =
    let pl = List.tl (List.rev ((self#as_common)::self#parent_list)) in
    let pl = List.map (fun p -> string_of_int p#id) pl in
    List.fold_left (fun x y -> x^":"^y) (List.hd pl) (List.tl pl)

 end

and

 (** Common features for system components (chips and wires).
     This class collect common methods and performs the system subscription
     (only when the parent is the system). *)
 virtual component ~(name:string) ~(parent:common) (system:system) =
  let id = fresh_id () in
  let tracing_methods = new tracing_methods_for_components ~id ~name system in
  let given_parent = parent in
  object (self)
  inherit common ~id ~name ~parent ()
  inherit destroy_methods ()

  method system = system
  method tracing = tracing_methods
  method virtual to_dot : string

  method set_parent new_parent =
   begin
   let old_parent = self#parent in
   (match new_parent, old_parent with
   | (Some np) , (Some op) when (op#id = system#id) && (op#id <> np#id)
       -> system#component_list#remove (self :> component)
   | _ -> ()
   );
   parent <- new_parent
   end

  initializer
  (* Subscribe to the component list if the system is also the parent: *)
   if given_parent#id = system#id then system#component_list#add (self :> component)
 end

and

(** A chip is a box with some input ports, some output ports and possibly an internal state.
    Each port is a reference that can be dynamically linked to a wire. *)
 virtual chip ?name (system:system) =
  let name = match name with None -> fresh_chip_name "chip" | Some x -> x in
  object (self)
  inherit component ~name ~parent:system#as_common system

  (* For chip classification (reflection), debugging and drawing purposes *)
  val virtual in_port_names  : string list
  val virtual out_port_names : string list
  method in_port_names  =  in_port_names
  method out_port_names = out_port_names
  method port_names = List.append in_port_names out_port_names
  method virtual input_wire_connections  : (string * (common option)) list
  method virtual output_wire_connections : (string * (common option)) list

  (* This method performs a part of the work: wire connections are translated into dot edges (->). *)
  method to_dot : string =
   let xs = List.filter (function (p,x) -> x<>None) self#input_wire_connections  in
   let ys = List.filter (function (p,x) -> x<>None) self#output_wire_connections in
   let xs = List.map    (function (p, Some w) -> (Printf.sprintf " %s -> %d:%s;\n" w#dot_reference self#id p) | _ -> assert false) xs in
   let ys = List.map    (function (p, Some w) -> (Printf.sprintf " %d:%s -> %s;\n" self#id p w#dot_reference) | _ -> assert false) ys in
   List.fold_left (^) "" (xs@ys)

  method kind : [ `Source | `Sink | `Relay ] =
   match in_port_names, out_port_names with
   | [],[] -> assert false
   | [], _ -> `Source
   | _ ,[] -> `Sink
   | _ ,_  -> `Relay

  method is_reactive : bool = self#kind <> `Source
 end

and

 (** A source is a chip with 0 in-degree. In other words, is an active (not reactive) system.
     Each instance of this class has a method 'emit' with a specific signature allowing the
     user to set several wires at the same time. *)
 virtual source ?name (system:system) =
  let name = match name with None -> fresh_source_name "source" | Some x -> x in
  object (self) inherit chip ~name system as self_as_chip
  method to_dot =
   Printf.sprintf "%d [fillcolor=\"#FFDAB9\", label=\"%s (%d) | { \\ | {%s} }\"];\n%s"
     self#id
     self#name
     self#id
     (dot_record_of_port_names self#out_port_names)
     self_as_chip#to_dot

  initializer
   (* Run-time type checking *)
   assert (self#kind = `Source)
 end

and

 (** A reactive chip is a chip with non zero in-degree. Such chips have a stabilization method
     that perform internal transitions and/or output ports setting with the current value
     of wires connected to input ports. *)
 virtual reactive ?name (system:system) =
  object (self)
  inherit chip ?name system as self_as_chip

  method virtual stabilize : performed

  method traced_stabilize =
   if self#tracing#is_enabled = false then self#stabilize else
    begin
      self#tracing#lparen "stabilization...";
      let result = self#stabilize in
      let msg = match result with
       | Performed false -> "chip already stabilized."
       | Performed true  -> "stabilized."
      in
      self#tracing#rparen msg;
      result
    end

  initializer
   system#reactive_list#add (self :> reactive)
 end

and

 (** A relay is a reactive chip with non zero in-degree and non zero out-degree. *)
 virtual relay ?name (system:system) =
  let name = match name with None -> fresh_relay_name "relay" | Some x -> x in
  object (self) inherit reactive ~name system as self_as_chip

  method to_dot =
   Printf.sprintf "%d [fillcolor=\"#F0F8FF\", label=\"%s (%d) | { {%s} | \\ | {%s} }\"];\n%s"
     self#id
     self#name
     self#id
     (dot_record_of_port_names self#in_port_names)
     (dot_record_of_port_names self#out_port_names)
     self_as_chip#to_dot

  initializer
   (* Run-time type checking *)
   assert (self#kind = `Relay)
 end

and

 (* A sink is a reactive chip with 0 out-degree. *)
 virtual sink ?name (system:system) =
  let name = match name with None -> fresh_sink_name "sink" | Some x -> x in
  object (self) inherit reactive ~name system as self_as_chip

  method to_dot =
   Printf.sprintf "%d [fillcolor=\"#F5F5DC\", label=\"%s (%d) | { {%s} | \\  }\"];\n%s"
     self#id
     self#name
     self#id
     (dot_record_of_port_names self#in_port_names)
     self_as_chip#to_dot

  initializer
   (* Run-time type checking *)
   assert (self#kind = `Sink)
 end

and

 (** A system is a container of chips and wires. *)
 system
  ?(father: system option)
  ?name
  ?(max_number_of_iterations = max_number_of_iterations)
  ?(tracing = tracing_enable) (* on stderr *)
  ?(set_default_system=true)
  ()
  =
 let name = match name with None -> fresh_system_name "system" | Some x -> x in
 let mutex_methods = match father with
   | None   -> new mutex_methods ~user:name ()
   | Some s -> new mutex_methods ~mutex:s#mutex_methods#mutex ~user:name ()
 in
 let tracing_methods = new tracing_methods_for_systems ~name ~tracing in
 object (self)
  inherit common ~name ()
  method mutex_methods = mutex_methods
  method tracing = tracing_methods

  val reactive_list : reactive container =
   new container ~list_name:"reactive components" ~owner:name ~with_mutex:mutex_methods#with_mutex

  val component_list : component container =
   new container ~list_name:"components" ~owner:name ~with_mutex:mutex_methods#with_mutex

  method component_list = component_list
  method reactive_list = reactive_list
  method relay_list : reactive list = List.filter (fun c->c#kind=`Relay) reactive_list#content
  method sink_list  : reactive list = List.filter (fun c->c#kind=`Sink)  reactive_list#content

  (* For debugging: *)
  method show_component_list () =
   begin
    Printf.eprintf "System: %s\n" name;
    List.iter (fun c -> Printf.eprintf "\t* %s (%d)\n" c#name c#id) component_list#content;
    Printf.eprintf "\t Relays: ";
    List.iter (fun c -> Printf.eprintf "%s (%d) " c#name c#id) self#relay_list;
    Printf.eprintf "\n\t Sinks: ";
    List.iter (fun c -> Printf.eprintf "%s (%d) " c#name c#id) self#sink_list;
    Printf.eprintf "\n---\n"; flush stderr
   end

  (** The system stabilize itself calling the stabilization of its reactive chips.
      Relays are firstly stabilized with a stabilization loop and finally sinks are stabilized
      in a single step. The result indicated if the stabilization has really changed the state
      of some components. *)
  method stabilize : performed =
  let relay_list = self#relay_list in
  let rec loop acc =
    if (acc >= max_number_of_iterations)
     then (failwith (Printf.sprintf "system#stabilize: stabilization failed after %d iterations" max_number_of_iterations))
     else begin
      let performed_list = List.map (fun (x:reactive) -> x#traced_stabilize) relay_list in
      if List.exists ((=)(Performed true)) performed_list then loop (acc+1) else acc
     end
  in
  self#tracing#lparen "system stabilization...";
  self#tracing#lparen "relays' stabilization...";
  let loops = loop 0 in
  self#tracing#rparen "relays' stabilized.";
  self#tracing#lparen "sinks' stabilization...";
  let sink_results = List.map (fun (x:reactive) -> x#traced_stabilize) self#sink_list in
  let result = (loops > 0) || (List.exists ((=)(Performed true)) sink_results) in
  self#tracing#rparen "sinks' stabilized.";
  self#tracing#rparen "system stabilized.";
  (Performed result)

 method to_dot =
 let components = List.fold_left (fun x y -> x^"\n  "^y) "" (List.map (fun c->c#to_dot) component_list#content)
 in Printf.sprintf "
digraph system_%s_%d {
 node [style=filled, shape=Mrecord, fontsize=8];
 edge [arrowtail=inv, arrowsize=0.7];
 rankdir=LR;
 %s
}\n" self#name self#id components

 initializer
  if set_default_system then current_system := Some (self :> system) else ()
 end

and

 (** A wire is a sort of pipe that is constantly stabilized. The common example is a reference.
     You can get and set its value by methods #get and #set_alone (virtual).
     The #set method is the sequence of two actions: the individually set of the wires
     followed by the global set of its system. *)
 virtual ['a,'b] wire ?name ?parent (system:system) =
  let name = match name with None -> fresh_wire_name "wire" | Some x -> x in
  let parent = match parent with None -> system#as_common | Some p -> p in
  object (self)
  inherit component ~name ~parent system

  method virtual get_alone : 'b
  method virtual set_alone : 'a -> unit

  (** This would be a final method. *)
  method get : 'b =
   self#tracing#message "reading wire (get)";
   let action () = self#get_alone in
   self#system#mutex_methods#with_mutex action

  (** This would be a final method. *)
  method set (x:'a) : unit =
   self#tracing#lparen "setting wire...";
   let action () =
     begin
     (self#set_alone x);
     ignore (self#system#stabilize);
     self#tracing#rparen "wire set.";
     end
   in self#system#mutex_methods#with_mutex action

  method to_dot =
   Printf.sprintf "%d [shape=record, label=\"%s (%d)\"];"
     self#id
     self#name
     self#id

 end

(** References are simple examples of wires. *)
class ['a] wref ?name ?parent (system:system) (value:'a) =
 let name = match name with None -> fresh_wire_name "wire" | Some x -> x in
 object (self)
  inherit ['a,'a] wire ~name ?parent system
  val mutable content = value
  method get_alone   = content
  method set_alone v = (content <- v)
 end

(** Counters as wires: "set" means "incr". *)
class wcounter ?name ?parent (system:system) =
 let name = match name with None -> fresh_wire_name "wcounter" | Some x -> x in
 object (self)
  inherit [unit,int] wire ~name ?parent system
  val mutable content = 0
  method get_alone    = content
  method set_alone () = (content <- content + 1)
 end

class ['a] wlist ?name ?parent (system:system) (value:'a list) =
 let name = match name with None -> fresh_wire_name "wlist" | Some x -> x in
 object (self)
  inherit ['a list] wref ~name ?parent system value
  method update_with (f:'a list -> 'a list) =
    self#tracing#message "updating the list";
    let action () =
      content <- f content;
      ignore (self#system#stabilize);
      self#tracing#rparen "wlist updated.";
    in
    self#system#mutex_methods#with_mutex action
  (* Equivalent to: update_with (List.append [x]) *)
  method add x =
    self#tracing#message "adding on top of the list";
    let action () =
      content <- x::content;
      ignore (self#system#stabilize);
      self#tracing#rparen "wlist updated.";
    in
    self#system#mutex_methods#with_mutex action
  method coerce = (self :> ('a list,'a list) wire)
 end

class ['a,'b] wire_of_accessors ?name ?parent (system:system) get_alone set_alone =
 let name = match name with None -> fresh_wire_name "wire_of_accessors" | Some x -> x in
 object (self)
  inherit ['a,'b] wire ~name ?parent system
  method get_alone = get_alone ()
  method set_alone = set_alone
 end

(** A cable is a wire containing and managing some homogeneous wires. *)
class ['a,'b] cable
 ?name
 ?(tracing = tracing_enable) (* on stderr *)
 ?parent
 (system:system) =
 let name = match name with None -> fresh_wire_name "cable" | Some x -> x in
 let given_system = system in
 object (cable)
  inherit [(int * 'a), (int * 'b) list] wire ~name ?parent system

  val wire_list : ('a,'b) wire container =
   new container ~list_name:"wires" ~owner:name ~with_mutex:given_system#mutex_methods#with_mutex

  method wire_list = wire_list

  method get_alone       = List.map (fun c -> (c#id, c#get_alone)) wire_list#content
  method set_alone (i,v) =
   try
    let w = List.find (fun w->w#id = i) wire_list#content in
    w#set_alone v
   with Not_found as e ->
     begin
      cable#tracing#message "wire identifier not found in cable...";
      raise e
     end

  (* Note that this method do not recursively call the method #to_dot of its components.
     This limitation is due to the non-compositional syntax of dot and the consequence
     is that a system can be represented from the top to, at most, the second level of depth. *)
  method to_dot =
   Printf.sprintf "%d [shape=record, label=\"%s (%d) | {|{%s}|}\"];"
     cable#id
     cable#name
     cable#id
     (dot_record_of_port_names (List.map (fun w->w#name) cable#wire_list#content))

end (* cable *)

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

let extract ?caller = function
 | Some x -> x
 | None   -> (match caller with
              | None   -> failwith ("extract")
              | Some c -> failwith ("extract called by "^c)
              )

class ['a] option_obj ?name ?owner (x:'a option) =
 let name  = match name  with Some x -> x     | None -> "option_obj" in
 let owner = match owner with Some x -> x^"#" | None -> "" in
 let extract_notification = Printf.sprintf "%s%s#extract" owner name in
 object
  val mutable content = x
  method get = content
  method set x = content <- x

  method extract = match content with
    | Some x -> x
    | None   -> failwith extract_notification

  method map : 'b. ('a -> 'b) -> 'b option =
   fun f -> match content with
   | None   -> None
   | Some x -> Some (f x)

  method bind : 'b. ('a -> 'b option) -> 'b option =
   fun f -> match content with
   | None   -> None
   | Some x -> (f x)

 end

class ['b] in_port_connection ~(equality:('b->'b->bool)) (w: ('a,'b) wire) =
 let readonly_wire = (w : ('a,'b) wire :> 'b readonly_wire) in
 object
  method readonly_wire = readonly_wire
  val mutable previous_value = None

  method read_and_compare sensitiveness =
   let v' = readonly_wire#get_alone in
   let unchanged = (not sensitiveness) ||
     (match previous_value with
     | None   -> false
     | Some v -> equality v v'
     ) in
   let () = (previous_value <- Some v') in
   (v', unchanged)

 end

class ['b] in_port ?(sensitiveness=true) ?(equality:('b->'b->bool) option) ~(name:string) ?(wire: ('a,'b) wire option) () =
 let equality = match equality with None -> (=) | Some eq -> eq in
 let connection =
   let connection_opt = match wire with
    | None -> None
    | Some w -> Some (new in_port_connection ~equality w) in
   new option_obj ~name:"connection" ~owner:name connection_opt in
 object (self)
  method name = name

  val mutable sensitiveness = sensitiveness
  method sensitiveness = sensitiveness
  method set_sensitiveness x = sensitiveness <- x

  val mutable equality = equality
  method equality = equality
  method set_equality x = equality <- x

  method connection = connection
  method connect : 'a. ('a,'b) wire -> unit =
   fun w -> connection#set (Some (new in_port_connection ~equality w))

  method disconnect : unit = connection#set None

  method read_and_compare =
   self#connection#extract#read_and_compare sensitiveness

 end

class ['a] out_port_connection (w: ('a,'b) wire) =
 let writeonly_wire = (w : ('a,'b) wire :> 'a writeonly_wire) in
 object
  method writeonly_wire = writeonly_wire
 end

class ['a] out_port ~(name:string) ?(wire: ('a,'b) wire option) () =
 let connection =
   let connection_opt = match wire with None -> None | Some w -> Some (new out_port_connection w) in
   new option_obj ~name:"connection" ~owner:name connection_opt in
 object (self)
  method name = name

  method connection = connection
  method connect : 'b. ('a,'b) wire -> unit =
   fun w -> connection#set (Some (new out_port_connection w))

  method disconnect : unit = connection#set None

 end

let teach_ocamldep = ()

(** Create a current system. *)
let initialize () = match !current_system with
 | None -> ignore (new system ())
 | _ -> ()

(** Stabilize the current system. *)
let stabilize () =
 let system = (extract ~caller:"Chip.stabilize: current system undefined" !current_system) in
 system#stabilize

let get_current_system () =
 let system = (extract ~caller:"Chip.current_system: current system undefined" !current_system) in
 system

let get_or_initialize_current_system () = match !current_system with
 | None   -> new system ()
 | Some s -> s

(** Simplified constructor. *)
let wref
 ?name
 ?parent
 ?(system=(get_or_initialize_current_system ())) x =
 new wref ?name ?parent system x

let wcounter
 ?name
 ?parent
 ?(system=(get_or_initialize_current_system ())) () =
 new wcounter ?name ?parent system

let wlist
 ?name
 ?parent
 ?(system=(get_or_initialize_current_system ())) x =
 new wlist ?name ?parent system x

let wire_of_accessors
 ?name
 ?parent
 ?(system:system=(get_or_initialize_current_system ())) ~(get:unit -> 'b) ~(set:'a->unit) () =
 ((new wire_of_accessors ?name ?parent system get set) :> ('a,'b) wire)

(** Simplified constructor. *)
let cable
 ?name
 ?parent
 ?(system=(get_or_initialize_current_system ())) () =
 new cable ?name ?parent system

(** A wire in a cable do not belong to the component list of the system. *)
let wref_in_cable
 ?name
 ~(cable:('a,'a) cable)
 (x:'a) =
 let result = new wref ?name ~parent:cable#as_common cable#system x in
 let () = cable#wire_list#add result in
 result
