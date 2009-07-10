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


(** Runtime support for the "chip" syntax extension. *)

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

let tracing_enable = true
let tracing_policy = Top_tracing_policy

class type tracing_methods =
  object
    method is_enable : bool
    method set : bool -> unit
    method level : int
    method lparen : string -> unit
    method message : string -> unit
    method rparen : string -> unit
    method tab : char -> string
  end

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
    Recursive_mutex.with_mutex ~prefix:(user^": ") mutex
  end (* mutex_methods *)

(** Support for managing the component garbage. *)
class virtual destroy_methods () =
  object (self)
  val mutable destroy_callbacks = []
  method add_destroy_callback f = (destroy_callbacks <- f::destroy_callbacks)
  method destroy =
   let () = self#tracing#lparen "destroying me..." in
   let () = List.iter (fun f -> f ()) destroy_callbacks in
   let () = self#tracing#rparen "destroyed." in ()
  method private virtual tracing : tracing_methods
  end (* destroy_methods *)

(** Support for tracing system's activities. *)
class tracing_methods_for_systems ~(name:string) ~(tracing:bool) =
  object (self)

  val mutable tracing = tracing
  method is_enable = tracing
  method set x = tracing <- x

  val mutable level = 0
  method level = level
  method tab prompt =
   let n = self#level in
   (String.make n ' ')^(String.make (n+1) prompt)^" "

  method lparen msg =
   if tracing = false then () else
    begin
     self#message_with_prompt '+' msg;
     level <- level + 1 ;
    end

  method rparen msg =
   if tracing = false then () else
    begin
     level <- level - 1 ;
     self#message_with_prompt '-' msg;
    end

  method private message_with_prompt prompt msg =
   if tracing = false then () else
    begin
     Printf.eprintf "%s%s: %s\n" (self#tab prompt) name msg;
     flush stderr
    end

  method message msg =
   if tracing = false then () else self#message_with_prompt '*' msg

  end (* object *)

(** Support for tracing component's activities. *)
class tracing_methods_for_components ~(id:int) ~(name:string) system =
  let format_message msg = (Printf.sprintf "%s (%d): %s" name id msg) in
  object (self)
   inherit tracing_methods_for_systems ~name ~tracing:system#tracing#is_enable as super
   method is_enable = match tracing_policy with
    | Top_tracing_policy -> tracing || system#tracing#is_enable
    | Bot_tracing_policy -> tracing
   method level = system#tracing#level + level
   method lparen  msg = system#tracing#lparen  (format_message msg)
   method rparen  msg = system#tracing#rparen  (format_message msg)
   method message msg = system#tracing#message (format_message msg)
  end

class

 (** Common features for chips, wires and systems. *)
 common ?(id=fresh_id ()) ~(name:string) () =
  object (self)
   method id = id
   method name = name
 end

and

 (** Common features for chips and wires.
     This class collect common methods and performs the system subscription. *)
 component ~(name:string) (system:system) =
  let id = fresh_id () in
  let tracing_methods = new tracing_methods_for_components ~id ~name system in
  object (self)
  inherit common ~id ~name ()
  inherit destroy_methods ()

  method system = system
  method tracing = tracing_methods

  initializer
   system#component_list#add (self :> component)
 end

and

(** A chip is a box with some input ports, some output ports and possibly an internal state.
    Each port is a reference that can be dynamically linked to a wire. *)
 virtual chip ?(name = fresh_chip_name "chip") (system:system) =
  object (self) inherit component ~name system

  (* For chip classification (reflection), debugging and drawing purposes *)
  val virtual in_port_names  : string list
  val virtual out_port_names : string list
  method in_port_names  =  in_port_names
  method out_port_names = out_port_names
  method port_names = List.append in_port_names out_port_names

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
 virtual source ?(name = fresh_source_name "source") (system:system) =
  object (self) inherit chip ~name system
  initializer
   (* Run-time type checking *)
   assert (self#kind = `Source)
 end

and

 (** A reactive chip is a chip with non zero in-degree. Such chips have a stabilization method
     that perform internal transitions and/or output ports setting with the current value
     of wires connected to input ports. *)
 virtual reactive ?(name = fresh_chip_name "reactive") (system:system) =
  object (self)
  inherit chip ~name system as self_as_chip

  method virtual stabilize : performed

  method traced_stabilize =
   if self#tracing#is_enable = false then self#stabilize else
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
 virtual relay ?(name = fresh_relay_name "relay") (system:system) =
  object (self) inherit reactive ~name system
  initializer
   (* Run-time type checking *)
   assert (self#kind = `Relay)
 end

and

 (* A sink is a reactive chip with 0 out-degree. *)
 virtual sink ?(name = fresh_sink_name "sink") (system:system) =
  object (self) inherit reactive ~name system
  initializer
   (* Run-time type checking *)
   assert (self#kind = `Sink)
 end

and

 (** A system is a container of chips and wires. *)
 system
  ?(father: system option)
  ?(name = fresh_system_name "system")
  ?(max_number_of_iterations = max_number_of_iterations)
  ?(tracing = tracing_enable) (* on stderr *)
  ?(set_default_system=true)
  ()
  =
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

 initializer
  if set_default_system then current_system := Some (self :> system) else ()
 end

and

 (** A wire is a sort of pipe that is constantly stabilized. The common example is a reference.
     You can get and set its value by methods #get and #set_alone (virtual).
     The #set method is the sequence of two actions: the individually set of the wires
     followed by the global set of its system. *)
 virtual ['a,'b] wire ?(name = fresh_wire_name "wire") (system:system) =
  object (self)
  inherit component ~name system

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

 end

(** References are simple examples of wires. *)
class ['a] wref
 ?(name = fresh_wire_name "wref") (system:system) (value:'a) =
 object (self)
  inherit ['a,'a] wire ~name system
  val mutable content = value
  method get_alone   = content
  method set_alone v = (content <- v)
 end

class ['a] wire_of_accessors
 ?(name = fresh_wire_name "wire_of_accessors") (system:system) get_alone set_alone =
 object (self)
  inherit ['a,'a] wire ~name system
  method get_alone = get_alone ()
  method set_alone = set_alone
 end

(** A cable is a wire containing and managing some homogeneous wires. *)
class ['a,'b] cable
 ?(name = fresh_wire_name "cable")
 ?(tracing = tracing_enable) (* on stderr *)
 (system:system) =
 let given_system = system in
 object (cable)
  inherit [(int * 'a), (int * 'b) list] wire ~name system

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

end (* cable *)

type ('b,'a) in_port  = (('b,'a) wire * 'a) option
type ('a,'b) out_port = ('a,'b) wire option

let in_port_of_wire_option = function None -> None | Some w -> Some (w,None)

let extract ?caller = function
 | Some x -> x
 | None   -> (match caller with
              | None   -> failwith ("extract")
              | Some c -> failwith ("extract called by "^c)
              )

(** Extract the value of the wire connected to a port comparing it with its previous value. *)
let extract_and_compare
  (caller:string)
   equality (* :'a -> 'a -> bool *)
  (port:((('b,'a) wire as 'w) * 'a option) option) : 'w * 'a * bool
 =
 let (w, vo) = extract ~caller port in
 let v' = w#get_alone in
 let unchanged = match vo with
  | None   -> false
  | Some v -> (Obj.magic equality) v v'
 in (w, v', unchanged)


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
 ?(name = fresh_wire_name "wref")
 ?(system=(get_or_initialize_current_system ())) x =
 new wref ~name system x

let wire_of_accessors
 ?(name = fresh_wire_name "wire_of_accessors")
 ?(system:system=(get_or_initialize_current_system ())) ~(get:unit -> 'b) ~(set:'a->unit) () =
 ((new wire_of_accessors ~name system get set) :> ('a,'b) wire)

(** Simplified constructor. *)
let cable
 ?(name = fresh_wire_name "wref")
 ?(system=(get_or_initialize_current_system ())) () =
 new cable ~name system
(*  ((new cable ~name system) :>  (int * 'a, 'a list) wire) *)

let wref_in_cable
 ?(name = fresh_wire_name "wref_in_cable")
 ~(cable:('a,'a) cable)
 (x:'a) =
 let result = new wref ~name cable#system x in
 let () = cable#wire_list#add result in
 result
