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

class

 (* Wires perform their operations (get,set) preventing from effects of other threads.
     In a single-thread application this support is unnecessary. *)
 mutex_methods ~(name:string) =
  object (self)
  val mutex     = Recursive_mutex.create ()
  method lock   = Recursive_mutex.lock   mutex
  method unlock = Recursive_mutex.unlock mutex
  method with_mutex : 'a. ( unit -> 'a) -> 'a =
    Recursive_mutex.with_mutex ~prefix:(name^": ") mutex
  end (* object *)

and

 tracing_methods ~(name:string) ~(tracing:bool) =
  object (self)

  val mutable tracing = tracing
  method tracing = tracing
  method set_tracing x = tracing <- x

  val mutable tracing_level = 0
  method tracing_level = tracing_level
  method tracing_tab prompt =
   let n = self#tracing_level in
   (String.make n ' ')^(String.make (n+1) prompt)^" "

  method tracing_lparen msg =
   if tracing = false then () else
    begin
     self#tracing_message_with_prompt '+' msg;
     tracing_level <- tracing_level + 1 ;
    end

  method tracing_rparen msg =
   if tracing = false then () else
    begin
     tracing_level <- tracing_level - 1 ;
     self#tracing_message_with_prompt '-' msg;
    end

  method tracing_message_with_prompt prompt msg =
   if tracing = false then () else
    begin
     Printf.eprintf "%s%s: %s\n" (self#tracing_tab prompt) name msg;
     flush stderr
    end

  method tracing_message msg =
   if tracing = false then () else self#tracing_message_with_prompt '*' msg

  end (* object *)

and

 virtual tracing_methods_for_components ~(name:string) system =
  object (self)
   inherit tracing_methods ~name ~tracing:system#tracing as super
   method tracing = match tracing_policy with
    | Top_tracing_policy -> tracing || system#tracing
    | Bot_tracing_policy -> tracing
   method tracing_level = system#tracing_level + tracing_level
   method private format_message  msg = (Printf.sprintf "%s (%d): %s" name self#id msg)
   method tracing_lparen  msg = system#tracing_lparen  (self#format_message msg)
   method tracing_rparen  msg = system#tracing_rparen  (self#format_message msg)
   method tracing_message msg = system#tracing_message (self#format_message msg)
   method virtual id : int
  end

and

 (** Common features for chips, wires and systems. *)
 common ~(name:string) =
  object (self)
   val id = fresh_id ()
   method id = id
   method name = name
 end

and

 (** Common features for chips and wires. This class just performs the system subscription. *)
 component ~(name:string) system =
  object (self)
  inherit common ~name
  inherit tracing_methods_for_components ~name system
  val mutable system = system
  method system = system
  method destroy =
    self#tracing_message "destroying as component";
    system#remove_component (self :> component)

  initializer
   system#add_component (self :> component)
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
  inherit chip ~name system

  method virtual stabilize : performed

  method traced_stabilize =
   if self#tracing = false then self#stabilize else
    begin
      self#tracing_lparen "stabilization...";
      let result = self#stabilize in
      let msg = match result with
       | Performed false -> "chip already stabilized."
       | Performed true  -> "stabilized."
      in
      self#tracing_rparen msg;
      result
    end

  method destroy =
   self#tracing_message "destroying as reactive";
   system#remove_reactive (self :> reactive)

  initializer
   system#add_reactive (self :> reactive)
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
  ?(name = fresh_system_name "system")
  ?(max_number_of_iterations = max_number_of_iterations)
  ?(tracing = tracing_enable) (* on stderr *)
  ?(set_default_system=true)
  ()
  = object (self)
  inherit common ~name
  inherit tracing_methods ~name ~tracing
  inherit mutex_methods ~name

  val mutable reactive_list : reactive list = []
  val mutable component_list : component list = []

  method add_reactive r =
   r#tracing_lparen "system adding me as reactive component...";
   let action () = begin
     reactive_list <- r :: reactive_list;
     r#tracing_rparen "added.";
    end in
   self#with_mutex action

  method add_component (c:component) =
   c#tracing_lparen "system adding me as component...";
   let action () = begin
     component_list <- c :: component_list;
     c#tracing_rparen "added.";
    end in
   self#with_mutex action

  method remove_component (c:component) =
   c#tracing_lparen "system removing me from components...";
   let action () = begin
     component_list <- List.filter (fun x -> x#id != c#id) component_list;
     c#tracing_rparen "removed.";
    end in
   self#with_mutex action

  method remove_reactive (r:reactive) =
   r#tracing_lparen "system removing me from reactive components...";
   let action () = begin
    reactive_list  <- List.filter (fun x -> x!=r) reactive_list;
    component_list <- List.filter (fun x -> x#id != r#id) component_list;
    r#tracing_rparen "removed.";
   end in
   self#with_mutex action

  method relay_list : reactive list = List.filter (fun c->c#kind=`Relay) reactive_list
  method sink_list  : reactive list = List.filter (fun c->c#kind=`Sink)  reactive_list

  (* For debugging: *)
  method show_component_list () =
   begin
    Printf.eprintf "System: %s\n" name;
    List.iter (fun c -> Printf.eprintf "\t* %s (%d)\n" c#name c#id) component_list;
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
  self#tracing_lparen "system stabilization...";
  self#tracing_lparen "relays' stabilization...";
  let loops = loop 0 in
  self#tracing_rparen "relays' stabilized.";
  self#tracing_lparen "sinks' stabilization...";
  let sink_results = List.map (fun (x:reactive) -> x#traced_stabilize) self#sink_list in
  let result = (loops > 0) || (List.exists ((=)(Performed true)) sink_results) in
  self#tracing_rparen "sinks' stabilized.";
  self#tracing_rparen "system stabilized.";
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
   self#tracing_message "reading wire (get)";
   let action () = self#get_alone in
   self#system#with_mutex action

  (** This would be a final method. *)
  method set (x:'a) : unit =
   self#tracing_lparen "setting wire...";
   let action () =
     begin
     (self#set_alone x);
     ignore (self#system#stabilize);
     self#tracing_rparen "wire set.";
     end
   in self#system#with_mutex action

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

(** A cable is a system of homogeneous wires. *)
class ['a,'b] cable
 ?(name = fresh_wire_name "cable")
 ?(tracing = tracing_enable) (* on stderr *)
 (system:system) =
 object (cable)
  inherit tracing_methods ~name ~tracing
  inherit [(int * 'a), (int * 'b) list] wire ~name system

  val mutable wire_list : ('a,'b) wire list = []

  method get_alone       = List.map (fun c -> (c#id, c#get_alone)) wire_list
  method set_alone (i,v) =
   try
    let w = List.find (fun w->w#id = i) wire_list in
    w#set_alone v
   with Not_found as e ->
     begin
      cable#tracing_message "wire identifier not found in cable...";
      raise e
     end

  method add_wire (w:('a,'b) wire) =
   w#tracing_lparen "cable adding me as wire...";
   let action () = begin
     wire_list <- w :: wire_list;
     w#tracing_rparen "added.";
    end in
   cable#system#with_mutex action

  method remove_wire (w:('a,'b) wire) =
   w#tracing_lparen "cable removing me from wires...";
   let action () = begin
     wire_list <- List.filter (fun x -> x!=w) wire_list;
     w#tracing_rparen "removed.";
    end in
   cable#system#with_mutex action;

  method create_wref ?(name= fresh_wire_name "cable_wire") (v:'a) =
   let result =
     object (wire)
      inherit ['a] wref ~name cable#system v as w

      (* I redefine get and set in order to access to cable. *)
      method get =
       wire#tracing_message "reading wire in cable (get)";
       List.assoc w#id cable#get

      method set x =
       wire#tracing_message "setting wire in cable";
       cable#set (w#id, x)

      (* Destroy means remove wire from both cable and system lists. *)
      method destroy =
       wire#tracing_lparen "destroying wire in cable...";
       cable#remove_wire wire;
       w#destroy;
       wire#tracing_rparen "wire destroyed."

     end (* object *)
   in
   (cable#add_wire result);
   result

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

(** Simplified constructor. *)
let wire_of_accessors
 ?(name = fresh_wire_name "wire_of_accessors")
 ?(system=(get_or_initialize_current_system ())) ~(get:unit -> 'a) ~(set:'a->unit) () =
 ((new wire_of_accessors ~name system get set) :> ('a,'a) wire)

(** Simplified constructor. *)
let cable
 ?(name = fresh_wire_name "wref")
 ?(system=(get_or_initialize_current_system ())) () =
 new cable ~name system
(*  ((new cable ~name system) :>  (int * 'a, 'a list) wire) *)
