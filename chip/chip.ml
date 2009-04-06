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

let extract ?caller = function
 | Some x -> x
 | None   -> (match caller with None -> failwith ("extract") | Some c -> failwith ("extract called by "^c))

(* Extract the value of the wire connected to a port comparing it with its previous value. *)
let extract_and_compare (caller:string) equality (* :'a -> 'a -> bool *) (port:((< get : 'a; .. > as 'w) * 'a option) option) : 'w * 'a * bool =
 let (w, vo) = extract ~caller port in
 let v' = w#get in
 let unchanged = match vo with
  | None   -> false
  | Some v -> (Obj.magic equality) v v'
 in (w, v', unchanged)

type performed = Performed of bool 
let max_number_of_iterations = 1024
let current_system = ref None

class

 (** Common features for chips, wires and systems. *)
 common ~(name:string) =
  object (self)
   val id = fresh_id ()
   method id = id
   method name = name
   method trace tracing_method_name =
    begin
    (Printf.eprintf "In %s#%s (id=%d)\n" self#name tracing_method_name self#id);
    (flush stderr)
    end
 end

and

 (** Common features for chips and wires. This class just performs the system subscription. *)
 component ~(name:string) system =
  object (self) inherit common ~name
  val mutable system = system
  method system = system
  initializer
   system#add_component (self :> component)
 end

and

(** A chip is a box with some input ports, some output ports and possibly an internal state.
    Each port is a reference that can be dynamically linked to a wire. *)
 virtual chip ?(name = fresh_chip_name "chip") (system:system) =
  object (self) inherit component ~name system

  (* For chip classification, debugging and drawing purposes *)
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
  object (self) inherit chip ~name system
  method virtual stabilize : performed
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
 system ?(name = fresh_system_name "system") ?(max_number_of_iterations = max_number_of_iterations) () =
  object (self) inherit common ~name

  val mutable reactive_list : reactive list = []
  val mutable component_list : component list = []

  method add_reactive r = reactive_list <- r :: reactive_list
  method add_component (c:component) = component_list <- c :: component_list

  method remove_component (c:component) = begin
    component_list <- List.filter (fun x -> x#id != c#id) component_list;
   end

  method remove_reactive (r:reactive) = begin
    reactive_list  <- List.filter (fun x -> x!=r) reactive_list;
    component_list <- List.filter (fun x -> x#id != r#id) component_list;
   end

  method relay_list : reactive list = List.filter (fun c->c#kind=`Relay) reactive_list
  method sink_list  : reactive list = List.filter (fun c->c#kind=`Sink)  reactive_list

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
      let performed_list = List.map (fun (x:reactive) -> x#stabilize) relay_list in
      if List.exists ((=)(Performed true)) performed_list then loop (acc+1) else acc
     end
  in
  let loops = loop 0 in
  let sink_results = List.map (fun (x:reactive) -> x#stabilize) self#sink_list in
  let result = (loops > 0) || (List.exists ((=)(Performed true)) sink_results) in
  (Performed result)

 initializer
  current_system := Some (self :> system)
 end

and

 (** A wire is a sort of pipe that is constantly stabilized? The common example is a reference.
     You can get and set its value by methods #get and #set_alone (virtual).
     The #set method is the sequence of two actions: the individually set of the wires
     followed by the global set of its system. *)
 virtual ['a,'b] wire ?(name = fresh_wire_name "wire") (system:system) =
  object (self)
  inherit component ~name system

  method virtual get : 'b
  method virtual set_alone : 'a -> unit

  (** This must be a final method. *)
  method set (x:'a) : unit =
   begin
   (self#set_alone x);
   ignore (self#system#stabilize)
   end
  end


(** References are simple examples of wires. *)
class ['a] wref ?(name = fresh_wire_name "wref") (system:system) (value:'a) =
  object (self) inherit ['a,'a] wire ~name system
  val mutable content = value
  method get = content
  method set_alone v = (content <- v)
 end

type ('b,'a) in_port  = (('b,'a) wire * 'a) option
type ('a,'b) out_port = ('a,'b) wire option

let in_port_of_wire_option = function None -> None | Some w -> Some (w,None)

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
let wref ?(system=(get_or_initialize_current_system ())) x =
 new wref system x
