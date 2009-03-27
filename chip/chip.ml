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

let fresh_name_generator () = let x = ref 0 in
 fun prefix -> (incr x); Printf.sprintf "%s%i" prefix !x
 ;;

let fresh_chip_name   = fresh_name_generator () ;;
let fresh_wire_name   = fresh_name_generator () ;;
let fresh_system_name = fresh_name_generator () ;;

let extract ?caller = function
 | Some x -> x
 | None   -> (match caller with None -> failwith ("extract") | Some c -> failwith ("extract called by "^c))
;;

(* Extract the value of the wire connected to a port comparing it with its previous value. *)
let extract_and_compare (caller:string) equality (* :'a -> 'a -> bool *) (port:((< get : 'a; .. > as 'w) * 'a option) option) : 'w * 'a * bool =
 let (w, vo) = extract ~caller port in
 let v' = w#get in
 let unchanged = match vo with
  | None   -> false
  | Some v -> (Obj.magic equality) v v'
 in (w, v', unchanged)
;;


let here () = (Printf.eprintf "Here\n"); flush stderr;;

type performed = Performed of bool ;;
let max_number_of_iterations = 1024 ;;
let current_system = ref None ;;

(** A chip has a set of input ports, a set of output ports and an internal state.
    Each port is a reference. The method 'propagate', set the outputs and the internal
    state depending on state and inputs. *)
class

 (** Common features for chips, wires and systems. *)
 common ~(name:string) =
  object (self)
   method name = name
   method trace tracing_method_name =
    begin
    (Printf.eprintf "In %s#%s\n" self#name tracing_method_name);
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

 (** A chip is something with a stabilization function and with a reference to its container (system).
     The chip performs its system subscription (as chip) at the creation. *)
 virtual chip ?(name = fresh_chip_name "chip") (system:system) =
  object (self) inherit component ~name system

  method virtual stabilize : performed

  (* Just for debugging or drawing purposes *)
  val virtual in_port_names  : string list
  val virtual out_port_names : string list
  method in_port_names  =  in_port_names
  method out_port_names = out_port_names
  method port_names = List.append in_port_names out_port_names

  initializer
   system#add_chip (self :> chip)
 end

and

 (** A system is a container of chips and wires. *)
 system ?(name = fresh_system_name "system") ?(max_number_of_iterations = max_number_of_iterations) () =
  object (self) inherit common ~name

  val mutable chip_list : chip list = []
  val mutable component_list : component list = []

  method add_chip chip = chip_list <- chip :: chip_list
  method add_component (c:component) = component_list <- c :: component_list

  (** The system stabilize itself calling the stabilization of its chips.
      The result indicated if the stabilization has really changed the state of some components. *)
  method stabilize : performed =
  let rec loop acc =
    if (acc >= max_number_of_iterations)
     then (failwith (Printf.sprintf "system#stabilize: stabilization failed after %d iterations" max_number_of_iterations))
     else begin
      let performed_list = List.map (fun (x:chip) -> x#stabilize) chip_list in
      if List.exists ((=)(Performed true)) performed_list then loop (acc+1) else acc
     end
  in
  let loops = loop 0 in (Performed (loops > 0))

 initializer
  current_system := Some (self :> system)
 end

and

 (** A wire is a sort of pipe that is constantly stabilized like a reference.
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
;;

(** A refences is a simple example of wire. *)
class ['a] wref ?(name = fresh_wire_name "wref") (system:system) (value:'a) =
  object (self) inherit ['a,'a] wire ~name system
  val mutable content = value
  method get = content
  method set_alone v = (content <- v)
 end
 ;;

(** Simplified constructor. *)
let wref ?(system=(extract ~caller:"Chip.wref: current system undefined" !current_system)) x = new wref system x;;

type ('b,'a) in_port  = (('b,'a) wire * 'a) option ;;
type ('a,'b) out_port = ('a,'b) wire option ;;

let in_port_of_wire_option = function None -> None | Some w -> Some (w,None) ;;

let teach_ocamldep = () ;;

let initialize () = match !current_system with
 | None -> ignore (new system ())
 | _ -> ()
;;

let stabilize () =
 let system = (extract ~caller:"Chip.stabilize: current system undefined" !current_system) in
 system#stabilize
;;

let get_current_system () =
 let system = (extract ~caller:"Chip.current_system: current system undefined" !current_system) in
 system
;;
