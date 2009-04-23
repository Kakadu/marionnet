(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007  Luca Saiu

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

(* open PreludeExtra.Prelude;; *) (* We want synchronous terminal output *)

(** A general-purpose polymorphic graph data structure, written in imperative
    style.
    Nodes are identified by automatically-assigned unique ids, which are used
    also to recognize endpoints, for each edge.
    The implementation should be reasonably efficient, but remove_node can be
    optimized. remove_edge is difficult to make better because of a (gratuitous,
    in my opinion) restriction in Hashtbl: it's not allowed to remove a pair
    <key, value>, but only to blindly remove the "current" binding of key. 
    So I have to get all bindings, filter them out, remove all bindings from the
    table, and reinsert the surviving ones. *)

type id = int;;
let fresh_id = Counter.make_int_generator ()

type 'a graph =
    (* Nodes: *)
    (id, 'a) Hashtbl.t *
    (* Edges, as a set of ordered node pairs: *)
    ((id, id) Hashtbl.t) * (* for the edge "x |-> y" x is the key, y is the value *)
    (* Reversed edges, as a set of ordered node pairs: *)
    ((id, id) Hashtbl.t);;

let make_empty_graph () : 'a graph =
  Hashtbl.create 257, Hashtbl.create 257, Hashtbl.create 257;;

let add_node node (graph : 'a graph) =
  let nodes, _, _ = graph in
  let id = fresh_id () in
  Hashtbl.add nodes id node;
  id;;

let get_node_ids (graph : 'a graph) : id list =
  let nodes, _, _ = graph in
  Hashtbl.fold
    (fun id _ list -> id :: list)
    nodes
    [];;

let get_node (id : id) (graph : 'a graph) : 'a =
  let nodes, _, _ = graph in
  Hashtbl.find nodes id;;

let get_forward_star id (graph : 'a graph) =
  let _, edges, _ = graph in
  Hashtbl.find_all edges id;;

let get_backward_star id (graph : 'a graph) =
  let _, _, backward_edges = graph in
  Hashtbl.find_all backward_edges id;;

(** Print an understandable representation of a graph, given a function printing
    a node: *)
let print_graph (print_node : 'a -> unit) (graph : 'a graph) =
  let ids : id list = List.sort compare (get_node_ids graph) in
  print_string "Nodes:\n";
  List.iter
    (fun id ->
      let node : 'a = get_node id graph in
      Printf.printf "%i. " id;
      print_node node;
      print_string "\n")
    ids;
  print_string "Edges:\n";
  List.iter
    (fun from_id ->
      let forward_star = List.sort compare (get_forward_star from_id graph) in
      if forward_star <> [] then begin
        List.iter
          (fun to_id -> Printf.printf "%i -> %i; " from_id to_id)
          forward_star;
        print_string "\n";
      end)
    ids;;

let has_edge from_id to_id (graph : 'a graph) =
  List.exists
    (fun a_to_id -> a_to_id = to_id)
    (get_forward_star from_id graph);;

let add_edge source_id destination_id (graph : 'a graph) =
  if not (has_edge source_id destination_id graph) then begin
    let _, edges, reversed_edges = graph in
    Hashtbl.add edges source_id destination_id;
    Hashtbl.add reversed_edges destination_id source_id;
  end;;

let clear (graph : 'a graph) =
  let nodes, forward_edges, backward_edges = graph in
  Hashtbl.clear nodes;
  Hashtbl.clear forward_edges;
  Hashtbl.clear backward_edges;;

let get_forward_edges (graph : 'a graph) =
  let _, edges, _ = graph in
  Hashtbl.fold
    (fun from_id to_id list -> (from_id, to_id) :: list)
    edges
    [];;

let get_backward_edges (graph : 'a graph) =
  let _, edges, _ = graph in
  Hashtbl.fold
    (fun from_id to_id list -> (to_id, from_id) :: list)
    edges
    [];;

(** Remove the edge (from_id |-> to_id), if it exists, otherwise do nothing.
    In any case *don't* remove any node. 
    Yes, this implementation sucks: see the comment at the beginning to
    understand why I had to do it this way. *)
let remove_edge from_id to_id (graph : 'a graph) =
  let _, forward_edges, backward_edges = graph in
  (* Get the current forward star of from_id and the current backward star of
     to_id: the edge we want to remove, if it exists, is in both: *)
  let forward_star = get_forward_star from_id graph in 
  let backward_star = get_backward_star to_id graph in
  (* Temporarily remove all the edges from from_id and all the edges to to_id: *)
  List.iter
    (fun _ -> Hashtbl.remove forward_edges from_id)
    forward_star;
  List.iter
    (fun _ -> Hashtbl.remove backward_edges to_id)
    backward_star;
  (* Re-insert all the edges, except the one we want to remove: *)
  List.iter
    (fun a_to_id -> if a_to_id <> to_id then Hashtbl.add forward_edges from_id a_to_id)
    forward_star;
  List.iter
    (fun a_from_id -> if a_from_id <> from_id then Hashtbl.add backward_edges to_id a_from_id)
    backward_star;;

(** Remove the given node, if it exists; otherwise do nothing *)
let remove_node id (graph : 'a graph) =
  let nodes, _, _ = graph in
(*   print_string "================ Before:\n"; print_graph print_string graph; *)
  (* First remove all edges involving id... *)
  let forward_star = get_forward_star id graph in
  List.iter (fun to_node -> remove_edge id to_node graph) forward_star;
  let backward_star = get_backward_star id graph in
  List.iter (fun from_node -> remove_edge from_node id graph) backward_star;
(*   print_string "================ After edges removal:\n"; print_graph print_string graph; *)
  (* ...then remove the node: *)
  Hashtbl.remove nodes id;
(*   print_string "================ After node removal:\n"; print_graph print_string graph *)
  ;;


(** Given a graph and its root, return a list of the reachable node ids in some unspecified
    topological sort, where if a |-> b is an edge b precedes a in the result. Forward edges
    are taken as elements of a 'source depends on destination' relation.
    This also works when the graph is made of several distinct connected
    components: every node is returned (exactly once).
    If the graph is cyclic then the result is undefined. *)
let topological_sort (graph : 'a graph) =
  let nodes = get_node_ids graph in
  let touched_nodes = Hashtbl.create (List.length nodes) in
  let result = ref [] in
  let rec depth_first_visit root =
    if not (Hashtbl.mem touched_nodes root) then begin
      Hashtbl.add touched_nodes root ();
      List.iter depth_first_visit (get_forward_star root graph);
      result := root :: !result;
    end in
  List.iter
    depth_first_visit
    (List.filter
       (fun node -> get_backward_star node graph = [])
       nodes);
  List.rev !result;;


(*
(** Example *)
let rec print_list print_node g xs =
  match xs with
    [] -> print_string "\n";
  | id :: ys -> (print_node (get_node id g); print_string " "; print_list print_node g ys);;

let _ =
  let g = make_empty_graph () in
(*   let a = add_node "a" g in *)
(*   let b = add_node "b" g in *)
(*   let c = add_node "c" g in *)
(*   let _ = add_edge a b g in *)
(*   let _ = add_edge a c g in *)
(*   let _ = add_edge b c g in *)
(*   let _ = add_edge b b g in *)
(*   let _ = remove_node a g in *)
(*   let n1 = add_node "a" g in *)
(*   let n2 = add_node "b" g in *)
(*   let n3 = add_node "c" g in *)
(*   let n4 = add_node "d" g in *)
(*   let n5 = add_node "e" g in *)
(*   let n6 = add_node "f" g in *)
(*   let n7 = add_node "g" g in *)
(*   let n8 = add_node "h" g in *)
(*   let n9 = add_node "i" g in *)
  let n4 = add_node "d" g in
  let n2 = add_node "b" g in
  let n1 = add_node "a" g in
  let n8 = add_node "h" g in
  let n3 = add_node "c" g in
  let n6 = add_node "f" g in
  let n9 = add_node "i" g in
  let n5 = add_node "e" g in
  let n7 = add_node "g" g in
  let _ = add_edge n1 n2 g in
  let _ = add_edge n2 n3 g in
  let _ = add_edge n1 n4 g in
  let _ = add_edge n1 n5 g in
  let _ = add_edge n3 n5 g in
  let _ = add_edge n5 n6 g in
  let _ = add_edge n7 n4 g in
  let _ = add_edge n5 n8 g in
  let _ = add_edge n8 n7 g in

  print_graph print_string g;
  print_string "\nTopological sort: ";
  print_list print_string g (topological_sort g);
  print_string "\n";;
*)
