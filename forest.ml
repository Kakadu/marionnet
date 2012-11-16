(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007, 2008  Luca Saiu
   Copyright (C) 2007, 2010  Jean-Vincent Loddo
   Copyright (C) 2007, 2008, 2010  Université Paris 13

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

(** A purely functional data structure for tree forests.
    For all iterators, the order of visit is always depth-first, left-to-right. *)

(** This definition prevents equivalences (each forest as a
    unique representation). *)
type 'a t =
  | Empty
  | NonEmpty of 'a      (** first tree root     *)
             *  ('a t)  (** first tree subtrees *)
             *  ('a t)  (** other nodes         *)

type 'a tree = 'a * 'a t (** a tree is a root with the forest of its childs *)
type 'a leaf = 'a        (** a leaf is a tree without childs *)

let empty = Empty

(** Add a tree to a forest. *)
let add_tree ((x,childs):'a tree) t = NonEmpty (x,childs,t)

(** Add to a forest a tree which is a leaf. *)
let add_leaf (x:'a) t = NonEmpty (x,Empty,t)

(** Make a forest with a single tree. *)
let of_tree ((x,childs):'a tree) = NonEmpty (x,childs,Empty)

let to_tree = function
| NonEmpty(root, childs, Empty) -> (root, childs)
| _ -> invalid_arg "Forest.to_tree: the forest is not a singleton"

(** Make a forest with a single tree which is a leaf. *)
let of_leaf (x:'a leaf) = NonEmpty (x,Empty,Empty)

let tree_of_leaf (x:'a leaf) : 'a tree = (x, Empty)

(** Returns the list of the 'a elements belong the forest.
    The order is depth-first, left-to-right. *)
let rec to_list (forest:'a t) : 'a list  =
  match forest with
    Empty ->
      []
  | NonEmpty(root, subtrees, rest) ->
      root :: (List.append (to_list subtrees) (to_list rest))

(** Append the second forest at the end of the first one. *)
let rec concat forest1 forest2 =
  match forest1 with
    Empty ->
      forest2
  | NonEmpty(root1, subtrees1, rest1) ->
      NonEmpty(root1, subtrees1, concat rest1 forest2)

(** Map the function over the 'a elements of the forest. *)
let rec map f forest =
  match forest with
    Empty ->
      Empty
  | NonEmpty(root, subtrees, rest) ->
      let root = f root in
      let subtrees = map f subtrees in
      let rest = map f rest in
      NonEmpty(root, subtrees, rest)

(** Iterate calling f on all nodes. The order is depth-first, left-to-right.
    f has the node as its first parameter, and its "parent-tree-node-option" as
    its second parameter *)
let rec iter ?parent (f : 'a -> 'a option -> unit) (forest : 'a t) =
  match forest with
    Empty ->
      ()
  | NonEmpty(root, subtrees, rest) ->
      begin
        f root parent;
        iter ~parent:root f subtrees;
        iter ?parent f rest
      end

(** Bad nodes (which not verify the property p) are cut and orphans lifted up. *)
let rec filter p forest =
  match forest with
    Empty ->
      Empty
  | NonEmpty(root, subtrees, rest) ->
      let subtrees = filter p subtrees in
      let rest = filter p rest in
      if p root then
        NonEmpty(root, subtrees, rest)
      else
        concat subtrees rest

(** Return a list of all the nodes in the given forest satisfying the given predicate.
    The order is as usual depth-first, left-to-right. *)
let rec nodes_such_that predicate forest =
  match forest with
    Empty ->
      []
  | NonEmpty(root, subtrees, rest) ->
      let switch = predicate root in
      let subtrees = nodes_such_that predicate subtrees in
      let rest = nodes_such_that predicate rest in
      let xs = List.append subtrees rest in
      if switch then root::xs else xs

(** Return the node in the given forest satisfying the given predicate.
    The order is as usual depth-first, left-to-right. *)
let rec search predicate forest =
  match forest with
  | Empty -> None
  | NonEmpty(root, subtrees, rest) ->
      if predicate root then (Some root) else
      match (search predicate subtrees) with
      | None -> search predicate rest
      | x -> x
;;

let search_and_replace pred repl t =
  map (fun a -> if pred a then repl a else a) t
;;

(** Return the node in the given forest satisfying the given predicate.
    The order is as usual depth-first, left-to-right. Raises [Not_found]
    if the element is not found. *)
let find predicate forest =
  match search predicate forest with
  | None   -> raise Not_found
  | Some x -> x
;;

(** Return the parent of a node satisfying the given predicate.
    The order is as usual depth-first, left-to-right. *)
let parent_of_node_such_that predicate forest =
 let rec loop ?parent forest =
  match forest with
  | Empty -> None
  | NonEmpty (root, subtrees, rest) ->
      if predicate root then parent else
      match (loop ~parent:root subtrees) with
      | None -> loop ?parent rest
      | x -> x
 in loop forest
;;

let parent_of node forest =
  parent_of_node_such_that ((=) node) forest
;;

(** Return the first-level nodes (similar to 'find -maxdepth 1'). *)
let rec roots_of = function
  | Empty -> []
  | NonEmpty(root, _ , rest) -> root :: (roots_of rest)
;;

(** Return a list of all the children of the given node in the given
    forest, in some unspecified order. Note that the given node may
    appear in several positions in the forest. In this case the result
    is the catenation of childrens of these occurrences. *)
let children_nodes node forest =
  let rec children_nodes_of_existing_node node forest =
  match forest with
    Empty ->
      []
  | NonEmpty(root, subtrees, rest) ->
      if root = node then
        roots_of subtrees
      else
        List.append
          (children_nodes_of_existing_node node subtrees)
          (children_nodes_of_existing_node node rest)
  in
  match nodes_such_that (fun a_node -> a_node = node) forest with
  |[] -> failwith "children_nodes: node not existing"
  | _ -> children_nodes_of_existing_node node forest

(** Return the root of the single child of the given node.
    Fail if the node has a number of children different from one. *)
let child_node node forest =
  let singlet = children_nodes node forest in
  if List.length singlet <> 1 then
    failwith "child_node: the node has zero or more than one children"
  else
    List.hd singlet

(** Return a list of all the descendant nodes of the given node in the given
    forest. The order is depth-first, left-to-right. *)
let rec descendant_nodes node forest =
  match forest with
    Empty ->
      []
  | NonEmpty(root, subtrees, rest) ->
      if root = node then
        to_list subtrees
      else
        List.append
          (descendant_nodes node subtrees)
          (descendant_nodes node rest)

(** Grandchildrens *)
let grandchildren_nodes_with_repetitions node forest =
  let children_nodes_of_node = children_nodes node forest in
  List.flatten
    (List.map
       (fun node -> children_nodes node forest)
       children_nodes_of_node);;

let printable_string_of_forest
 ?(level=0)
 ?(string_of_node=(fun _ ->"<NODE>"))
 forest
 =
 let buffer = Buffer.create 100 in
 let print_string x = Buffer.add_string buffer x in
 let print_node x   = Buffer.add_string buffer (string_of_node x) in

 (* Support for indentation *)
 let indent = function level ->
   for i = 1 to level do print_string "  "; done;
   if level = 0 then print_string "* " else print_string "`-"
 in
 let rec loop ~level = function
  | Empty -> ()
  | NonEmpty(root, subtrees, rest) ->
      begin
	indent level;
	print_node root;
	print_string "\n";
	loop ~level:(level + 1) subtrees;
	loop ~level rest;
      end
 in
 loop ~level forest;
 Buffer.contents buffer
;;

(** A printer for forests: *)
let rec print_forest ?level ?string_of_node ~channel forest =
  let s = printable_string_of_forest ?level ?string_of_node forest in
  Printf.kfprintf flush channel "%s" s
;;

(** Add the given tree to the given forest, as a new child of every
    found node satisfying the given predicate. The new forest is
    returned *)
let rec add_tree_to_forest_for_each predicate tree_root tree_subtrees forest =
  match forest with
    Empty ->
      Empty
  | NonEmpty(root, subtrees, rest) ->
      if predicate root then
        NonEmpty(root,
                 concat
                   (add_tree_to_forest_for_each predicate tree_root tree_subtrees subtrees)
                   (NonEmpty(tree_root, tree_subtrees, Empty)),
                 rest)
      else
        NonEmpty(root,
                 add_tree_to_forest_for_each
                   predicate tree_root tree_subtrees subtrees,
                 add_tree_to_forest_for_each
                   predicate tree_root tree_subtrees rest)

(** Add the given tree to the given forest, as a new child of the only
    node satisfying the given predicate, or at toplevel if no node satisfies
    it. If more than a node in the forest satisfies the predicate an exception
    is raised. *)
let add_tree_to_forest predicate tree_root tree_subtrees forest =
  let nodes = to_list forest in
  let satisfying_nodes = List.filter predicate nodes in
  let satisfying_nodes_length = List.length satisfying_nodes in
  if satisfying_nodes_length = 0 then
    concat forest (NonEmpty(tree_root, tree_subtrees, Empty))
  else if satisfying_nodes_length = 1 then
    add_tree_to_forest_for_each predicate tree_root tree_subtrees forest
  else
    failwith
      (Printf.sprintf
         "add_tree_to_forest predicate: more than one node (in fact %i) satisfies the predicate"
         satisfying_nodes_length)


(* --- Jean --- facilities using forests to encode trees: *)

(** Has the forest the form of a tree (i.e. a forest of length 1)? *)
let is_tree = function
| NonEmpty (root,childs,Empty) -> true
| _ -> false


(** Has the forest the form of a leaf? *)
let is_leaf = function
| NonEmpty (root,Empty,Empty) -> true
| _ -> false


(** A forest may be viewed as a list of trees. *)
let rec to_treelist (forest:'a t) : ('a tree list) =
match forest with
| Empty -> []
| NonEmpty (root,childs,rest) -> (root,childs)::(to_treelist rest)


(** A list of forests may be viewed as a single big forest.
    The forests in the list are simply catenated. *)
let rec of_forestlist (l:'a t list) = match l with
| []                             -> Empty
| Empty::l'                      -> of_forestlist l'
| (NonEmpty (x,childs,rest))::l' -> NonEmpty (x,childs, (concat rest (of_forestlist l')))
;;

(** A list of trees may be recompacted into a single forest. This function
    is similar to the [of_forestlist] but prevents the call to [concat] and
    also checks if all elements are really trees. An exception [Failure "of_nodelist"]
    is raised when a non tree element is encountered (use [of_forestlist] if you want
    flexibility). *)
(*let rec of_treelist (l:'a t list) = match l with
| []                              -> Empty
| (NonEmpty (x,childs,Empty))::l' -> NonEmpty (x,childs, (of_treelist l'))
| _ -> failwith "of_nodelist" (* A run-time type checking *)*)

let rec of_treelist (l:'a tree list) = match l with
| []             -> Empty
| (x,childs)::l' -> NonEmpty (x,childs, (of_treelist l'))

(** Convert a list of unstructured elements into a forest of leafs. *)
let of_list (l:'a list) = of_forestlist (List.map of_leaf l)

