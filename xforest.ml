(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007  Jean-Vincent Loddo
   Copyright (C) 2008  Luca Saiu
   Copyright (C) 2007, 2008  Université Paris 13

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


(** A forest concretization very close to XML. The type of nodes is
    [string * (string * string list)] where the first element is the
    tag and the second is the list of attributes, i.e. bindings in the
    form (key,value) where both key and value are strings. *)

type tag = string ;;

type attribute  = (string * string) ;;
type attributes = attribute list ;;

type node   = tag * attributes ;;

(** The forest concretization and its aliases. *)

type forest = node Forest.t ;;
type t      = forest ;;
type tree   = node * forest ;; (* the root and its children *)


(* *************************** *
        Class interpreter
 * *************************** *)

(** An Xforest interpreter is an object able to update itself
    reading an Xforest and, conversely, able to encode itself
    into an Xforest *)
class virtual interpreter () = object (self)

 (** Interpret a tree. The tag is ignored here. *)
 method from_tree ((tag,attrs):node) (children:forest) =
  begin
   (* Interpret attributes *)
   Log.printf "About to interpret *attributes* with tag \"%s\"\n" tag;
   List.iter self#eval_forest_attribute attrs;

   (* Interpret children *)
   Log.printf "About to interpret *children* with tag \"%s\"\n" tag;
   let l = Forest.to_treelist children in
   List.iter (self#eval_forest_child) l
  end

 (** The default interpretation of an attribute is ignore. *)
 method eval_forest_attribute : (attribute -> unit) =
   fun attr -> ()

 (** The default interpretation of a child is ignore. *)
 method eval_forest_child : (tree -> unit) =
  fun tree -> ()

 (** Encode self into an xtree. Typically this method calls
     recursively the same method of its children in order to construct
      its representation as forest. *)
 method virtual to_tree : tree

 (** May be redefined. Otherwise, by default, is simply a call to the method constructing
     the tree which is transformed in a forest (singleton). *)
 method to_forest : forest =
   Forest.of_tree self#to_tree

end;; (* class interpreter *)


(** print_forest specialization for xforest *)
let rec print_xforest ?level ~channel forest =
 let string_of_attr (name,value) = (name^"="^"\""^value^"\"") in
 let fold_strings = function
  | []   -> ""
  | [x]  -> x
  | x::r -> List.fold_left (fun a b -> a ^ " " ^ b) x r  in
 let string_of_attrs attrs = fold_strings (List.map string_of_attr attrs) in
 let string_of_node (tag,attrs) = ("<" ^ tag ^ "[" ^ (string_of_attrs attrs) ^ "]>") in
 Forest.print_forest ?level ~string_of_node ~channel forest
;;

(** Facilities for encoding/decoding fields in an object which are not strings. *)

let encode x = Marshal.to_string   x [Marshal.No_sharing] ;;
let decode y = Marshal.from_string y 0 ;;

(** EXAMPLE 1 *)

(* In a class, just add method like:

method to_tree =
 Forest.leaf ("cable",[("name","xxx");("label","xxx")]);;

method eval_forest_attribute : (string * string) -> unit = function
 | ("name",name) -> self#set_name name
 | ("kind",kind) -> self#set_kind kind
 | _ -> () *)

(** EXAMPLE 2 *)

(*method to_tree =
 let name = Forest.tree ("name",[]) (Forest.leaf ("xxx",[]))
 let kind = Forest.tree ("kind",[]) (Forest.leaf ("yyy",[]))
 in Forest.node ("cable",[]) (Forest.of_treelist [name; kind])

(** EXAMPLE 2 *)
method eval_forest_child (root,children) = match root with
 | ("name", attrs) ->
     let name = new name () in (* nel new senza argomenti l'essenza della backward-compatibility *)
     name#from_tree x;       (* chiamata ricorsiva al from_forest *)
     self#set_name = name;     (* oppure potrei accumulare... *)
 ...
 | _ -> ()
 *)


