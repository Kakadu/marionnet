(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007, 2008  Luca Saiu
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


type row_item =
  | String of string
  | CheckBox of bool
  | Icon of string;; (* Ugly, but this avoids that OCaml bitches about
                        parametric polymorphism and classes *)

let item_to_string item =
  match item with
    String s | Icon s -> s
  | CheckBox _ -> failwith "item_to_string: the parameter is a checkbox";;

let item_to_bool item =
  match item with
    CheckBox b -> b
  | String _ -> failwith "bool_to_string: the parameter is a string"
  | Icon _ -> failwith "bool_to_string: the parameter is an icon";;

(** Return a written representation of the given item, suitable for debugging,
    which also includes the constructor: *)
let pretty_string_of_item =
  function
    | String s -> Printf.sprintf "#string<%s>" s
    | CheckBox b -> Printf.sprintf "#checkbox<%s>" (if b then "true" else "false")
    | Icon s -> Printf.sprintf "#icon<%s>" s;;

(** Print a written representation of the given row, suitable for debugging;
    the printed string also includes the constructor: *)
let pretty_print_row row =
  let rec pretty_print_row_elements_content row =
    match row with
    | [] ->
        ()
    | (name, value) :: rest -> begin
        Printf.printf "%s=%s " name (pretty_string_of_item value);
        pretty_print_row_elements_content rest;
    end in
  Printf.printf "{ ";
  pretty_print_row_elements_content row;
  Printf.printf "}";;
