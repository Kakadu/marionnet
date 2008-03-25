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
