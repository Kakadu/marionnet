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


open Forest;;
open Row_item;;

type node = (string * row_item) list;;

type treeview_forest = node forest;;

type dump_type = int * treeview_forest;;

let print_node node =
  print_string "[ ";
  List.iter
    (fun (string, row_item) ->
      Printf.printf "%s: " string;
      match row_item with
      | String s ->
          Printf.printf "%s; " s
      | CheckBox b ->
          Printf.printf "%s; " (if b then "T" else "F")
      | Icon i ->
          Printf.printf "i:%s; " i)
    node;
  print_string "]";;

let print_treeview (next_identifier, forest) =
  Printf.printf "Next identifier: %i\n\n" next_identifier;
  print_forest forest print_node;;
