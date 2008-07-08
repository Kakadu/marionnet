(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007  Jean-Vincent Loddo
   Copyright (C) 2007, 2008  Luca Saiu

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


(** What to show in the GUI for 'no variant'. This is safe to hardcode translated,
    as it doesn't break file format compatibility. *)
let no_variant_text =
  "aucune";;

(** What to show in the GUI for 'no variant'. This is safe to hardcode translated,
    as it's never saved in project files due to variant symlink resolution: *)
let suggested_text =
  "suggested";; (* "suggérée" (* I'd like to avoid accented letters in file names...*) *)

(** The name of the one and only router distribution, without the "router-" prefix: *)
let router_unprefixed_filesystem =
  try
    let variable_value = 
      Initialization.configuration#string "MARIONNET_ROUTER_FILESYSTEM" in
    if variable_value = "" then
      failwith "empty variable"
    else
      variable_value
  with _ ->
    "default";;
