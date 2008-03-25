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


open Forest;;
open Print_treeview;;

let forest_file_name = 
  let arguments = Sys.argv in
  assert (Array.length arguments = 2);
  Array.get arguments 1;;

let _ =
  let channel = open_in forest_file_name in
  let stuff : dump_type = Marshal.from_channel channel in
  close_in channel;
  print_treeview stuff;;
