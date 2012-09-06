(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2012  Jean-Vincent Loddo

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

(* Useful tools to manage cow files. *)

(** Create a fresh filename, without making the file (empty cow files are not allowed) *)
val make_temporary_cow_file_name :
  states_directory:string -> unit -> string

(** Alias for make_temporary_cow_file_name: *)
val make_fresh_cow_file_name :
  states_directory:string -> unit -> string

(** Make a sparse copy of the given file, and return the (relative) name of the copy.
    Note that this also succeeds when the source file does not exist, as it's the case
    with 'fresh' states.
    If not given, a cow_file_name is created, then used as target file.
    Both the source_pathname and cow_file_name may be implicit or absolute. *)
val duplicate_cow_file_into_states_directory :
  source_pathname:string ->
  states_directory:string ->
  ?cow_file_name:string -> unit -> string

(** Note that the cow_file_name may be implicit or absolute: *)
val cow_file_exists :
  states_directory:string ->
  cow_file_name:string -> unit -> bool
