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

(* Create a fresh filename, without making the file
   (empty cow files are not allowed) *)
let rec make_temporary_cow_file_name ~states_directory () =
 let candidate =
   Printf.sprintf
     "%i-%i-%i.cow"
     (Random.int 65536)
     (Random.int 65536)
     (Random.int 65536)
 in
 let full_pathname = Filename.concat states_directory candidate in
 if Sys.file_exists full_pathname
   then make_temporary_cow_file_name ~states_directory ()
   else candidate
;;

(* Alias for make_temporary_cow_file_name: *)
let make_fresh_cow_file_name = make_temporary_cow_file_name ;;

(* Make a sparse copy of the given file, and return the (relative) name of the copy.
   Note that this also succeeds when the source file does not exist, as it's the case
   with 'fresh' states.
   If not given, a cow_file_name is created, then used as target file.
   Both the source_pathname and cow_file_name may be implicit or absolute. *)
let duplicate_cow_file_into_states_directory
  ~source_pathname   (* may be implicit or not *)
  ~states_directory
  ?cow_file_name     (* may be implicit or not *)
  ()
  =
  let name_of_the_copy =
    match cow_file_name with
    | None -> make_temporary_cow_file_name ~states_directory () (* implicit *)
    | Some name -> Filename.basename name (* now is certainly implicit *)
    in
  (* The source_pathname may be implicitely prefixed by states_directory: *)
  let full_source_pathname =
    match Filename.is_implicit source_pathname with
    | true  -> Filename.concat states_directory source_pathname
    | false -> source_pathname
  in
  let full_copy_pathname =
    Filename.concat states_directory name_of_the_copy
  in
  let command_line =
    (Printf.sprintf "cp --sparse=always %s %s"
	full_source_pathname
	full_copy_pathname)
  in
  (Log.printf "About to making a copy of a cow file...\n");
  (if Sys.file_exists full_copy_pathname
     then Log.printf "Strangely the cow file %s already exists!!!!\n" full_copy_pathname);
  try
    Log.system_or_fail command_line;
    name_of_the_copy
  with _ -> begin
    Simple_dialogs.error
      "This is a serious problem"
      "I cannot copy a (sparse) cow file!"
      ();
    Log.printf "I cannot duplicate a (sparse) cow file! To do: react in some reasonable way\n";
    name_of_the_copy;
  end
;;

(* Note that the cow_file_name may be implicit or absolute: *)
let cow_file_exists
  ~states_directory
  ~cow_file_name
  ()
  =
  let full_pathname =
    match Filename.is_implicit cow_file_name with
    | true  -> Filename.concat states_directory cow_file_name
    | false -> cow_file_name
  in
  Sys.file_exists full_pathname
;;
