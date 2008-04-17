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

open PreludeExtra.Prelude;; (* We want synchronous terminal output *)

(** Parse the command line looking for the option "--exam". It's the only command-line
    argument we support as of now. *)

(** Command line arguments, without the initial executable name *)
let command_line_arguments =
  List.tl (Array.to_list Sys.argv);;
(*   let list_of_array a = *)
(*     let result = ref [] in (\* quite ugly *\) *)
(*     for i = (Array.length a) - 1 downto 0 do *)
(*       result := a.(i) :: !result; *)
(*     done; *)
(*     !result in *)
(*   List.tl (list_of_array Sys.argv);; *)

(** Are we in exam mode? *)
let are_we_in_exam_mode =
  List.exists
    (fun argument ->
      if argument = "--exam" then begin
        print_string ("WE ARE IN EXAM MODE.\n");
        true
      end
      else begin
        print_string ("Warning: I don't understand the command line argument \""^argument^"\"\n");
        false
      end)
    command_line_arguments;;

let window_title =
  if are_we_in_exam_mode then
    "Marionnet (EXAM)"
  else
    "Marionnet";;
