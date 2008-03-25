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

Printf.printf "Setting up directory path names for Marionnet...\n";;

(* Fail immediately if the environment directory is not defined: *)
try
  ignore (Sys.getenv "MARIONNET_HOME")
with Not_found ->
  failwith "The environment variable MARIONNET_HOME is not defined.";;

(* Ok, now we can safely set everything and go on: *)
let marionnet_home =
  Sys.getenv "MARIONNET_HOME";;
let marionnet_home_filesystems =
  marionnet_home^"/filesystems/";;
let marionnet_home_kernels =
  marionnet_home^"/kernels/";;
let marionnet_home_images =
  marionnet_home^"/images/";;
let marionnet_home_bin =
  marionnet_home^"/bin/";;

Printf.printf "Success.\n\n";;

(* To do: This doesn't really belong here... *)
Printf.printf "Checking that Marionnet is running as root...\n";;
if (Unix.getuid ()) != 0 then begin
  Printf.printf "\n*****************************************\n";
  Printf.printf "* Marionnet should be run as root.      * \n";
  Printf.printf "* This is needed to create tap devices. *\n";
  Printf.printf "*****************************************\n\n";
  failwith "The current UID is not zero";
end;;
Printf.printf "Success.\n\n";;
