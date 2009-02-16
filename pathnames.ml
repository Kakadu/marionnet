(* This file is part of Marionnet, a virtual network laboratory
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

open PreludeExtra.Prelude;; (* We want synchronous terminal output *)

Log.printf "Setting up directory path names for Marionnet...\n";;

(** Return the value of the given configuration variable, if it's defined
    as a non-empty string; otherwise return the second argument: *)
let configuration_variable_or_ variable_name default_value =
  let fallback_value =
    default_value ^ "/" in
  try
    let variable_value =
      Initialization.configuration#string variable_name in
    if variable_value = "" then
      default_value ^ "/"
    else
      variable_value ^ "/"
  with Not_found ->
    fallback_value;;

let configuration_variable_or variable_name default_value =
  let result = configuration_variable_or_ variable_name default_value in 
  Log.printf "\n %s (%s) = %s\n\n" variable_name default_value result;
  result;;

let marionnet_home =
  configuration_variable_or
    "MARIONNET_PREFIX"
    (Meta.prefix ^ "/share/" ^ Meta.name);;
let marionnet_home_filesystems =
  configuration_variable_or
    "MARIONNET_FILESYSTEMS_PATH"
    (marionnet_home^"/filesystems/");;
let marionnet_home_kernels =
  configuration_variable_or
    "MARIONNET_KERNELS_PATH"
    (marionnet_home^"/kernels/");;
let marionnet_home_images =
  marionnet_home^"/images/";;
let marionnet_home_bin =
  marionnet_home^"/bin/";;

(** The prefix to prepend to VDE executables; this allows us to install
    patched versions in an easy way, before our changes are integrated
    into VDE's mainline... *)
let vde_prefix =
  try
    Initialization.configuration#string "MARIONNET_VDE_PREFIX"
  with _ ->
    "";;

Log.printf "the VDE prefix is %s\n" vde_prefix;;

(* Enter the right directory: *)
try
  Unix.chdir marionnet_home;
with _ ->
  failwith ("Could not enter the directory (" ^ marionnet_home ^ ")");;
