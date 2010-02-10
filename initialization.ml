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


open Unix;;
open Sys;;
open Gettext;;

(** First of all print the version number. This is useful for problem reports: *)
Printf.printf "=======================================================\n" ;;
Printf.printf " Welcome to %s version %s\n" Meta.name Meta.version;; 
Printf.printf " Please report bugs to marionnet-dev@marionnet.org \n" ;;
Printf.printf "=======================================================\n\n" ;;

(* Seed the random number generator: *)
Random.self_init ();;

(** Read configuration files: *)
let configuration =
  new Configuration_files.configuration
    ~software_name:"marionnet"
    ~variables:["MARIONNET_SOCKET_NAME";
                "MARIONNET_BRIDGE";(* This is temporary: more than one bridge will be usable... *)
                "MARIONNET_KEYBOARD_LAYOUT";
                "MARIONNET_DEBUG";
                "MARIONNET_PDF_READER";
                "MARIONNET_POSTSCRIPT_READER";
                "MARIONNET_DVI_READER";
                "MARIONNET_HTML_READER";
                "MARIONNET_TEXT_EDITOR";
                "MARIONNET_ROUTER_PORT0_IPV4_DEFAULT";
                (* *Optional* configuration variables: *)
		"MARIONNET_TERMINAL";
                "MARIONNET_PREFIX";
                "MARIONNET_FILESYSTEMS_PATH";
                "MARIONNET_KERNELS_PATH";
                "MARIONNET_VDE_PREFIX";
                "MARIONNET_ROUTER_FILESYSTEM";
              ]
    ();;

(** Remember the cwd directory at startup time: *)
let cwd_at_startup_time =
  Unix.getcwd ();;

(* We can not use Log.printf here because Initialization is a top file 
   use by Log (and  others), do that creates circular dependencies *)
(*Printf.printf "Setting up directory path names for Marionnet...\n";; *)

(** Return the value of the given configuration variable, if it's defined as 
    a non-empty string; otherwise return the second argument, third argument
    gives the type of variable in order to add a slash if it is a pathname *)
let configuration_variable_or_ variable_name default_value var_type =
  let fallback_value = 
    if var_type = "type_path" then
      default_value ^ "/" 
    else
      default_value in
  try
    let variable_value =
      configuration#string variable_name in
    if var_type = "type_path" then
      (if variable_value = "" then
        default_value ^ "/"
      else
        variable_value ^ "/")
    else
      (if variable_value = "" then
        default_value
      else
        variable_value)
  with Not_found ->
    fallback_value;;

(** Wrapper of configuration_variable_or_ function, display values 
    of a given variable during its initialization *) 
let configuration_variable_or variable_name default_value var_type =
  let result = configuration_variable_or_ variable_name default_value var_type in
(*  Printf.printf "\n %s (%s) = %s\n" variable_name default_value result;*)
  result;;

(* Here we initialze some variables with user or default value *)
let marionnet_home =
  configuration_variable_or
    "MARIONNET_PREFIX" 
    (Meta.prefix ^ "/share/" ^ Meta.name) 
    "type_path";;
let marionnet_home_filesystems =
  configuration_variable_or
    "MARIONNET_FILESYSTEMS_PATH"
    (marionnet_home^"/filesystems/") 
    "type_path" ;;
let marionnet_home_kernels =
  configuration_variable_or
    "MARIONNET_KERNELS_PATH"
    (marionnet_home^"/kernels/") 
    "type_path";;
(* The prefix to prepend to VDE executables; this allows us to install
    patched versions in an easy way, before our changes are integrated
    into VDE's mainline... *)
let vde_prefix =
  configuration_variable_or
    "MARIONNET_VDE_PREFIX"
    "" 
    "type_other";;
(* What is terminal that Marionnet must use to lanch a virtual host *)
let marionnet_terminal =
  configuration_variable_or
    "MARIONNET_TERMINAL"
    "xterm,-T,-e"
    "type_other";;
let marionnet_home_images =
  marionnet_home^"/images/";;
let marionnet_home_bin =
  marionnet_home^"/bin/";;

(* Default for the factory-set configuration address for routers.
   The result is a couple (ip,nm) where ip is the 4-tuple IPv4 and nm is the CIDR netmask. *)
let router_port0_default_ipv4_config  =
 let str = configuration_variable_or
    "MARIONNET_ROUTER_PORT0_DEFAULT_IPV4_CONFIG"
    "192.168.1.254/24"
    "type_other"
 in Scanf.sscanf str "%i.%i.%i.%i/%i" (fun b1 b2 b3 b4 nm -> ((b1, b2, b3, b4), nm))
;;

(* Useful for setting spin buttons. => ip library ? *)
let router_port0_default_ipv4_config_float_converted =
 let f = float_of_int in
 let ((ip1,ip2,ip3,ip4),nm) = router_port0_default_ipv4_config in
 (((f ip1),(f ip2),(f ip3),(f ip4)),(f nm))
;;

(* Enter the right directory: *)
try
  Unix.chdir marionnet_home;
with _ ->
  failwith ("Could not enter the directory (" ^ marionnet_home ^ ")");;

(* inizialize gettext with marionnet as textdomain and the default path where to find .mo files *)
(* initialize_gettext "marionnet" "/usr/share/locale";; *)
