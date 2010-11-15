(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007, 2008  Luca Saiu
   Copyright (C) 2009, 2010  Jean-Vincent Loddo
   Copyright (C) 2009  Jonathan Roudiere
   Copyright (C) 2007, 2008, 2009, 2010  Université Paris 13

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


Log.printf ~v:0 ~banner:false
  "=======================================================
 Welcome to %s
 Version              : %s
 Source revision      : %s - %s
 Ocamlbricks revision : %s - %s

 Built in date %s on system:
 
%s
 
 For bug reporting, please get a launchpad account and
 either:
  - report bugs at https://bugs.launchpad.net/marionnet
 or do *all* the following:
  - add yourself to the marionnet-dev team
  - add yourself to the marionnet-dev mailing list
  - write to marionnet-dev@lists.launchpad.net
=======================================================\n"
  Meta.name
  Meta.version
  Meta.revision Meta.source_date
  Meta_ocamlbricks.revision Meta_ocamlbricks.source_date
  Meta.build_date
  (StringExtra.fmt ~tab:8 ~width:40 Meta.uname)
;;  

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
                (* *Optional* configuration variables: *)
		"MARIONNET_TERMINAL";
                "MARIONNET_PREFIX";
                "MARIONNET_FILESYSTEMS_PATH";
                "MARIONNET_KERNELS_PATH";
                "MARIONNET_VDE_PREFIX";
                "MARIONNET_ROUTER_FILESYSTEM";
                "MARIONNET_ROUTER_KERNEL";
                "MARIONNET_MACHINE_FILESYSTEM";
                "MARIONNET_MACHINE_KERNEL";
                "MARIONNET_ROUTER_PORT0_DEFAULT_IPV4_CONFIG";
              ]
    ();;

(** Remember the cwd directory at startup time: *)
let cwd_at_startup_time =
  Unix.getcwd ();;

(** Return the value of the given configuration variable, if it's defined as
    a non-empty string; otherwise return the default.
    The third argument is a continuation that may be useful, for instance,
    to add a slash if it is a pathname. *)
let configuration_variable_or ?k ?(default="") variable_name =
  let fallback e x = Log.printf ~force:true "Warning: %s not declared.\n" x in
  let result =
    Log.printf "Searching for variable %s:\n" variable_name;
    match Option.of_fallible_application ~fallback configuration#string variable_name
    with
    | None
    | Some "" ->
        Log.printf " - using default \"%s\"\n" default;
        default
    | Some x  ->
        Log.printf " - found value \"%s\"\n" x;
        x
   in
   (* Launch the continuation on the result: *)
   match k with None -> result | Some f -> (f result)
 ;;

(** Firstly read if the debug mode must be activated.
    In this way the variable parsing can be monitored. *)
module Debug_level = struct

  let of_bool = function
    | false -> 0
    | true  -> 1
  
  let default_level = of_bool (configuration#bool "MARIONNET_DEBUG")
  
  let current = ref default_level
  let set x = (current := x)
  let get () = !current

  let are_we_debugging () = ((get ())>0)
  let set_from_bool b = set (of_bool b)
  
  (** Interpret the current state as suffix to append to shell commands. *)
  let redirection () =
    if are_we_debugging () then "" else " >/dev/null 2>/dev/null "

end

(** Link the function used by Log with Debug_mode.get: *)
let () = Log.Tuning.Set.debug_level Debug_level.get;;
Log.printf
  "MARIONNET_DEBUG is %b (debug level %d)\n" 
  (Debug_level.are_we_debugging ()) (* is true iff you read the message *)
  (Debug_level.get ()) 
;;

(* Used as continuation (~k) calling configuration_variable_or: *)
let append_slash x = x ^ "/" ;;

(* What is terminal that Marionnet must use to lanch a virtual host *)
let marionnet_terminal =
  let default = "xterm,-T,-e" in
  configuration_variable_or ~default "MARIONNET_TERMINAL" ;;

let router_filesystem_default_epithet =
  let default = "default" in
  configuration_variable_or ~default "MARIONNET_ROUTER_FILESYSTEM"

let router_kernel_default_epithet =
  let default = "default" in
  configuration_variable_or ~default "MARIONNET_ROUTER_KERNEL"

let machine_filesystem_default_epithet =
  let default = "default" in
  configuration_variable_or ~default "MARIONNET_MACHINE_FILESYSTEM"

let machine_kernel_default_epithet =
  let default = "default" in
  configuration_variable_or ~default "MARIONNET_MACHINE_KERNEL"

(* TODO: make it more robust and logged *)
module Path = struct

 let marionnet_home =
   let default = (Meta.prefix ^ "/share/" ^ Meta.name) in
   configuration_variable_or ~k:append_slash ~default "MARIONNET_PREFIX"

 let filesystems =
   let default = (marionnet_home^"/filesystems/") in
   configuration_variable_or ~k:append_slash ~default "MARIONNET_FILESYSTEMS_PATH"

 let kernels =
   let default = (marionnet_home^"/kernels/") in
   configuration_variable_or ~k:append_slash ~default "MARIONNET_KERNELS_PATH"

 let images = marionnet_home^"/images/"
 let leds   = marionnet_home^"/images/leds/"
 let bin    = marionnet_home^"/bin/"

 (* The prefix to prepend to VDE executables; this allows us to install
    patched versions in an easy way, before our changes are integrated
    into VDE's mainline... *)
 let vde_prefix = configuration_variable_or "MARIONNET_VDE_PREFIX";;

 (* User installation: *)

 let user_home =
   try (Sys.getenv "HOME") with Not_found ->
   try ("/home/"^(Sys.getenv "USER")) with Not_found ->
   try ("/home/"^(Sys.getenv "LOGNAME")) with Not_found ->
   try (Sys.getenv "PWD") with Not_found ->
   "."

 let user_filesystems = user_home^"/.marionnet/filesystems"
 let user_kernels = user_home^"/.marionnet/kernels"

end;;

(* Default for the factory-set configuration address for routers.
   The result is a couple (ip,nm) where ip is the 4-tuple IPv4 and nm is the CIDR netmask. *)
let router_port0_default_ipv4_config  =
 let variable_name = "MARIONNET_ROUTER_PORT0_DEFAULT_IPV4_CONFIG" in
 let default = "192.168.1.254/24" in
 let value = configuration_variable_or ~default variable_name in
 let parse arg = Ipv4.config_of_string ~strict:true arg in
 try parse value
 with _ -> begin
   Log.printf ~force:true "Warning: ill-formed value for %s\n" variable_name;
   parse default
   end
;;

(* Enter the right directory: *)
try
  Unix.chdir Path.marionnet_home;
with _ ->
  failwith ("Could not enter the directory (" ^ Path.marionnet_home ^ ")");;
