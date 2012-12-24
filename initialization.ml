(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007, 2008  Luca Saiu
   Copyright (C) 2009, 2010  Jean-Vincent Loddo
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

open Gettext

(* ***************************************** *
             Get basic infos
 * ***************************************** *)

let user_intelligible_version, released =
 match StrExtra.First.matchingp (Str.regexp "^[0-9]+[.][0-9]+[.][0-9]+$") Version.version with
 | true  ->
     (* it's a released version *)
     (Version.version, true)
 | false ->
     (* It's just the name of the branch *)
     let str = Printf.sprintf "%s revno %s" Version.version Meta.revision in
     (str, false)
;;

(* ***************************************** *
               Parse argv
 * ***************************************** *)

let () =
  Argv.register_usage_msg
    (Printf.sprintf "Usage: %s [OPTIONS] [FILE]\nOptions:" Sys.argv.(0))
;;

(* Registering options: *)
let option_v = Argv.register_unit_option "v" ~aliases:["-version"] ~doc:"print version and exit" () ;;
let option_debug = Argv.register_unit_option "-debug" ~doc:"activate messages for debugging" () ;;
let option_splash = Argv.register_unit_option "-splash" ~doc:"print splash message and exit" () ;;
let () = Argv.register_h_option_as_help () ;;

(* Registering arguments: *)
let optional_file_to_open =
  let error_msg =
    Printf.sprintf
      (f_ "%s: expected a readable regular file containing the marionnet project (.mar)")
      Sys.argv.(0)
  in
  Argv.register_filename_optional_argument ~r:() ~f:() ~error_msg () ;;

(* Argv.parse tuning: *)
let () = Argv.tuning
  ~no_error_location_parsing_arguments:()
  ~no_usage_on_error_parsing_arguments:()
  () ;;

(* Parse now (except if we are debugging with the toplevel): *)
let () =
  if not (List.mem (Array.get Sys.argv 0) ["/tmp/marionnet-toplevel"; "/tmp/marionnet-utop"])
   then Argv.parse ()
;;

(* Now we may inspect the references: *)

let () = if !option_v = Some () then begin
  Printf.kfprintf flush stdout "marionnet version %s\n" (user_intelligible_version);
  exit 0;
 end;;

(* else continue: *)

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

let () = if !option_splash = Some () then exit 0;;

(* else continue: *)

(* Seed the random number generator: *)
Random.self_init ();;

(** Remember the cwd directory at startup time: *)
let cwd_at_startup_time =
  Unix.getcwd ();;

(** Firstly read if the debug mode must be activated.
    In this way the variable parsing can be monitored. *)
module Debug_level = struct

  let of_bool = function
    | false -> 0
    | true  -> 1

  let default_level =
    of_bool ((!option_debug=Some()) || 
             (Configuration.extract_bool_variable_or ~default:false "MARIONNET_DEBUG"))

  let current = ref default_level
  let set x = (current := x)
  let get () = !current

  let are_we_debugging () = ((get ())>0)
  let set_from_bool b = set (of_bool b)

  (** Interpret the current state as suffix to append to shell commands. *)
  let redirection () =
    if are_we_debugging () then "" else " >/dev/null 2>/dev/null "

end

(** Link the function used by the marionnet's and the ocamlbricks's logs with Debug_mode.get: *)
let () =
  Log.Tuning.Set.debug_level Debug_level.get;
  Ocamlbricks_log.Tuning.Set.debug_level Debug_level.get
;;

Log.printf
  "MARIONNET_DEBUG is %b (debug level %d)\n"
  (Debug_level.are_we_debugging ()) (* is true iff you read the message *)
  (Debug_level.get ())
;;

(* Used as continuation (~k) calling `extract_string_variable_or': *)
let append_slash x = x ^ "/" ;;

(* What is terminal that Marionnet must use to lanch a virtual host *)
let marionnet_terminal =
  let default = "xterm,-T,-e" in
  Configuration.extract_string_variable_or ~default "MARIONNET_TERMINAL" ;;

let router_filesystem_default_epithet =
  let default = "default" in
  Configuration.extract_string_variable_or ~default "MARIONNET_ROUTER_FILESYSTEM"

let router_kernel_default_epithet =
  let default = "default" in
  Configuration.extract_string_variable_or ~default "MARIONNET_ROUTER_KERNEL"

let machine_filesystem_default_epithet =
  let default = "default" in
  Configuration.extract_string_variable_or ~default "MARIONNET_MACHINE_FILESYSTEM"

let machine_kernel_default_epithet =
  let default = "default" in
  Configuration.extract_string_variable_or ~default "MARIONNET_MACHINE_KERNEL"

(* Path related configuration variables.
   TODO: make it more robust and logged *)
module Path = struct

 let marionnet_home =
   let default = (Meta.prefix ^ "/share/" ^ Meta.name) in
   Configuration.extract_string_variable_or ~k:append_slash ~default "MARIONNET_PREFIX"

 let filesystems =
   let default = (marionnet_home^"/filesystems/") in
   Configuration.extract_string_variable_or ~k:append_slash ~default "MARIONNET_FILESYSTEMS_PATH"

 let kernels =
   let default = (marionnet_home^"/kernels/") in
   Configuration.extract_string_variable_or ~k:append_slash ~default "MARIONNET_KERNELS_PATH"

 let images = marionnet_home^"/images/"
 let leds   = marionnet_home^"/images/leds/"
 let bin    = marionnet_home^"/bin/"

 (* The prefix to prepend to VDE executables; this allows us to install
    patched versions in an easy way, before our changes are integrated
    into VDE's mainline... *)
 let vde_prefix = 
   Configuration.extract_string_variable_or ~default:"" "MARIONNET_VDE_PREFIX";;

 (* User installation: *)

 let user_home =
   try (Sys.getenv "HOME") with Not_found ->
   try ("/home/"^(Sys.getenv "USER")) with Not_found ->
   try ("/home/"^(Sys.getenv "LOGNAME")) with Not_found ->
   try (Sys.getenv "PWD") with Not_found ->
   "."

 let user_filesystems = user_home^"/.marionnet/filesystems"
 let user_kernels = user_home^"/.marionnet/kernels"

 let marionnet_tmpdir : string option =
   Configuration.get_string_variable "MARIONNET_TMPDIR"

end (* Path *)
;;

(* Warnings related configuration variables. *)
module Disable_warnings = struct

let temporary_working_directory_automatically_set =
  Configuration.extract_bool_variable_or 
    ~default:false 
    "MARIONNET_DISABLE_WARNING_TEMPORARY_WORKING_DIRECTORY_AUTOMATICALLY_SET"

end (* Warnings *)

(* Default for the factory-set configuration address for routers.
   The result is a couple (ip,nm) where ip is the 4-tuple IPv4 and nm is the CIDR netmask. *)
let router_port0_default_ipv4_config  =
 let variable_name = "MARIONNET_ROUTER_PORT0_DEFAULT_IPV4_CONFIG" in
 let default = "192.168.1.254/24" in
 let value = Configuration.extract_string_variable_or ~default variable_name in
 let parse arg = Ipv4.config_of_string arg in
 try parse value
 with _ -> begin
   Log.printf ~force:true "Warning: ill-formed value for %s\n" variable_name;
   parse default
   end
;;

let keep_all_snapshots_when_saving =
  Configuration.extract_bool_variable_or 
    ~default:false
    "MARIONNET_KEEP_ALL_SNAPSHOTS_WHEN_SAVING"

(* Enter the right directory: *)
try
  Unix.chdir Path.marionnet_home;
with _ ->
  failwith ("Could not enter the directory (" ^ Path.marionnet_home ^ ")");;
