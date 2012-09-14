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

(* Read configuration files: *)
include Configuration ;;

(** Remember the cwd directory at startup time: *)
let cwd_at_startup_time =
  Unix.getcwd ();;

(*Ex: ~mthd:configuration#bool *)
let polymorphic_configuration_variable_or
  ?(k:('a -> 'a) option)
  ?dont_warning_if_undeclared
  ?(unsuitable_value=(fun y -> false)) (* values are suitable by default *)
  ~(to_string:'a -> string)
  ~(default:'a)
  ~(mthd:string -> 'a)
  (variable_name:string)
  =
  let fallback e x =
    let force = if dont_warning_if_undeclared=None then true else false in
    Log.printf ~force "Warning: %s not declared.\n" x
  in
  let use_default () =
    Log.printf " - using default \"%s\"\n" (to_string default);
    default
  in
  let use_found_value y =
    Log.printf " - found value \"%s\"\n" (to_string y);
    y
  in
  let result =
    Log.printf "Searching for variable %s:\n" variable_name;
    match Option.apply_or_catch ~fallback mthd variable_name
    with
    | None -> use_default ()
    | Some y when (unsuitable_value y) -> use_default ()
    | Some y -> use_found_value y
   in
   (* Launch the continuation on the result: *)
   match k with None -> result | Some f -> (f result)
 ;;

(** Return the value of the given configuration variable, if it's defined as
    a non-empty string; otherwise return the default.
    The third argument is a continuation that may be useful, for instance,
    to add a slash if it is a pathname. *)
let configuration_variable_or ?k ?dont_warning_if_undeclared ?(default="") variable_name =
  polymorphic_configuration_variable_or
    ?k
    ~unsuitable_value:((=)"")
    ~to_string:(fun x->x)
    ~default
    ~mthd:configuration#string
    variable_name

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

(* Path related configuration variables.
   TODO: make it more robust and logged *)
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

 let marionnet_tmpdir =
   match (configuration_variable_or ~default:"" "MARIONNET_TMPDIR") with
   | "" -> None
   | v  -> Some v

end (* Path *)
;;

(* Warnings related configuration variables. *)
module Disable_warnings = struct

let temporary_working_directory_automatically_set =
  polymorphic_configuration_variable_or
    ~dont_warning_if_undeclared:()
    ~to_string:(string_of_bool)
    ~default:false
    ~mthd:configuration#bool
    "MARIONNET_DISABLE_WARNING_TEMPORARY_WORKING_DIRECTORY_AUTOMATICALLY_SET"

end (* Warnings *)

(* Default for the factory-set configuration address for routers.
   The result is a couple (ip,nm) where ip is the 4-tuple IPv4 and nm is the CIDR netmask. *)
let router_port0_default_ipv4_config  =
 let variable_name = "MARIONNET_ROUTER_PORT0_DEFAULT_IPV4_CONFIG" in
 let default = "192.168.1.254/24" in
 let value = configuration_variable_or ~default variable_name in
 let parse arg = Ipv4.config_of_string arg in
 try parse value
 with _ -> begin
   Log.printf ~force:true "Warning: ill-formed value for %s\n" variable_name;
   parse default
   end
;;

let keep_all_snapshots_when_saving =
  polymorphic_configuration_variable_or
    ~dont_warning_if_undeclared:()
    ~to_string:(string_of_bool)
    ~default:false
    ~mthd:configuration#bool
    "MARIONNET_KEEP_ALL_SNAPSHOTS_WHEN_SAVING"

(* Enter the right directory: *)
try
  Unix.chdir Path.marionnet_home;
with _ ->
  failwith ("Could not enter the directory (" ^ Path.marionnet_home ^ ")");;
