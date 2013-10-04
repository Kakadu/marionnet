(* This file is part of marionnet
   Copyright (C) 2010  Jean-Vincent Loddo
   Copyright (C) 2010  Université Paris 13

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

(* `epithet' is almost a phantom type (almost because it is not abstract): *)
type 'a epithet = string
type variant = string
type filename = string
type dirname = string
type realpath = string

let string_of_epithet_kind =
  function
  | `distrib -> "distribution"
  | `variant -> "variant"
  | `kernel  -> "kernel"
  | _ -> assert false

class terminal_manager () =
 let hostxserver_name = "X HOST" in
 let xnest_name       = "X NEST" in
 let nox_name         = "No X" in
 object (self)
   method get_choice_list =
     [ hostxserver_name; xnest_name; nox_name ]

   method get_default = hostxserver_name
   method is_valid_choice x = List.mem x self#get_choice_list
   method is_hostxserver = ((=)hostxserver_name)
   method is_xnest       = ((=)xnest_name)
   method is_nox         = ((=)nox_name)
 end

(** Some name filters (predicates on strings): *)
module Filter = struct

 let ending_with_dot_relay =
   StrExtra.First.matchingp (Str.regexp "[.]relay\\($\\|[._-][a-zA-Z0-9._-]*[~]?$\\)")

 let ending_with_dot_conf =
   StrExtra.First.matchingp (Str.regexp "[.]conf[~]?$")

 let exclude_names_ending_with_dot_conf_or_dot_relay x =
    not ((ending_with_dot_conf x) || (ending_with_dot_relay x))

end (* Filter *)

 (** Read the given directory searching for names like [~prefix ^ "xxxxx"];
     return the list of epithets ["xxxxx"]. *)
(* let read_epithet_list ?(name_filter=fun _ -> true) ~prefix ~dir () = *)
let read_epithet_list ?(name_filter=Filter.exclude_names_ending_with_dot_conf_or_dot_relay) ~prefix ~dir () =
  let prefix_length = String.length prefix in
  let remove_prefix s = String.sub s prefix_length ((String.length s) - prefix_length) in
  let name_filter file_name =
    (name_filter file_name) &&
    ((String.length file_name) > prefix_length) &&
    ((String.sub file_name 0 prefix_length) = prefix)
  in
  let xs =
    SysExtra.readdir_as_list
       ~only_not_directories:()
       ~name_filter
       ~name_converter:remove_prefix
       dir in
  Log.printf ~v:2 "Searching in %s:\n" dir;
  List.iter (fun x -> Log.printf ~v:2 " - found %s%s\n" prefix x) xs;
  xs


let machine_prefix = "machine-"
let router_prefix  = "router-"
let kernel_prefix  = "linux-"

let root_filesystem_searching_list = [
   Initialization.Path.filesystems;
   ]

let user_filesystem_searching_list = [
   Initialization.Path.user_filesystems;
   ]

(* In the order of priority: *)
let kernel_searching_list = [
   Initialization.Path.user_kernels;
   Initialization.Path.kernels;
   ]

module String_map = MapExtra.String_map

(* For a given choice the last binding with a directory will wins building the mapping.
   So we reverse the searching list: *)
let make_epithet_to_dir_mapping ~kind ?realpath ~prefix ~directory_searching_list () =
  Log.printf "Searching for a (%s) prefix: \"%s\"\n" (string_of_epithet_kind kind) prefix;
  let normalize_dir = match realpath with
   | None    -> (fun x -> Some x)
   | Some () -> (fun x -> UnixExtra.realpath x)
  in
  let searching_list = List.rev directory_searching_list in
  let xss =
     List.map
       (fun dir ->
          let epithet_list = read_epithet_list ~prefix ~dir () in
          List.map (fun x -> (x, (normalize_dir dir))) epithet_list
        )
        searching_list
  in
  let yss = List.flatten xss in
  let yss = List.filter (fun (e,d)->d<>None) yss in
  let yss = List.map (function (e, Some dir)->(e,dir) | _ -> assert false) yss in
  (List.iter (function (e,d) -> Log.printf "* %s -> %s\n" e d) yss);
  String_map.of_list yss


(** epithet -> (variant list) * dir *)
let make_epithet_to_variant_list_and_dir_mapping ~prefix ~epithet_to_dir_mapping =
    String_map.mapi
      (fun epithet dir ->
        let dir = Printf.sprintf "%s/%s%s_variants" dir prefix epithet in
        ((read_epithet_list ~prefix:"" ~dir), dir)
      )
      epithet_to_dir_mapping


class type ['a] epithet_manager_object =
  object
    (* Constructor's arguments: *)
    method directory_searching_list : dirname list
    method prefix : string (* "machine-", "router-", "kernel-", "" (nothing for variants) *)
    (* Public interface: *)
    method get_epithet_list    : 'a epithet list
    method get_default_epithet : 'a epithet option
    method epithet_exists      : 'a epithet -> bool
    method realpath_of_epithet : 'a epithet -> realpath
    method resolve_epithet_symlink : 'a epithet -> 'a epithet
    (* Morally private methods: *)
    method epithets_of_filename : ?no_symlinks:unit -> filename -> ('a epithet) list
    method epithets_sharing_the_same_realpath_of : ?no_symlinks:unit -> ('a epithet) -> ('a epithet) list
    method filename_of_epithet : ('a epithet) -> filename
    method realpath_exists : string -> bool
    method filter : ('a epithet -> bool) -> unit
  end


class ['a] epithet_manager
  : ?default_epithet:('a epithet) ->
    ?filter:('a epithet->bool) ->
    kind: [> `distrib | `kernel | `variant ] ->
    directory_searching_list:string list ->
    prefix:string ->
    unit -> ['a] epithet_manager_object
  =
  fun
  ?(default_epithet="default")
  ?filter
  ~kind
  ~directory_searching_list
  ~prefix (* "machine-", "router-", "linux-", "" (for variants), ... *)
  ()
  ->
  let epithet_to_dir_mapping =
    make_epithet_to_dir_mapping ~kind ~realpath:() ~prefix ~directory_searching_list ()
  in
  (* Filter the list if required with the optional parameter `filter': *)
  let epithet_to_dir_mapping =
    match filter with
    | None   -> epithet_to_dir_mapping
    | Some f -> String_map.filter (fun epth _dir -> f epth) epithet_to_dir_mapping
  in
  object (self)

  (* The version stored in the object is the destructive (non-persistent)
     version of the same mapping: *)
  val mutable epithet_to_dir_mapping = epithet_to_dir_mapping

  (* Destructive filter application: *)
  method filter f =
    epithet_to_dir_mapping <- String_map.filter (fun epth _dir -> f epth) (epithet_to_dir_mapping)

  method directory_searching_list = directory_searching_list
  method prefix = prefix

  method get_epithet_list : 'a epithet list =
    String_map.domain epithet_to_dir_mapping

  method epithet_exists (epithet:'a epithet) : bool =
    String_map.mem epithet epithet_to_dir_mapping

  method (*private*) filename_of_epithet (epithet:'a epithet) =
    let dir = String_map.find epithet epithet_to_dir_mapping in
    (Printf.sprintf "%s/%s%s" dir prefix epithet)

  method realpath_of_epithet (epithet:'a epithet) : realpath =
    let filename = (self#filename_of_epithet epithet) in
    match (UnixExtra.realpath filename) with
    | Some x -> x
    | None   -> filename

  method (*private*) epithets_of_filename ?no_symlinks (filename:string) : ('a epithet) list =
    let realpath = Option.extract (UnixExtra.realpath filename) in
    let pred = match no_symlinks with
     | None    -> (fun e -> (self#realpath_of_epithet e) = realpath)
     | Some () ->
       (fun e ->
          (not (UnixExtra.is_symlink (self#filename_of_epithet e))) &&
          ((self#realpath_of_epithet e) = realpath))
    in
    (List.filter pred self#get_epithet_list)

  (* [machine-]default -> [machine-]debian-51426 *)
  method resolve_epithet_symlink (epithet:'a epithet) : 'a epithet =
   let filename = self#filename_of_epithet epithet in
   match UnixExtra.is_symlink filename with
   | false -> epithet
   | true  ->
      (match (self#epithets_of_filename ~no_symlinks:() filename) with
      | []            -> epithet
      | epithet'::_   -> epithet' (* we get the first *)
      )

  method epithets_sharing_the_same_realpath_of ?(no_symlinks:unit option) (epithet:'a epithet) : ('a epithet) list =
   let filename = self#filename_of_epithet epithet in
   self#epithets_of_filename ?no_symlinks filename

  method realpath_exists filename =
    let xs = List.map (self#filename_of_epithet) self#get_epithet_list in
    List.mem filename xs

  (* When a machine is created, we call this method to set a default epithet.*)
  method get_default_epithet : 'a epithet option =
    if self#epithet_exists default_epithet then (Some default_epithet) else
    let xs = self#get_epithet_list in
    match xs with
    | []   -> None
    | x::_ -> Some x (* We get the first as default... *)

end (* class epithet_manager *)

let get_and_parse_SUPPORTED_KERNELS (t : Configuration_files.t) : string -> (unit, string option) Either.t =
  let x = Configuration_files.get_string_list_variable "SUPPORTED_KERNELS" t in
  let brackets = (Str.regexp "^\\[\\(.*\\)\\]$") in
  let slashes  = (Str.regexp "^/\\(.*\\)/$") in
  let extract result =
    let (_,_,groups) = Option.extract result in
    List.hd groups
  in
  let rec loop acc = function
  | [] -> (List.rev acc)
  | x::xs when (StrExtra.First.matchingp brackets x) ->
      let brackets_content = extract (StrExtra.First.matching brackets x) in
      loop ((`Brackets brackets_content)::acc) xs
  | x::xs when (StrExtra.First.matchingp slashes x) ->
      let slashes_content = extract (StrExtra.First.matching slashes x) in
      loop ((`Slashes slashes_content)::acc) xs
  | x::xs ->
      loop ((`AString x)::acc) xs
  in
  let token_list : ([`Brackets of string | `Slashes of string | `AString of string] list) option =
    Option.map (loop []) x
  in
  let rec collapse_AString acc = function
  | [] -> List.rev acc
  | (`AString x)::(`AString y)::zs -> collapse_AString acc ((`AString (String.concat " " [x;y]))::zs)
  | x::ys -> collapse_AString (x::acc) ys
  in
  let token_list = Option.map (collapse_AString []) token_list in
  let rec parse acc = function
  | [] -> List.rev acc
  | (`Brackets x)::(`AString y)::zs -> parse (((`kernel_epithet x), Some y)::acc) zs
  | (`Brackets x)::zs               -> parse (((`kernel_epithet x), None)::acc) zs
  | (`Slashes x)::(`AString y)::zs  -> parse (((`kernel_regexpr (Str.regexp x)), Some y)::acc) zs
  | (`Slashes x)::zs                -> parse (((`kernel_regexpr (Str.regexp x)), None)::acc) zs
  | (`AString x)::_ ->
      let msg = Printf.sprintf "Parsing variable SUPPORTED_KERNELS: unexpected string `%s'" x in
      failwith msg
  in
  let parsing_result
    : ([> `kernel_epithet of string | `kernel_regexpr of Str.regexp ] * string option) list option
    = Option.map (parse []) token_list
  in
  let parsing_result_as_predicate_list : ((string -> bool) * string option) list option =
    let epithet_predicate_of = function
    | `kernel_epithet x -> ((=)x)
    | `kernel_regexpr r -> (StrExtra.First.matchingp r)
    in
    Option.map (List.map (fun (k,so) -> ((epithet_predicate_of k),so))) parsing_result
  in
  function epithet ->
    match parsing_result_as_predicate_list with
    | None -> Either.Right (None)  (* The epithet is ok, without special console options *)
    | Some pred_so_list ->
        begin
          match (ListExtra.search (fun (pred,so) -> pred epithet) pred_so_list) with
          | None -> Either.Left () (* The epithet will be not accepted *)
          | Some (_,options) -> Either.Right (options) (* The epithet is ok, may be with options *)
        end
  (* end of get_and_parse_SUPPORTED_KERNELS() *)

class virtual_machine_installations
  ?(user_filesystem_searching_list = user_filesystem_searching_list)
  ?(root_filesystem_searching_list = root_filesystem_searching_list)
  ?(kernel_searching_list=kernel_searching_list)
  ?(kernel_prefix = kernel_prefix)
  ?(kernel_default_epithet:[`kernel] epithet option)
  ?(filesystem_default_epithet:[`distrib] epithet option)
  ~prefix (* "machine-", "router-", ... *)
  () =
  (* The actual filesystem searching list is the merge of user (prioritary)
     and root lists: *)
  let filesystem_searching_list =
    List.append user_filesystem_searching_list root_filesystem_searching_list
  in
  (* The manager of all filesystem epithets: *)
  let filesystems : [`distrib] epithet_manager =
    new epithet_manager
        ~filter:Filter.exclude_names_ending_with_dot_conf_or_dot_relay
        ~kind:`distrib
	~prefix
	~directory_searching_list:filesystem_searching_list
	?default_epithet:filesystem_default_epithet
	()
  in
  (* The manager of all kernel epithets: *)
  let kernels : [`kernel] epithet_manager =
    new epithet_manager
        ~filter:Filter.exclude_names_ending_with_dot_conf_or_dot_relay
        ~kind:`kernel
        ~prefix:kernel_prefix
        ~directory_searching_list:kernel_searching_list
        ?default_epithet:kernel_default_epithet
        ()
  in
  (* The kit of managers (one per filesystem epithet) for variant epithets.
     This mapping is created from `filesystems#get_epithet_list' *)
  let filesystem_variants_mapping =
   let epithet_manager_of filesystem_epithet : [`variant] epithet_manager =
    begin
     let directory_searching_list_of e =
        List.map
          (fun dir -> Printf.sprintf "%s/%s%s_variants" dir prefix e)
          filesystem_searching_list
     in
     let directory_searching_list =
       let epithets = filesystems#epithets_sharing_the_same_realpath_of filesystem_epithet in
       let epithets = ListExtra.lift_to_the_top_positions ((=)filesystem_epithet) epithets in
       List.flatten (List.map directory_searching_list_of epithets)
     in
     new epithet_manager
        ~kind:`variant
        ~prefix:""
        ~directory_searching_list
       ()
    end
   in
   let assoc_list :  ([`distrib] epithet * [`variant] epithet_manager) list =
     List.map (fun e -> (e,epithet_manager_of e)) filesystems#get_epithet_list
   in
   String_map.of_list assoc_list
  in
  (* Now we build the mapping filesystem-epithet -> Configuration_files.t option *)
  let filesystem_config_mapping =
    let mill =
      fun filesystem_epithet ->
	let filename = filesystems#filename_of_epithet (filesystem_epithet) in
	let config_file = Printf.sprintf "%s.conf" (filename) in
	let result =
	  match Sys.file_exists (config_file) with
	  | false -> None
	  | true  ->
	      let () = Log.printf "configuration file found for \"%s\"\n" filesystem_epithet in
	      let config =
		Configuration_files.make
		  ~dont_read_environment:()
		  ~file_names:[config_file]
		  ~variables:[ "MD5SUM"; "AUTHOR"; "DATE"; "MTIME"; "SUPPORTED_KERNELS"; "X11_SUPPORT";
		               "MEMORY_MIN_SIZE"; "MEMORY_SUGGESTED_SIZE"; "MULTIPLE_CONSOLES_SUPPORT"; "BINARY_LIST"; ]
		  ()
	      in
	      Some (config)
	in
	result
    (* end mill () *)
    in
    String_map.of_list (List.map (fun e -> (e, mill e)) filesystems#get_epithet_list)
  in
  (* Now we build the mapping filesystem-epithet -> marionnet_relay-script list *)
  let filesystem_relay_script_mapping =
    let mill =
      fun filesystem_epithet ->
	let filename = filesystems#filename_of_epithet (filesystem_epithet) in
	let dot_relay_file = Printf.sprintf "%s.relay" (filename) in
	let result =
	  match Sys.file_exists (dot_relay_file) with
	  | false -> None
	  | true  ->
	      let () = Log.printf "relay script found for \"%s\"\n" filesystem_epithet in
	      Some (dot_relay_file)
	in
	result
    (* end mill () *)
    in
    String_map.of_list (List.map (fun e -> (e, mill e)) filesystems#get_epithet_list)
  in
  (* Now the mapping filesystem-epithet -> [(kernel1, console-options1); (kernel2, console-options2);...] option *)
  let filesystem_kernels_mapping =
    let mill =
      fun filesystem_epithet ->
        let config = String_map.find (filesystem_epithet) (filesystem_config_mapping) in
        Option.bind config
          (fun config_t ->
             try
               let filter : [`kernel] epithet -> (unit, string option) Either.t =
                  get_and_parse_SUPPORTED_KERNELS config_t
               in
               let ks = kernels#get_epithet_list in
               let ks = List.map (fun k -> (k, filter k)) ks in
               let ks = List.filter (fun (k,r) -> r <> Either.Left ()) ks in
               let ks = List.map (fun (k,r) -> (k, Either.extract r)) ks in
               let () =
                 Log.printf "Selected kernels for \"%s\": [%s]\n"
                   filesystem_epithet
                   (String.concat " " (List.map fst ks))
               in
               (Some ks)
             with Failure msg ->
                 let () = Log.printf "%s => \"%s\" config file ignored!\n" msg filesystem_epithet in
                 None)
    in
    String_map.of_list (List.map (fun e -> (e, mill e)) filesystems#get_epithet_list)
  in
  (* The manager for terminal (X support): *)
  let terminal_manager =
    new terminal_manager ()
  in
  object (self)
  method filesystem_searching_list = filesystem_searching_list
  method kernel_searching_list = kernel_searching_list
  method kernel_prefix = kernel_prefix
  method prefix = prefix

  method filesystems = filesystems
  method kernels = kernels

  method variants_of filesystem_epithet =
    String_map.find (filesystem_epithet) (filesystem_variants_mapping)

  method relay_script_of filesystem_epithet =
    String_map.find (filesystem_epithet) (filesystem_relay_script_mapping)

  (* Here, if we replace the first two lines of the following definition by:
    ---
    method supported_kernels_of (filesystem_epithet:[`distrib] epithet) : ([`kernel] epithet * (string option)) list =
    ---
    we obtain an error message about the method's type:
    [ `distrib ] epithet -> ('c epithet * string option) list where 'c is unbound *)
  method supported_kernels_of : [`distrib] epithet -> ([`kernel] epithet * (string option)) list =
    fun filesystem_epithet ->
      match String_map.find (filesystem_epithet) (filesystem_kernels_mapping) with
      | None    -> List.map (fun k -> (k,None)) kernels#get_epithet_list
      | Some ks -> ks

  (* Do not propose any filesystems which haven't at least one compatible installed kernel: *)
  initializer
    filesystems#filter
      (fun e -> (self#supported_kernels_of e)<>[])

  method get_kernel_console_arguments : [`distrib] epithet -> [`kernel] epithet -> string option =
    fun filesystem_epithet kernel_epithet ->
      try
        let ks = self#supported_kernels_of (filesystem_epithet) in
        List.assoc (kernel_epithet) ks
      with Not_found ->
        let () =
          Log.printf
            "Disk.virtual_machine_installations#get_kernel_console_arguments: couple (%s,%s) unknown!\n"
            (filesystem_epithet) (kernel_epithet)
        in None

  (** Terminal choices to handle uml machines.
      The list doesn't depend on the choosen distribution (in this version): *)
  method terminal_manager_of (_: [`distrib] epithet) = terminal_manager

  method root_export_dirname epithet =
    let root_dir = List.hd root_filesystem_searching_list in
    (Printf.sprintf "%s/%s%s_variants" root_dir prefix epithet)

  method user_export_dirname epithet =
    let user_dir = List.hd user_filesystem_searching_list in
    (Printf.sprintf "%s/%s%s_variants" user_dir prefix epithet)

  method multiple_consoles_supported_by epithet =
    let config = String_map.find (epithet) (filesystem_config_mapping) in
    if config = None then false else (* continue: *)
    let x = Configuration_files.get_bool_variable "MULTIPLE_CONSOLES_SUPPORT" (Option.extract config) in
    (x = Some true)

  method check_filesystems_MTIME_consistency () =
    let check =
      fun filesystem_epithet ->
        let config = String_map.find (filesystem_epithet) (filesystem_config_mapping) in
        if config = None then () else (* continue: *)
        let mtime = Configuration_files.get_int_variable "MTIME" (Option.extract config) in
        Option.iter
          (fun expected_mtime ->
             let realpath = filesystems#realpath_of_epithet (filesystem_epithet) in
             let actual_mtime =
               int_of_float ((Unix.stat realpath).Unix.st_mtime)
             in
             if actual_mtime = expected_mtime then () else (* warning: *)
	     let title = (s_ "Modification time (MTIME) inconsistency") in
	     let message =
	       Printf.sprintf
		 (f_ "The filesystem `%s%s' has the mtime %d, but the expected value was %d.\nPlease run the command:\n\n<tt><small>sudo touch -d @%d %s</small></tt>\n\nin order to fix this inconsistency. Otherwise, machines or routers with this filesystem defined in a project created elsewhere can not be restarted.")
 		 (prefix) (filesystem_epithet) (actual_mtime) (expected_mtime) (expected_mtime) (realpath)
	     in
             Simple_dialogs.warning title message ())
          mtime
    in
    List.iter (check) filesystems#get_epithet_list

end

let get_router_installations
  ?(user_filesystem_searching_list = user_filesystem_searching_list)
  ?(root_filesystem_searching_list = root_filesystem_searching_list)
  ?(kernel_searching_list=kernel_searching_list)
  ?(kernel_prefix = kernel_prefix)
  ?(kernel_default_epithet=Initialization.router_kernel_default_epithet)
  ?(filesystem_default_epithet=Initialization.router_filesystem_default_epithet)
  () =
     new virtual_machine_installations
       ~prefix:"router-"
       ~kernel_default_epithet
       ~filesystem_default_epithet
       ()

let get_machine_installations
  ?(user_filesystem_searching_list = user_filesystem_searching_list)
  ?(root_filesystem_searching_list = root_filesystem_searching_list)
  ?(kernel_searching_list=kernel_searching_list)
  ?(kernel_prefix = kernel_prefix)
  ?(kernel_default_epithet=Initialization.machine_kernel_default_epithet)
  ?(filesystem_default_epithet=Initialization.machine_filesystem_default_epithet)
  () =
     new virtual_machine_installations
       ~prefix:"machine-"
       ~kernel_default_epithet
       ~filesystem_default_epithet
       ()

let vm_installations_and_epithet_of_prefixed_filesystem prefixed_filesystem =
 try
  let p = String.index prefixed_filesystem '-' in
  let prefix = String.sub prefixed_filesystem 0 (p+1) in
  let epithet = String.sub prefixed_filesystem (p+1) ((String.length prefixed_filesystem)-(p+1)) in
  let vm_installations =
    (match prefix with
     | "machine-" -> get_machine_installations ()
     | "router-"  -> get_router_installations ()
     | _ -> (assert false)
     )
  in
  (vm_installations, epithet)
 with _ -> failwith (Printf.sprintf "vm_installations_and_epithet_of_prefixed_filesystem: %s" prefixed_filesystem)

let user_export_dirname_of_prefixed_filesystem prefixed_filesystem =
  let (vm_installations, epithet) =
    vm_installations_and_epithet_of_prefixed_filesystem prefixed_filesystem
  in
  vm_installations#user_export_dirname epithet

let root_export_dirname_of_prefixed_filesystem prefixed_filesystem =
  let (vm_installations, epithet) =
    vm_installations_and_epithet_of_prefixed_filesystem prefixed_filesystem
  in
  vm_installations#root_export_dirname epithet


module Make_and_check_installations (Unit:sig end) = struct

  let machines = get_machine_installations ()
  let routers  = get_router_installations ()

  let () = begin
    machines#check_filesystems_MTIME_consistency ();
    routers#check_filesystems_MTIME_consistency ();
    end

end (* Make_and_check_installations *)
