(* This file is part of marionnet
   Copyright (C) 2010 Jean-Vincent Loddo

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


type epithet = string
type variant = string
type filename = string
type dirname = string
type realpath = string

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

 (** Read the given directory searching for names like [~prefix ^ "xxxxx"];
     return the list of epithets ["xxxxx"]. *)
let read_epithet_list ~prefix ~dir =
  let prefix_length = String.length prefix in
  let remove_prefix s = String.sub s prefix_length ((String.length s) - prefix_length) in
  let name_filter file_name =
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
let make_epithet_to_dir_mapping ?realpath ~prefix ~directory_searching_list () =
  let normalize_dir = match realpath with
   | None    -> (fun x -> Some x)
   | Some () -> (fun x -> UnixExtra.realpath x)
  in
  let searching_list = List.rev directory_searching_list in
  let xss =
     List.map
       (fun dir ->
          let epithet_list = read_epithet_list ~prefix ~dir in
          List.map (fun x -> (x, (normalize_dir dir))) epithet_list
        )
        searching_list
  in
  let yss = List.flatten xss in
  let yss = List.filter (fun (e,d)->d<>None) yss in
  let yss = List.map (function (e, Some dir)->(e,dir) | _ -> assert false) yss in
  Log.printf "Searching for prefix: \"%s\"\n" prefix;
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


class epithet_manager
  ?(default_epithet="default")
  ~directory_searching_list
  ~prefix (* "machine-", "router-", "linux-", "" (for variants), ... *)
  () =
  let epithet_to_dir_mapping =
    make_epithet_to_dir_mapping ~realpath:() ~prefix ~directory_searching_list ()
  in
  object (self)

  method directory_searching_list = directory_searching_list
  method prefix = prefix

  method get_epithet_list =
    String_map.domain epithet_to_dir_mapping

  method epithet_exists epithet =
    String_map.mem epithet epithet_to_dir_mapping

  method (*private*) filename_of_epithet epithet =
    let dir = String_map.find epithet epithet_to_dir_mapping in
    (Printf.sprintf "%s/%s%s" dir prefix epithet)

  method realpath_of_epithet epithet =
    let filename = (self#filename_of_epithet epithet) in
    match (UnixExtra.realpath filename) with
    | Some x -> x
    | None   -> filename

  method (*private*) epithets_of_filename ?no_symlinks filename =
    let realpath = Option.extract (UnixExtra.realpath filename) in
    let pred = match no_symlinks with
     | None    -> (fun e -> (self#realpath_of_epithet e) = realpath)
     | Some () ->
       (fun e ->
          (not (UnixExtra.is_symlink (self#filename_of_epithet e))) &&
          ((self#realpath_of_epithet e) = realpath))
    in
    List.filter pred self#get_epithet_list
    
  (* [machine-]default -> [machine-]debian-51426 *)
  method resolve_epithet_symlink epithet =
   let filename = self#filename_of_epithet epithet in
   match UnixExtra.is_symlink filename with
   | false -> epithet
   | true  ->
      (match (self#epithets_of_filename ~no_symlinks:() filename) with
      | []            -> epithet
      | epithet'::_   -> epithet' (* we get the first *)
      )

  method epithets_sharing_the_same_realpath_of ?no_symlinks epithet =
   let filename = self#filename_of_epithet epithet in
   self#epithets_of_filename ?no_symlinks filename
   
  method realpath_exists filename =
    let xs = List.map (self#filename_of_epithet) self#get_epithet_list in
    List.mem filename xs

  (* When a machine is created, we call this method to set a default epithet.*)
  method get_default_epithet =
    if self#epithet_exists default_epithet then (Some default_epithet) else
    let xs = self#get_epithet_list in
    match xs with
    | []   -> None
    | x::_ -> Some x (* We get the first as default... *)

end (* class epithet_manager *)


class virtual_machine_installations
  ?(user_filesystem_searching_list = user_filesystem_searching_list)
  ?(root_filesystem_searching_list = root_filesystem_searching_list)
  ?(kernel_searching_list=kernel_searching_list)
  ?(kernel_prefix = kernel_prefix)
  ?kernel_default_epithet
  ?filesystem_default_epithet
  ~prefix (* "machine-", "router-", ... *)
  () =
  (* The actual filesystem searching list is the merge of user (prioritary)
     and root lists: *)
  let filesystem_searching_list =
    List.append user_filesystem_searching_list root_filesystem_searching_list
  in
  (* The manager for filesystem epithets: *)
  let filesystems =
    new epithet_manager
	~prefix
	~directory_searching_list:filesystem_searching_list
	?default_epithet:filesystem_default_epithet
	()
  in
  (* The manager for kernel epithets: *)
  let kernels =
    new epithet_manager
       ~prefix:kernel_prefix
       ~directory_searching_list:kernel_searching_list
       ?default_epithet:kernel_default_epithet
       ()
  in
  (* The kit of managers (one per filesystem epithet) for variant epithets: *)
  let variants =
   let epithet_manager_of filesystem_epithet =
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
       ~prefix:""
       ~directory_searching_list
       ()
    end
   in
   let assoc_list =
     List.map (fun e -> (e,epithet_manager_of e)) filesystems#get_epithet_list
   in
   String_map.of_list assoc_list
  in

  let terminal_manager =
    new terminal_manager ()
  in
  
  object
  method filesystem_searching_list = filesystem_searching_list
  method kernel_searching_list = kernel_searching_list
  method kernel_prefix = kernel_prefix
  method prefix = prefix

  method filesystems = filesystems
  method kernels = kernels
  method variants_of filesystem_epithet = String_map.find filesystem_epithet variants

  (** Terminal choices to handle uml machines.
      The list doesn't depend on the choosen distribution (in this version): *)
  method terminal_manager_of (_:epithet) = terminal_manager

  method root_export_dirname epithet =
    let root_dir = List.hd root_filesystem_searching_list in
    (Printf.sprintf "%s/%s%s_variants" root_dir prefix epithet)

  method user_export_dirname epithet =
    let user_dir = List.hd user_filesystem_searching_list in
    (Printf.sprintf "%s/%s%s_variants" user_dir prefix epithet)

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
