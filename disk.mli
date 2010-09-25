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

(** Manage files and informations installed on disk(s). *)

val machine_prefix : string
val router_prefix : string
val kernel_prefix : string

type epithet = string
type variant = string
type filename = string
type dirname = string
type realpath = string

val root_filesystem_searching_list : dirname list
val user_filesystem_searching_list : dirname list

val kernel_searching_list : dirname list

class terminal_manager :
 unit ->
 object
   method get_choice_list : string list
   method get_default     : string
   method is_valid_choice : string -> bool
   method is_xnest        : string -> bool
   method is_nox          : string -> bool
   method is_hostxserver  : string -> bool
 end  


class epithet_manager :
  ?default_epithet:string ->
  directory_searching_list:string list ->
  prefix:string ->
  unit ->
  object
    (* Constructor's arguments: *)
    
    method directory_searching_list : dirname list
    method prefix : string

    (* Public interface: *)
    
    method get_epithet_list    : epithet list
    method get_default_epithet : epithet option
    method epithet_exists      : epithet -> bool
    method realpath_of_epithet : epithet -> realpath
  
    method resolve_epithet_symlink : epithet -> epithet

    (* Morally private methods: *)
    
    method epithets_of_filename : ?no_symlinks:unit ->
      filename -> epithet list

    method epithets_sharing_the_same_realpath_of : ?no_symlinks:unit ->
      epithet -> epithet list

    method filename_of_epithet : epithet -> filename
    method realpath_exists : string -> bool

  end

class virtual_machine_installations :
  ?user_filesystem_searching_list:string list ->
  ?root_filesystem_searching_list:string list ->
  ?kernel_searching_list:string list ->
  ?kernel_prefix:string ->
  ?kernel_default_epithet:string -> 
  ?filesystem_default_epithet:string ->
  prefix:string ->
  unit ->
  object
    (* Constructor's arguments: *)
    
    method filesystem_searching_list : dirname list
    method kernel_searching_list     : dirname list
    method kernel_prefix : string
    method prefix : string
    
    (* Public interface: *)
    
    method filesystems : epithet_manager
    method kernels     : epithet_manager
    method variants_of : epithet -> epithet_manager
    method terminal_manager_of : epithet -> terminal_manager

    (* filesystem epithet -> dirname *)
    method root_export_dirname : epithet -> dirname
    method user_export_dirname : epithet -> dirname
  end

(** Final user's machines strictu sensu *)
val get_machine_installations :
  ?user_filesystem_searching_list:string list ->
  ?root_filesystem_searching_list:string list ->
  ?kernel_searching_list:string list ->
  ?kernel_prefix:string ->
  ?kernel_default_epithet:string ->
  ?filesystem_default_epithet:string ->
  unit -> virtual_machine_installations

val get_router_installations :
  ?user_filesystem_searching_list:string list ->
  ?root_filesystem_searching_list:string list ->
  ?kernel_searching_list:string list ->
  ?kernel_prefix:string ->
  ?kernel_default_epithet:string ->
  ?filesystem_default_epithet:string ->
  unit -> virtual_machine_installations

val vm_installations_and_epithet_of_prefixed_filesystem :
  string -> virtual_machine_installations * epithet
  
val user_export_dirname_of_prefixed_filesystem : string -> dirname
val root_export_dirname_of_prefixed_filesystem : string -> dirname
 