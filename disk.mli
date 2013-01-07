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

(** Manage files and informations installed on disk(s). *)

val machine_prefix : string
val router_prefix : string
val kernel_prefix : string

(* `epithet' is almost a phantom type: *)
type 'a epithet = string
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


class ['a] epithet_manager :
  ?default_epithet:('a epithet) ->
  ?filter:('a epithet -> bool) ->
  kind: [> `distrib | `kernel | `variant ] ->
  directory_searching_list:string list ->
  prefix:string ->
  unit ->
  object
    (* Constructor's arguments: *)

    method directory_searching_list : dirname list
    method prefix : string

    (* Public interface: *)

    method get_epithet_list    : 'a epithet list
    method get_default_epithet : 'a epithet option
    method epithet_exists      : 'a epithet -> bool
    method realpath_of_epithet : 'a epithet -> realpath

    method resolve_epithet_symlink : 'a epithet -> 'a epithet

    (* Morally private methods: *)

    method epithets_of_filename : ?no_symlinks:unit ->
      filename -> ('a epithet) list

    method epithets_sharing_the_same_realpath_of : ?no_symlinks:unit ->
      ('a epithet) -> ('a epithet) list

    method filename_of_epithet : ('a epithet) -> filename
    method realpath_exists : string -> bool

    method filter : ('a epithet -> bool) -> unit

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

    method filesystems : [`distrib] epithet_manager
    method kernels     : [`kernel]  epithet_manager

    method variants_of           : [`distrib] epithet -> [`variant] epithet_manager
    method supported_kernels_of  : [`distrib] epithet -> ([`kernel] epithet * (string option)) list
    method get_kernel_console_arguments : [`distrib] epithet -> [`kernel] epithet -> string option

    method terminal_manager_of   : [`distrib] epithet -> terminal_manager

    (* filesystem epithet -> dirname *)
    method root_export_dirname : [`distrib] epithet -> dirname
    method user_export_dirname : [`distrib] epithet -> dirname

    (* This method builds warnings if necessary: *)
    method check_filesystems_MTIME_consistency : unit -> unit

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
  string -> virtual_machine_installations * [`distrib] epithet

val user_export_dirname_of_prefixed_filesystem : string -> dirname
val root_export_dirname_of_prefixed_filesystem : string -> dirname

module Make_and_check_installations :
  functor (Unit : sig end) ->
    sig
      val machines : virtual_machine_installations
      val routers  : virtual_machine_installations
    end
