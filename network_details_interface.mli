(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2010  Jean-Vincent Loddo

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

class network_details_interface :
  packing:(GObj.widget -> unit) ->
  after_user_edit_callback:(string -> unit) ->
  unit ->
  object

  method add_checkbox_column :
    header:string ->
    ?shown_header:string ->
    ?hidden:bool ->
    ?reserved:bool ->
    ?default:(unit -> Row_item.row_item) ->
    ?constraint_predicate:(Row_item.row_item -> bool) ->
    unit -> Treeview.checkbox_column

  method add_device :
    ?port_row_completions:(string * (string * Row_item.row_item) list) list ->
    string -> string -> int -> unit

  method add_editable_string_column :
    header:string ->
    ?shown_header:string ->
    ?italic:bool ->
    ?bold:bool ->
    ?hidden:bool ->
    ?reserved:bool ->
    ?default:(unit -> Row_item.row_item) ->
    ?constraint_predicate:(Row_item.row_item -> bool) ->
    unit -> Treeview.editable_string_column

  method add_icon_column :
    header:string ->
    ?shown_header:string ->
    ?hidden:bool ->
    ?reserved:bool ->
    ?default:(unit -> Row_item.row_item) ->
    strings_and_pixbufs:(string * string) list ->
    unit -> Treeview.icon_column

  method add_menu_item :
    string -> (string option -> bool) -> (string option -> unit) -> unit

  method add_row : ?parent_row_id:string -> Treeview.row -> string

  method add_row_constraint :
    ?name:string -> ((string * Row_item.row_item) list -> bool) -> unit

  method add_separator_menu_item : unit

  method add_string_column :
    header:string ->
    ?shown_header:string ->
    ?italic:bool ->
    ?bold:bool ->
    ?hidden:bool ->
    ?reserved:bool ->
    ?default:(unit -> Row_item.row_item) ->
    ?constraint_predicate:(Row_item.row_item -> bool) ->
    unit -> Treeview.string_column

  method check_constraints : (string * Row_item.row_item) list -> unit
  method children_of : string -> string list
  method clear : unit
  method collapse_everything : unit
  method collapse_row : string -> unit
  method columns : Treeview.column list
  method create_store_and_view : unit
  method detach_view_in : (unit -> unit) -> unit
  method expand_everything : unit
  method expand_row : string -> unit
  method file_name : string
  method for_all_rows : (Gtk.tree_iter -> unit) -> unit
  method get_column : string -> Treeview.column
  method get_complete_forest : Treeview.row Forest.forest
  method get_complete_row : string -> Treeview.row
  method get_forest : (string * Row_item.row_item) list Forest.forest
  method get_id_forest : string Forest.forest
  method get_next_identifier : int
  method get_port_attribute : string -> string -> string -> string
  method get_port_attribute_by_index : string -> int -> string -> string
  method get_port_data : string -> string -> (string * Row_item.row_item) list
  method get_port_data_by_index : string -> int -> (string * Row_item.row_item) list
  method get_row : string -> (string * Row_item.row_item) list
  method get_row_item : string -> string -> Row_item.row_item
  method gtree_column_list : GTree.column_list
  method highlight_row : string -> unit
  method id_to_iter : string -> Gtk.tree_iter
  method id_to_path : string -> Gtk.tree_path
  method is_column_reserved : string -> bool
  method is_file_name_defined : bool
  method is_row_expanded : string -> bool
  method is_row_highlighted : string -> bool
  method is_view_detached : bool
  method iter_on_forest : (Gtk.tree_iter -> unit) -> Gtk.tree_iter option -> unit
  method iter_on_tree : (Gtk.tree_iter -> unit) -> Gtk.tree_iter -> unit
  method iter_to_id : Gtk.tree_iter -> string
  method iter_to_path : Gtk.tree_iter -> Gtk.tree_path
  method load : unit
  method make_identifier : int
  method parent_of : string -> string option
  method path_to_id : Gtk.tree_path -> string
  method path_to_iter : Gtk.tree_path -> Gtk.tree_iter
  method port_no_of : string -> int
  method remove_device : string -> unit
  method remove_reserved_fields : Treeview.row -> (string * Row_item.row_item) list
  method remove_row : string -> unit
  method remove_subtree : string -> unit
  method rename_device : string -> string -> unit
  method reset : unit
  method reset_file_name : unit
  method reset_identifier : unit
  method row_id_such_that : (Treeview.row -> bool) -> string
  method row_ids_such_that : (Treeview.row -> bool) -> string list
  method row_such_that : (Treeview.row -> bool) -> (string * Row_item.row_item) list
  method rows_such_that :
    (Treeview.row -> bool) -> (string * Row_item.row_item) list list
  method run_after_update_callback : string -> unit
  method save : unit
  method select_row : string -> unit
  method selected_row : (string * Row_item.row_item) list option
  method selected_row_id : string option
  method set_after_update_callback : (string -> unit) -> unit
  method set_collapse_row_callback : (string -> unit) -> unit
  method set_column : string -> string -> Row_item.row_item -> unit
  method set_column_visibility : string -> bool -> unit
  method set_contextual_menu_title : string -> unit
  method set_double_click_on_row_callback : (string -> unit) -> unit
  method set_expand_row_callback : (string -> unit) -> unit
  method set_expanded_row_ids : string list -> unit
  method set_file_name : string -> unit
  method set_forest : Treeview.row Forest.forest -> unit
  method set_next_identifier : int -> unit
  method set_port_attribute_by_index :
    string -> int -> string -> Row_item.row_item -> unit
  method set_port_string_attribute_by_index : string -> int -> string -> string -> unit
  method set_row : string -> Treeview.row -> unit
  method set_row_highlight_color : string -> string -> unit
  method set_row_item : string -> string -> Row_item.row_item -> unit
  method store : GTree.tree_store
  method unhighlight_row : string -> unit
  method unselect : unit
  method update_port_no :
    ?port_row_completions:(string * (string * Row_item.row_item) list) list ->
    string -> int -> unit
end

val make_network_details_interface :
  packing:(GObj.widget -> unit) ->
  after_user_edit_callback:(string -> unit) ->
  unit -> network_details_interface

val get_network_details_interface : unit -> network_details_interface
