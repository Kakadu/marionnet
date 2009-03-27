(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2009  Jean-Vincent Loddo

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


(* A menu can be attached to a menubar or to a menu_item_skel (as a submenu). *)
type menu_parent =
    Menubar  of GMenu.menu_shell
  | Menuitem of GMenu.menu_item_skel
  | Menu     of GMenu.menu

module type Factory =
    sig
      val factory : GMenu.menu_shell GMenu.factory
      val accel_group : Gtk.accel_group

      val add_menu : string -> GMenu.menu GMenu.factory

      val not_implemented_yet : 'a -> unit
      val monitor : string -> 'a -> unit

      val add_item :
        ?menu:GMenu.menu GMenu.factory ->
        ?submenu:GMenu.menu ->
        ?key:Gdk.keysym ->
        string ->
        ?callback:(unit -> unit) ->
        unit -> GMenu.menu_item

      val add_stock_item :
        ?menu:GMenu.menu GMenu.factory ->
        ?submenu:GMenu.menu ->
        ?key:Gdk.keysym ->
        string ->
        stock:GtkStock.id ->
        ?callback:(unit -> unit) ->
        unit -> GMenu.image_menu_item

      val add_imagefile_item :
        ?menu:GMenu.menu GMenu.factory ->
        ?submenu:GMenu.menu ->
        ?key:Gdk.keysym ->
        string ->
        ?callback:(unit -> unit) ->
        unit -> GMenu.image_menu_item

      val add_check_item :
        ?menu:GMenu.menu GMenu.factory ->
        ?active:bool ->
        ?key:Gdk.keysym ->
        string ->
        ?callback:(bool -> unit) ->
        unit -> GMenu.check_menu_item

      val add_separator :
        ?menu:GMenu.menu GMenu.factory ->
        unit -> unit

      val get_current_menu : unit -> GMenu.menu GMenu.factory
      val parent : menu_parent
      val window : GWindow.window
    end

module type Parents      = sig  val parent: menu_parent  val window : GWindow.window  end

module Make : functor (M : Parents) -> Factory

type env  = string Environment.string_env
type name = string

val mkenv     : (string * 'a) list -> 'a Environment.string_env
val no_dialog : 'a -> unit -> 'a Environment.string_env option

module type Entry_definition =
  sig
    val text     : string
    val stock    : GtkStock.id
    val key      : Gdk.keysym option
    val dialog   : unit -> env option
    val reaction : env -> unit
  end

module type Entry_with_children_definition =
  sig
    val text     : string
    val stock    : GtkStock.id
    val dynlist  : unit -> string list
    val dialog   : name -> unit -> env option
    val reaction : env -> unit
  end

module type Entry_callbacks =
  sig
    val key      : Gdk.keysym option
    val dialog   : unit -> env option
    val reaction : env -> unit
  end

module type Entry_with_children_callbacks =
  sig
    val dynlist  : unit -> string list
    val dialog   : name -> unit -> env option
    val reaction : env -> unit
  end

module Make_entry :
  functor (E : Entry_definition) ->
    functor (F : Factory) ->
      sig
        val item     : GMenu.image_menu_item
        val callback : unit -> unit
      end

module Make_entry_with_children :
  functor (E : Entry_with_children_definition) ->
    functor (F : Factory) ->
      sig
        val item     : GMenu.image_menu_item
        val submenu  : GMenu.menu
        val callback : name -> unit -> unit
      end
