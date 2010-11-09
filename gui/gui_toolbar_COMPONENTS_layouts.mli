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


module Toolbar : sig
 val append_image_menu : GButton.toolbar -> string -> string -> GMenu.image_menu_item
end

module type Toolbar_entry =
 sig
  val imagefile : string
  val tooltip   : string
  val packing   : [ `toolbar of GButton.toolbar | `menu_parent of Menu_factory.menu_parent ]
 end

module type State = sig val st:State.globalState end

(** Called for instance by gui_cloud.ml *)
module Layout_for_network_node :
 functor (State         : State) ->
 functor (Toolbar_entry : Toolbar_entry) ->
 functor (Add           : Menu_factory.Entry_callbacks) ->
 functor (Properties    : Menu_factory.Entry_with_children_callbacks) ->
 functor (Remove        : Menu_factory.Entry_with_children_callbacks) ->
 functor (Startup       : Menu_factory.Entry_with_children_callbacks) ->
 functor (Stop          : Menu_factory.Entry_with_children_callbacks) ->
 functor (Suspend       : Menu_factory.Entry_with_children_callbacks) ->
 functor (Resume        : Menu_factory.Entry_with_children_callbacks)
 -> sig module F:Menu_factory.Factory end

(** Called for instance by gui_machine.ml *)
module Layout_for_network_node_with_state :
 functor (State         : State) ->
 functor (Toolbar_entry : Toolbar_entry) ->
 functor (Add        : Menu_factory.Entry_callbacks) ->
 functor (Properties : Menu_factory.Entry_with_children_callbacks) ->
 functor (Remove     : Menu_factory.Entry_with_children_callbacks) ->
 functor (Startup    : Menu_factory.Entry_with_children_callbacks) ->
 functor (Stop       : Menu_factory.Entry_with_children_callbacks) ->
 functor (Suspend    : Menu_factory.Entry_with_children_callbacks) ->
 functor (Resume     : Menu_factory.Entry_with_children_callbacks) ->
 functor (Ungracefully_stop : Menu_factory.Entry_with_children_callbacks)
 -> sig module F:Menu_factory.Factory end

(** Called for instance by gui_cable.ml *)
module Layout_for_network_edge :
 functor (State         : State) ->
 functor (Toolbar_entry : Toolbar_entry) ->
 functor (Add        : Menu_factory.Entry_callbacks) ->
 functor (Properties : Menu_factory.Entry_with_children_callbacks) ->
 functor (Remove     : Menu_factory.Entry_with_children_callbacks) ->
 functor (Disconnect : Menu_factory.Entry_with_children_callbacks) ->
 functor (Reconnect  : Menu_factory.Entry_with_children_callbacks)
 -> sig
     module F:Menu_factory.Factory
     module Created_Add : (* Useful handler for cable sensitiveness. *)
      sig
        val item     : GMenu.image_menu_item
        val callback : unit -> unit
      end
    end

