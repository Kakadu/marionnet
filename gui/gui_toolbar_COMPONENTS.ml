(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2009, 2010  Jean-Vincent Loddo
   Copyright (C) 2009, 2010  Université Paris 13

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


(** Gui completion for the toolbar_COMPONENTS widget defined with glade. *)

open Gettext

module Make (State : sig val st:State.globalState end) = struct
 module Direct    = struct let crossover = false end
 module Crossover = struct let crossover = true  end

 module Params = struct
   include State
   let packing = `toolbar st#mainwin#toolbar_COMPONENTS
   end

 module Menus_for_machine = Machine.Make_menus (Params)
 module Menus_for_hub     = Hub.Make_menus (Params)
 module Menus_for_switch  = Switch. Make_menus (Params)
 module Menus_for_router  = Router. Make_menus (Params)
 module Menus_for_direct_cable    = Cable. Make_menus (Params) (Direct)
 module Menus_for_crossover_cable = Cable. Make_menus (Params) (Crossover)
 module Menus_for_cloud   = Cloud. Make_menus (Params)

 (* World gateway and bridge in the same sub-toolbar: *)
 module World_access_button = struct

   module F = Menu_factory.Make (struct
    let toolbar = State.st#mainwin#toolbar_COMPONENTS
    let image_menu_item =
       Gui_toolbar_COMPONENTS_layouts.Toolbar.append_image_menu
         toolbar
         "ico.world.palette.png"
         (s_ "Real world access")
    let parent = Menu_factory.Menuitem (image_menu_item :> GMenu.menu_item_skel)
    let window = State.st#mainwin#window_MARIONNET
   end)

   let world_gateway_menu_parent =
      let filename = Filename.concat Initialization.Path.images "ico.world_gateway.palette.png" in
      F.add_imagefile_item ~label:"Gateway" filename ()

   let world_bridge_menu_parent =
      let filename = Filename.concat Initialization.Path.images "ico.world_bridge.palette.png" in
      F.add_imagefile_item ~label:"Bridge" filename ()

   end

 module Params_for_world_gateway = struct
  include State
  let menu_parent = World_access_button.world_gateway_menu_parent
  let packing = `menu_parent (Menu_factory.Menuitem (menu_parent :> GMenu.menu_item_skel))
 end

 module Params_for_world_bridge = struct
  include State
  let menu_parent = World_access_button.world_bridge_menu_parent
  let packing = `menu_parent (Menu_factory.Menuitem (menu_parent :> GMenu.menu_item_skel))
 end

 module Menus_for_world_gateway = World_gateway. Make_menus (Params_for_world_gateway)
 module Menus_for_world_bridge  = World_bridge. Make_menus (Params_for_world_bridge)

end

