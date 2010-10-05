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


(** Gui completion for the toolbar_COMPONENTS widget defined with glade. *)

module Make (State : sig val st:State.globalState end) = struct
 module Direct    = struct let cablekind = Mariokit.Netmodel.Direct    end
 module Crossover = struct let cablekind = Mariokit.Netmodel.Crossover end
 module Menus_for_machine = Gui_machine.Make_menus (State)
 module Menus_for_hub     = Hub.Make_menus (State)
 module Menus_for_switch  = Switch. Make_menus (State)
 module Menus_for_router  = Router. Make_menus (State)
 module Menus_for_direct_cable    = Gui_cable. Make_menus (State) (Direct)
 module Menus_for_crossover_cable = Gui_cable. Make_menus (State) (Crossover)
 module Menus_for_cloud   = Gui_cloud.  Make_menus (State)
 module Menus_for_world_gateway = World_gateway. Make_menus (State)
 module Menus_for_world_bridge  = Gui_world_bridge. Make_menus (State)
end

