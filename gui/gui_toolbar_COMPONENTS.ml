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
 module Created_menus_for_machine = Gui_machine.Make_menus (State)
 module Created_menus_for_hub     = Gui_hub.    Make_menus (State)
 module Created_menus_for_switch  = Gui_switch. Make_menus (State)
 module Created_menus_for_router  = Gui_router. Make_menus (State)
 module Created_menus_for_direct_cable    = Gui_cable. Make_menus (State) (Direct)
 module Created_menus_for_crossover_cable = Gui_cable. Make_menus (State) (Crossover)
 module Created_menus_for_cloud   = Gui_cloud.  Make_menus (State)
 module Created_menus_for_socket  = Gui_socket. Make_menus (State)
end
