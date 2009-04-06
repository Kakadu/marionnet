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


(** Gui completion for the widget window_MARIONNET (main window) defined with glade. *)

#load "chip_parser.p4.cmo"
let () = Chip.teach_ocamldep

module Make (State : sig val st:State.globalState end) = struct

 open State
 let w = st#mainwin

 chip main_window_title_chip : (prj_filename) -> () =
  let title = match prj_filename with
  | None          -> Command_line.window_title
  | Some filename -> (Command_line.window_title ^ " - " ^ filename)
  in
  w#window_MARIONNET#set_title title

 let main_window_title = new main_window_title_chip ~prj_filename:st#prj_filename ()

end
