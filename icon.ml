(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007, 2008  Luca Saiu
   Copyright (C) 2010  Jean-Vincent Loddo
   Copyright (C) 2007, 2008, 2010  Université Paris 13

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

let icon_pixbuf =
  GdkPixbuf.from_file
    (if Initialization.are_we_in_exam_mode then
      Initialization.Path.images^"launcher-icons/marionnet-exam-launcher.png"
    else
      Initialization.Path.images^"launcher-icons/marionnet-launcher.png");;
