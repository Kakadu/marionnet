(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2011  Jean-Vincent Loddo
   Copyright (C) 2011  Université Paris 13

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

(** Parts of the environment variable DISPLAY. *)

(* The syntax of $DISPLAY is: [host]:display[.screen] *)

val host    : string        (* "localhost" by default *)
val display : string
val screen  : string        (* "0" by default *)

val mit_magic_cookie_1 : string option  (* The result of `xauth list $DISPLAY' *)
val cookie             : string option  (* Just an alias for `mit_magic_cookie_1' *)

val get_unused_local_display : unit -> string
