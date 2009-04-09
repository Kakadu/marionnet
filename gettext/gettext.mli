(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2009  Luca Saiu

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


(** Initialize gettext support. The first parameter is the text domain, the second one
    is the locale directory. This must be called before any calls to gettext: *)
val initialize_gettext : string -> string -> unit;;

(** Given a string in English, return its translated version: *)
val gettext : string -> string;;

(** Given a string in English, return its translated version. This is just an alias for
    gettext: *)
val s_ : string -> string;;

(** Given a format string in English (to be used with Printf.printf and friends), return
    its translated version: *)
val f_ : (('a, out_channel, unit) format) -> (('a, out_channel, unit) format);;
