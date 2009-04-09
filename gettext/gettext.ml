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


(* Make the primitives implemented in C visible to the OCaml world: *)
external non_thread_safe_gettext_primitive : string -> string = "gettext_primitive";;
external initialize_gettext : string -> string -> unit = "initialize_gettext_primitive";;

(* gettext() isn't thread-safe, so we should sequentialize concurrent calls
   with a mutex: *)
let the_mutex = Mutex.create ();;

(* Wrap the main primitive within a thread-safe function: *)
let gettext string_in_english =
  Mutex.lock the_mutex;
  let result = non_thread_safe_gettext_primitive string_in_english in
  Mutex.unlock the_mutex;
  result;;

(* Public versions for strings, with the type we like: *)
let s_ = gettext;;

(* Public versions for format strings, with the type we like: *)
let f_ x = (Obj.magic gettext ((Obj.magic x) : string));;
