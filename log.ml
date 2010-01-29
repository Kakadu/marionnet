(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2010  Jean-Vincent Loddo

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


(* A simple ocamlbricks' functor application: *)
include Log_builder.Make_simple (struct
  let is_log_enabled = Global_options.get_debug_mode
 end)

(* Wrappers providing a logged version of functions defined elsewhere. *)

(** Wrapper for [UnixExtra.system_or_fail]: run system with the given argument,
    and raise exception in case of failure; return unit on success. *)
let system_or_fail ?hide_output ?hide_errors command_line =
  let extract_hide_decision h = match h with
  | None          -> not (Global_options.get_debug_mode ())
  | Some decision -> decision in
  let hide_output = extract_hide_decision hide_output in
  let hide_errors = extract_hide_decision hide_errors in
  printf "Executing: %s\n" command_line;
  UnixExtra.system_or_fail ~hide_output ~hide_errors command_line
