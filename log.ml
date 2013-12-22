(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2008  Luca Saiu
   Copyright (C) 2010  Jean-Vincent Loddo
   Copyright (C) 2008, 2010  Université Paris 13

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

%str_item escape_raise_filter
;;

(* Initialized later, by Global_options, in order to break the cyclic dependency: *)
module Self = Log_builder.Make (struct
  let debug_level () = 0           (* the debug_level must be greater or equal to the verbosity, otherwise do nothing *)
  let verbosity = 1                (* the default value of verbosity for printing functions *)
  let log_channel = `stderr        (* put messages here *)
  let synchronized = true          (* using threads *)
 end);;

include (Log_builder.Extend_with_wrappers (Self)) ;;

(* Setting the ocamlbricks log verbosity to the same value: *)
let () = Ocamlbricks_log.Tuning.Set.verbosity (Tuning.verbosity ())
;;

(** Wrappers for system_or_ignore: the command is performed by Unix.system
    with logging features. In case of failure, the function doesn't produce
    any exception, but print the event on the log channel. *)
module Command = struct
 let ll pathname = system_or_ignore ("ls -l "^pathname)
end
