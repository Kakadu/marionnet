(* This file is part of Marionnet
   Copyright (C) 2013  Jean-Vincent Loddo
   Copyright (C) 2013  Université Paris 13

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

(** Follow the descendance of a process in order to clean the process table when exiting. *)

(** Start the thread monitoring a pid descendance. The result is the thunk killing the still running processes. *)
val start_monitor_and_get_kill_method :
  ?pid:int ->                               (* Default: Unix.getpid () *)
  ?wake_up_interval:float ->                (* Default: 4. (seconds) *)
  ?garbage_collection_frequence:int ->      (* Default: 4  (remove death processes each 4 cycles => each 16 seconds by default) *)
  unit -> (unit -> unit)
