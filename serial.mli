(* This file is part of marionnet
   Copyright (C) 2011 Jean-Vincent Loddo

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


val send_command :
  ?umid:string ->
  ?con:string ->
  ?pts:string ->
  string -> unit

val send_command_and_wait_answer :
  ?timeout:float ->
  ?buffer_size:int ->
  ?umid:string ->
  ?con:string ->
  ?pts:string ->
  string -> (StringExtra.Text.t * int)
