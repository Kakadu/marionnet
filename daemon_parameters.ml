(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2008  Luca Saiu
   Copyright (C) 2008  Université Paris 13

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


(** The number of seconds which must elapse from the last keepalive
    for the server to destroy all the resources of a client: *)
let timeout_interval = 120.0;;
assert (timeout_interval > 0.0);;

let select_timeout = timeout_interval;;
assert (select_timeout > 0.0);;

(** The number of seconds which should elapse between successive keepalives
    from the same client. It's safer to make this considerably smaller than
    timeout_interval, so that the client can send messages at a reasonable
    frequency even when under load, without its resources being destroyed: *)
let inter_keepalive_interval = timeout_interval /. 5.0;;

let socket_name =
  let default = "/tmp/my-marionnet-daemon-socket" in
  Configuration.extract_string_variable_or ~default "MARIONNET_SOCKET_NAME";;

(** How often we should print information about the currently allocated
    resources: *)
let debug_interval = timeout_interval /. 2.0;;
