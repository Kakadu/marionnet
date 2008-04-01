(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007, 2008  Luca Saiu

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


open PreludeExtra.Prelude;; (* We want synchronous terminal output *)
open Unix;;
open Sys;;
(*
(* Create the funny bridge we need for Ethernet plugs: *)
Printf.printf "Setting up the bridge...\n"; flush_all ();
let command_line =
  Printf.sprintf
    "%s/prepare_bridge.sh &> /dev/null"
    Pathnames.marionnet_home_bin
    in
match Unix.system command_line with
  Unix.WEXITED 0 -> begin
    Printf.printf "Ok, the bridge was setup.\n"; flush_all ();
  end
| _ -> begin
    Printf.printf "WARNING: Could not setup the bridge: Ethernet plugs won't work.\n"; flush_all ();
  end;;
*)

(* Seed the random number generator: *)
Random.self_init ();

(* Check that we're *not* running as root. Yes, this has been reversed
   since the last version: *)
Printf.printf "Checking whether Marionnet is running as root...\n";;
if (Unix.getuid ()) != 0 then begin
  Printf.printf "\n**********************************************\n";
  Printf.printf "* Marionnet should *not* be run as root, for * \n";
  Printf.printf "* security reasons.                          *\n";
  Printf.printf "*****************************************\n\n";
else
  Printf.printf "Success.\n\n";;
end;;
