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


(* open PreludeExtra.Prelude;; (\* We want synchronous terminal output *\) *)
open Unix;;
open Sys;;

(** First of all print the version number. This is useful for problem reports: *)
Printf.printf
  "=======================================================\n  Welcome to %s version %s\n  Please report bugs to marionnet-dev@marionnet.org \n=======================================================\n\n"
  Meta.name Meta.version;;

(* Seed the random number generator: *)
Random.self_init ();;

(** Read configuration files: *)
let configuration =
  new Configuration_files.configuration
    ~software_name:"marionnet"
    ~variables:["MARIONNET_SOCKET_NAME";
                "MARIONNET_BRIDGE";(* This is temporary: more than one bridge will be usable... *)
                "MARIONNET_KEYBOARD_LAYOUT";
                "MARIONNET_DEBUG";
                "MARIONNET_PDF_READER";
                "MARIONNET_POSTSCRIPT_READER";
                "MARIONNET_DVI_READER";
                "MARIONNET_HTML_READER";
                "MARIONNET_TEXT_EDITOR";
                (* *Optional* configuration variables: *)
                "MARIONNET_PREFIX";
                "MARIONNET_FILESYSTEMS_PATH";
                "MARIONNET_KERNELS_PATH";
                "MARIONNET_VDE_PREFIX";
                "MARIONNET_ROUTER_FILESYSTEM";
              ]
    ();;

(** Remember the cwd directory at startup time: *)
let cwd_at_startup_time =
  Unix.getcwd ();;
