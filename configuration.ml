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


(** Read configuration files: *)
let configuration =
  (* Lowest priority first: *)
  let file_names =
     [ Printf.sprintf "%s/share/marionnet/marionnet.conf" Meta.prefix; (* failsafe copy *)
       "/etc/marionnet/marionnet.conf";
       Printf.sprintf "%s/etc/marionnet/marionnet.conf" Meta.prefix;
       "~/.marionnet/marionnet.conf" ]
  in
  new Configuration_files.configuration
    ~file_names
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
		"MARIONNET_TERMINAL";
                "MARIONNET_PREFIX";
                "MARIONNET_FILESYSTEMS_PATH";
                "MARIONNET_KERNELS_PATH";
                "MARIONNET_VDE_PREFIX";
                "MARIONNET_ROUTER_FILESYSTEM";
                "MARIONNET_ROUTER_KERNEL";
                "MARIONNET_MACHINE_FILESYSTEM";
                "MARIONNET_MACHINE_KERNEL";
                "MARIONNET_ROUTER_PORT0_DEFAULT_IPV4_CONFIG";
                "MARIONNET_DISABLE_WARNING_TEMPORARY_WORKING_DIRECTORY_AUTOMATICALLY_SET";
                "MARIONNET_TMPDIR";
              ]
    ();;
