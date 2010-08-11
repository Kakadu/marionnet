(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2009  Jean-Vincent Loddo

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

open Gettext;;

(** Toolbar entry for the component 'socket' *)

(* Shortcuts *)
type env  = string Environment.string_env
let mkenv = Environment.make_string_env

module Make_menus (State : sig val st:State.globalState end) = struct

  open State

  module Toolbar_entry = struct
   let imagefile = "ico.socket.palette.png"
   let tooltip   = (s_ "Outgoing RJ45 socket")
  end

  module Add = struct

    let key      = Some GdkKeysyms._E

    let dialog   =
      let module M = Gui_dialog_SOCKET.Make (State) in
      M.dialog ~title:(s_ "Add socket") ~update:None

    let reaction r =
      let defects = Defects_interface.get_defects_interface () in
      let (name,label) = (r#get "name"),(r#get "label") in
      defects#add_device name "socket" 1;
      let g = (new Mariokit.Netmodel.bridge_socket ~network:st#network ~name ~label ()) in
      st#network#addBridge_socket g;
      st#update_sketch () ;
      st#update_state  ();
      st#update_cable_sensitivity ()

  end

  module Properties = struct

    let dynlist () =
      List.filter
        (fun x -> (st#network#getBridge_socketByName x)#can_startup)
        (st#network#getBridge_socketNames)

    let dialog =
     fun name -> let m = (st#network#getBridge_socketByName name) in
                 let title = (s_ "Modify socket") in
                 let module M = Gui_dialog_SOCKET.Make (State) in
                 M.dialog ~title:(title^" "^name) ~update:(Some m)

    let reaction r =
      let defects = Defects_interface.get_defects_interface () in
      let (name,oldname) = (r#get "name"),(r#get "oldname") in
      let g = st#network#getBridge_socketByName oldname in
      st#network#changeNodeName oldname name  ;
      g#set_label (r#get "label")             ;
      st#refresh_sketch () ;
      st#update_state  ();
      g#destroy;
      defects#rename_device oldname name;
      st#update_cable_sensitivity ()

  end

  module Remove = struct

    let dynlist     = Properties.dynlist
    let dialog name =
      Talking.EDialog.ask_question ~help:None ~cancel:false
        ~enrich:(mkenv [("name",name)])
        ~gen_id:"answer"
        ~title:(s_ "Remove")
        ~question:(Printf.sprintf (f_ "Are you sure that you want to remove %s\nand all the cables connected to this %s?") name (s_ "socket"))

    let reaction r =
      let defects = Defects_interface.get_defects_interface () in
      let (name,answer) = r#get("name"),r#get("answer") in
      if (answer="yes") then begin
        (st#network#delBridge_socket name);
        st#update_sketch ();
        st#update_state  ();
        defects#remove_device name;
        st#update_cable_sensitivity ();
      end
      else ()

  end

  module Startup = struct

    let dynlist    = Properties.dynlist
    let dialog     = Menu_factory.no_dialog
    let reaction r = (st#network#getBridge_socketByName (r#get "name"))#startup

  end

  module Stop = struct

    let dynlist () =
      List.filter
       (fun x -> (st#network#getBridge_socketByName x)#can_gracefully_shutdown)
       (st#network#getBridge_socketNames)

    let dialog = Menu_factory.no_dialog
    let reaction r = (st#network#getBridge_socketByName (r#get "name"))#gracefully_shutdown
  end

  module Suspend = struct

    let dynlist () =
      List.filter
       (fun x -> (st#network#getBridge_socketByName x)#can_suspend)
       (st#network#getBridge_socketNames)

    let dialog = Menu_factory.no_dialog
    let reaction r = (st#network#getBridge_socketByName (r#get "name"))#suspend

  end

  module Resume = struct

    let dynlist () =
      List.filter
       (fun x -> (st#network#getBridge_socketByName x)#can_resume)
       (st#network#getBridge_socketNames)

    let dialog = Menu_factory.no_dialog
    let reaction r = (st#network#getBridge_socketByName (r#get "name"))#resume

  end

 module Create_entries =
  Gui_toolbar_COMPONENTS_layouts.Layout_for_network_node (State) (Toolbar_entry) (Add) (Properties) (Remove) (Startup) (Stop) (Suspend) (Resume)

end
