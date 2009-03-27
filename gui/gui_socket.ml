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


(** Toolbar entry for the component 'socket' *)

(* Shortcuts *)
type env  = string Environment.string_env
let mkenv = Environment.make_string_env

module Make_menus (State : sig val st:State.globalState end) = struct

  open State

  module Toolbar_entry = struct
   let imagefile = "ico.socket.palette.png"
   let tooltip   = "Prise RJ45 vers l'extérieur"
  end

  module Add = struct

    let key      = Some GdkKeysyms._E

    let dialog   =
      let module M = Gui_dialog_SOCKET.Make (State) in
      M.dialog ~title:"Ajouter prise" ~update:None

    let reaction r =
      let defects = Defects_interface.get_defects_interface () in
      let (name,label) = (r#get "name"),(r#get "label") in
      defects#add_device name "gateway" 1;
      let g = (new Mariokit.Netmodel.gateway ~network:st#network ~name ~label ()) in
      st#network#addGateway g;
      st#update_sketch () ;
      st#update_state  ();
      st#update_cable_sensitivity ()

  end

  module Properties = struct

    let dynlist () =
      List.filter
        (fun x -> (st#network#getGatewayByName x)#can_startup)
        (st#network#getGatewayNames)

    let dialog =
     fun name -> let m = (st#network#getGatewayByName name) in
                 let title = "Modifier prise" in
                 let module M = Gui_dialog_SOCKET.Make (State) in
                 M.dialog ~title:(title^" "^name) ~update:(Some m)

    let reaction r =
      let defects = Defects_interface.get_defects_interface () in
      let (name,oldname) = (r#get "name"),(r#get "oldname") in
      let g = st#network#getGatewayByName oldname in
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
        ~title:"Supprimer"
        ~question:(Printf.sprintf "Confirmez-vous la suppression de %s\net du cable éventuellement branché à cette %s ?" name "prise")

    let reaction r =
      let defects = Defects_interface.get_defects_interface () in
      let (name,answer) = r#get("name"),r#get("answer") in
      if (answer="yes") then begin
        (st#network#delGateway name);
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
    let reaction r = (st#network#getGatewayByName (r#get "name"))#startup

  end

  module Stop = struct

    let dynlist () =
      List.filter
       (fun x -> (st#network#getGatewayByName x)#can_gracefully_shutdown)
       (st#network#getGatewayNames)

    let dialog = Menu_factory.no_dialog
    let reaction r = (st#network#getGatewayByName (r#get "name"))#gracefully_shutdown
  end

  module Suspend = struct

    let dynlist () =
      List.filter
       (fun x -> (st#network#getGatewayByName x)#can_suspend)
       (st#network#getGatewayNames)

    let dialog = Menu_factory.no_dialog
    let reaction r = (st#network#getGatewayByName (r#get "name"))#suspend

  end

  module Resume = struct

    let dynlist () =
      List.filter
       (fun x -> (st#network#getGatewayByName x)#can_resume)
       (st#network#getGatewayNames)

    let dialog = Menu_factory.no_dialog
    let reaction r = (st#network#getGatewayByName (r#get "name"))#resume

  end

 module Create_entries =
  Gui_toolbar_COMPONENTS_layouts.Layout_for_network_node (State) (Toolbar_entry) (Add) (Properties) (Remove) (Startup) (Stop) (Suspend) (Resume)

end
