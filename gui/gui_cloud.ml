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

(** Toolbar entry for the component 'cloud' *)

(* Shortcuts *)
type env  = string Environment.string_env
let mkenv = Environment.make_string_env
let env_to_string t = t#to_string (fun s->s)

module Make_menus (State : sig val st:State.globalState end) = struct

  open State

  module Toolbar_entry = struct
   let imagefile = "ico.cloud.palette.png"
   let tooltip   = s_ "Unknown layer 2 sub-network"
  end

  module Add = struct
    type t = env
    let to_string = env_to_string
    let key      = None

    let dialog   =
      let module M = Gui_dialog_CLOUD.Make (State) in
      M.dialog ~title: (s_ "Add cloud") ~update:None

    let reaction r =
      let defects = Defects_interface.get_defects_interface () in
      let (name,label) = (r#get "name"),(r#get "label") in
      defects#add_device (*~defective_by_default:true*) name "cloud" 2;
      let c = (new Mariokit.Netmodel.cloud ~network:st#network ~name ~label ()) in
      st#network_change st#network#add_cloud c;
  end

  module Properties = struct
    type t = env
    let to_string = env_to_string

    let dynlist () =
      List.filter
        (fun x -> (st#network#get_cloud_by_name x)#can_startup)
        (st#network#get_cloud_names)

    let dialog name =
      let m = (st#network#get_cloud_by_name name) in
      let title = (s_ "Modify cloud") in
      let module M = Gui_dialog_CLOUD.Make (State) in
      M.dialog ~title:(title^" "^name) ~update:(Some m)

    let reaction r =
      let defects = Defects_interface.get_defects_interface () in
      let (name,oldname,label) = (r#get "name"),(r#get "oldname"),(r#get "label") in
      let c = st#network#get_cloud_by_name oldname in
      st#network#change_node_name oldname name  ;
      c#set_label label ;
      st#refresh_sketch ();
      st#update_state  ();
      c#destroy;
      defects#rename_device oldname name;
      st#update_cable_sensitivity ()

  end

  module Remove = struct
    type t = env
    let to_string = env_to_string

    let dynlist     = Properties.dynlist
    let dialog name =
      Talking.EDialog.ask_question ~help:None ~cancel:false
        ~enrich:(mkenv [("name",name)])
        ~gen_id:"answer"
        ~title: (s_ "Remove")
        ~question:(Printf.sprintf (f_ "Are you sure that you want to remove %s\nand all cables connected to this %s?") name (s_ "cloud"))

    let reaction r =
      let defects = Defects_interface.get_defects_interface () in
      let (name,answer) = r#get("name"),r#get("answer") in
      if (answer="yes") then begin
        st#network_change st#network#del_cloud name;
        defects#remove_device name;
        end
      else ()

  end

  module Startup = struct
    type t = env
    let to_string = env_to_string

    let dynlist    = Properties.dynlist
    let dialog     = Menu_factory.no_dialog
    let reaction r = (st#network#get_cloud_by_name (r#get "name"))#startup

  end

  module Stop = struct
    type t = env
    let to_string = env_to_string

    let dynlist () =
      List.filter
       (fun x -> (st#network#get_cloud_by_name x)#can_gracefully_shutdown)
       (st#network#get_cloud_names)

    let dialog = Menu_factory.no_dialog
    let reaction r = (st#network#get_cloud_by_name (r#get "name"))#gracefully_shutdown
  end

  module Suspend = struct
    type t = env
    let to_string = env_to_string

    let dynlist () =
      List.filter
       (fun x -> (st#network#get_cloud_by_name x)#can_suspend)
       (st#network#get_cloud_names)

    let dialog = Menu_factory.no_dialog
    let reaction r = (st#network#get_cloud_by_name (r#get "name"))#suspend

  end

  module Resume = struct
    type t = env
    let to_string = env_to_string

    let dynlist () =
      List.filter
       (fun x -> (st#network#get_cloud_by_name x)#can_resume)
       (st#network#get_cloud_names)

    let dialog = Menu_factory.no_dialog
    let reaction r = (st#network#get_cloud_by_name (r#get "name"))#resume

  end

 module Create_entries =
  Gui_toolbar_COMPONENTS_layouts.Layout_for_network_node (State) (Toolbar_entry) (Add) (Properties) (Remove) (Startup) (Stop) (Suspend) (Resume)

end
