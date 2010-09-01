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

(** Toolbar entry for the component 'switch' *)

(* Shortcuts *)
type env  = string Environment.string_env
let mkenv = Environment.make_string_env

module Make_menus (State : sig val st:State.globalState end) = struct

  open State

  module Toolbar_entry = struct
   let imagefile = "ico.switch.palette.png"
   let tooltip   = (s_ "Switch")
  end

  module Add = struct
    let key      = Some (GdkKeysyms._S)
    let dialog   = let module M = Gui_dialog_SWITCH.Make (State) in M.dialog ~title:(s_ "Add switch") ~update:None

    let reaction r =
      let defects = Defects_interface.get_defects_interface () in
      let (name,eth) = (r#get "name"),(r#get "eth") in
      defects#add_device name "switch" (int_of_string eth);
      let d = (new Mariokit.Netmodel.device ~network:st#network ~name ~label:(r#get "label")
                ~devkind:Mariokit.Netmodel.Switch
                ~variant:Strings.no_variant_text
                (int_of_string eth) ()) in
      d#resolve_variant; (* don't store the variant as a symlink *)
      st#network#add_device d;
      st#update_cable_sensitivity ();
      st#update_sketch ();
      st#update_state  ();
      st#update_cable_sensitivity ()

  end

  module Properties = struct

    let dynlist () =
      List.filter
        (fun x -> (st#network#get_device_by_name x)#can_startup)
        (st#network#get_switch_names)

    let dialog =
     fun name -> let m = (st#network#get_device_by_name name) in
                 let title = (s_ "Modify switch") in
                 let module M = Gui_dialog_SWITCH.Make (State) in
                 M.dialog ~title:(title^" "^name) ~update:(Some m)

    let reaction r =
      let defects = Defects_interface.get_defects_interface () in
      let (name,oldname,eth) = (r#get "name"),(r#get "oldname"),(r#get "eth") in
      let d = st#network#get_device_by_name oldname in
      d#destroy;
      st#network#ledgrid_manager#destroy_device_ledgrid ~id:(d#id) ();
      st#network#change_node_name oldname name  ;
      d#set_label (r#get "label")             ;
      d#set_eth_number ~prefix:"port"  (int_of_string eth)  ;
      st#refresh_sketch () ;
      st#network#make_device_ledgrid d;
      Filesystem_history.rename_device oldname name;
      defects#rename_device oldname name;
      defects#update_ports_no name (int_of_string eth);
      st#update_cable_sensitivity ()

  end

  module Remove = struct

    let dynlist     = Properties.dynlist
    let dialog name =
      Talking.EDialog.ask_question ~help:None ~cancel:false
        ~enrich:(mkenv [("name",name)])
        ~gen_id:"answer"
        ~title:(s_ "Remove")
        ~question:(Printf.sprintf (f_ "Are you sure that you want to remove %s\nand all the cables connected to this %s?") name (s_ "switch"))

    let reaction r =
      let details = Network_details_interface.get_network_details_interface () in
      let defects = Defects_interface.get_defects_interface () in
      let (name,answer) = r#get("name"),r#get("answer") in
      if (answer="yes") then begin
        (st#network#del_device name) ;
        st#update_sketch ();
        st#update_state  ();
        st#update_cable_sensitivity ();
        defects#remove_device name;
        end
      else ()


  end

  module Startup = struct

    let dynlist    = Properties.dynlist
    let dialog     = Menu_factory.no_dialog
    let reaction r = (st#network#get_device_by_name (r#get "name"))#startup

  end

  module Stop = struct

    let dynlist () =
      List.filter
       (fun x -> (st#network#get_device_by_name x)#can_gracefully_shutdown)
       (st#network#get_switch_names)

    let dialog = Menu_factory.no_dialog
    let reaction r = (st#network#get_device_by_name (r#get "name"))#gracefully_shutdown
  end

  module Suspend = struct

    let dynlist () =
      List.filter
       (fun x -> (st#network#get_device_by_name x)#can_suspend)
       (st#network#get_switch_names)

    let dialog = Menu_factory.no_dialog
    let reaction r = (st#network#get_device_by_name (r#get "name"))#suspend

  end

  module Resume = struct

    let dynlist () =
      List.filter
       (fun x -> (st#network#get_device_by_name x)#can_resume)
       (st#network#get_switch_names)

    let dialog = Menu_factory.no_dialog
    let reaction r = (st#network#get_device_by_name (r#get "name"))#resume

  end

 module Create_entries =
  Gui_toolbar_COMPONENTS_layouts.Layout_for_network_node (State) (Toolbar_entry) (Add) (Properties) (Remove) (Startup) (Stop) (Suspend) (Resume)

end
