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

(** Toolbar entry for the component 'cable' (direct/crossover) *)

(* Shortcuts *)
type env  = string Environment.string_env
let mkenv = Environment.make_string_env
let env_to_string t = t#to_string (fun s->s)

module Netmodel = Mariokit.Netmodel

module Make_menus
 (State     : sig val st:State.globalState end)
 (Cablekind : sig val cablekind:Netmodel.cablekind end)
 = struct

  open State
  open Cablekind

  module Toolbar_entry = struct

   let imagefile = match cablekind with
      | Netmodel.Direct    -> "ico.cable.direct.palette.png"
      | Netmodel.Crossover -> "ico.cable.crossed.palette.png"

   let tooltip   = match cablekind with
      | Netmodel.Direct    -> (s_ "Straight cable")
      | Netmodel.Crossover -> (s_ "Crossover cable")

  end

  (* Common tool for Add.reaction and Properties.reaction. *)
  let define_endpoints r =
    let sock1 = { Netmodel.nodename = (r#get "left_node")  ; Netmodel.receptname = (r#get "left_recept")  } in
    let sock2 = { Netmodel.nodename = (r#get "right_node") ; Netmodel.receptname = (r#get "right_recept") } in
    let left_device, left_port   = sock1.Netmodel.nodename, sock1.Netmodel.receptname in
    let right_device, right_port = sock2.Netmodel.nodename, sock2.Netmodel.receptname in
    let left_endpoint_name  = Printf.sprintf "to %s (%s)" left_device  left_port  in
    let right_endpoint_name = Printf.sprintf "to %s (%s)" right_device right_port in
    (sock1, sock2, left_endpoint_name, right_endpoint_name)

  (* Common tool for dynlists *)
  let get_cable_names () = match cablekind with
   | Netmodel.Direct    -> st#network#get_direct_cable_names
   | Netmodel.Crossover -> st#network#get_crossover_cable_names


  module Add = struct

    type t = env
    let to_string = env_to_string
    
    let key      = match cablekind with
      | Netmodel.Direct    -> Some GdkKeysyms._D
      | Netmodel.Crossover -> Some GdkKeysyms._C

    let dialog   =
      let module M = Gui_dialog_CABLE.Make (State) (Cablekind) in
      let title = match cablekind with
       | Netmodel.Direct    -> (s_ "Add straight cable")
       | Netmodel.Crossover -> (s_ "Add crossover cable")
      in M.dialog ~title ~update:None

    let reaction r =
      let defects = Defects_interface.get_defects_interface () in
      let (left, right, left_endpoint_name, right_endpoint_name) = define_endpoints r in
      let (name,label) = (r#get "name"),(r#get "label") in
      defects#add_cable name (Netmodel.string_of_cablekind cablekind) left_endpoint_name right_endpoint_name;
      let c = (new Netmodel.cable ~motherboard:st#motherboard ~network:st#network ~name ~label ~cablekind ~left ~right ()) in
(*      st#motherboard#reverted_rj45cables_cable#add_wire c#dotoptions#inverted_wire;*)
      st#network#add_cable c;
      st#refresh_sketch () ;
      st#update_cable_sensitivity ();
  end

  module Properties = struct

    type t = env
    let to_string = env_to_string

    let dynlist () = get_cable_names ()
    let dialog name =
      let module M = Gui_dialog_CABLE.Make (State) (Cablekind) in
      let c = (st#network#get_cable_by_name name) in
      let title = match cablekind with
       | Netmodel.Direct    -> (s_ "Modify straight cable")
       | Netmodel.Crossover -> (s_ "Modify crossover cable")
      in M.dialog ~title:(title^" "^name) ~update:(Some c)

    let reaction r =
      let defects = Defects_interface.get_defects_interface () in
      let (left, right, left_endpoint_name, right_endpoint_name) = define_endpoints r in
      let (name,oldname,label) = (r#get "name"),(r#get "oldname"),(r#get "label") in
      defects#rename_cable oldname name;
      defects#rename_cable_endpoints name left_endpoint_name right_endpoint_name;
      st#network#del_cable oldname;
      (* Make a new cable; it should have a different identity from the old one, and it's
         important that it's initialized anew, to get the reference counter right: *)
      let c = (new Netmodel.cable ~motherboard:st#motherboard ~network:st#network ~name ~label ~cablekind ~left ~right ()) in
      st#network#add_cable c;
      st#refresh_sketch () ;
      st#update_cable_sensitivity ()

  end

  module Remove = struct

    type t = env
    let to_string = env_to_string

    let dynlist     = Properties.dynlist

    let dialog name =
      let question = match cablekind with
       | Netmodel.Direct    -> Printf.sprintf (f_ "Are you sure that you want to remove the straight cable %s?") name
       | Netmodel.Crossover -> Printf.sprintf (f_ "Are you sure that you want to remove the crossover cable %s?") name
      in
      Talking.EDialog.ask_question ~help:None ~cancel:false
        ~enrich:(mkenv [("name",name)])
        ~gen_id:"answer"
        ~title: (s_ "Remove")
        ~question

    let reaction r =
      let defects = Defects_interface.get_defects_interface () in
      let name,answer = r#get("name"), r#get("answer") in
      if (answer="yes") then begin
        (st#network#del_cable name);
        st#refresh_sketch ();
        defects#remove_cable name;
        st#update_cable_sensitivity ()
        end
      else ()

  end

  module Disconnect = struct
    type t = env
    let to_string = env_to_string

    let dynlist () =
      List.filter (fun x -> (st#network#get_cable_by_name x)#can_suspend) (get_cable_names ())

    let dialog = Menu_factory.no_dialog
    let reaction r = (st#network#get_cable_by_name (r#get "name"))#suspend

  end

  module Reconnect = struct
    type t = env
    let to_string = env_to_string

    let dynlist () =
      List.filter (fun x -> (st#network#get_cable_by_name x)#can_resume) (get_cable_names ())

    let dialog = Menu_factory.no_dialog
    let reaction r = (st#network#get_cable_by_name (r#get "name"))#resume

  end

 module Create_entries =
  Gui_toolbar_COMPONENTS_layouts.Layout_for_network_edge (State) (Toolbar_entry) (Add) (Properties) (Remove) (Disconnect) (Reconnect)

end
