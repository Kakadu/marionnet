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

(** Toolbar entry for the component 'machine' *)

(* Shortcuts *)
type env  = string Environment.string_env
let mkenv = Environment.make_string_env

module Make_menus (State : sig val st:State.globalState end) = struct

  open State

  module Toolbar_entry = struct
   let imagefile = "ico.machine.palette.png"
   let tooltip   = (s_ "Machine")
  end

  module Add = struct
    let key      = Some (GdkKeysyms._M)
    let dialog   = let module M = Gui_dialog_MACHINE.Make (State) in M.dialog ~title:(s_ "Add machine") ~update:None

    let reaction r =
      let details = Network_details_interface.get_network_details_interface () in
      let defects = Defects_interface.get_defects_interface () in
      let (name,eth) = (r#get "name"),(r#get "eth") in
(*      let (name,eth) = (r#get "name"), "2" in*)
      details#add_device name "machine" (int_of_string eth);
      defects#add_device name "machine" (int_of_string eth);
      let m =
        (new Mariokit.Netmodel.machine ~network:st#network ~name
          ~mem:     (int_of_string (r#get "memory"))
          ~ethnum:  (int_of_string eth)
          ~distr:   (r#get "distrib")
          ~variant: (r#get "patch")
          ~ker:     (r#get "kernel")
          ~ter:     (r#get "term") ()) in
      (* Don't store the variant as a symlink: *)
      m#resolve_variant;
      st#network_change st#network#add_machine m;
      Filesystem_history.add_device name ("machine-"^(r#get "distrib")) m#get_variant "machine"

  end

  module Properties = struct

    (* Only not running machines (which can startup) can be modified. *)
    let dynlist =
     fun () -> List.filter
                  (fun x -> (st#network#get_machine_by_name x)#can_startup)
                  (st#network#get_machine_names)

    let dialog =
     fun name -> let m = (st#network#get_machine_by_name name) in
                 let title = (s_ "Modify machine") in
                 let module M = Gui_dialog_MACHINE.Make (State) in M.dialog ~title:(title^" "^name) ~update:(Some m)

    let reaction r =
      let details = Network_details_interface.get_network_details_interface () in
      let defects = Defects_interface.get_defects_interface () in
      let (name,oldname,eth) = (r#get "name"),(r#get "oldname"), (int_of_string (r#get "eth")) in
      let m = st#network#get_machine_by_name oldname in
      m#destroy; (* make sure the simulated object is in state 'no-device' *)
      st#network#change_node_name oldname name  ;
      m#set_memory      (int_of_string (r#get "memory"))  ;
      m#set_eth_number  eth;
      (* Distribution and variant can not be changed; they are set once and for all at creation time: *)
      (* Instead the kernel can be changed later: *)
      m#set_kernel      (r#get "kernel")  ;
      m#set_terminal    (r#get "term"  )  ;
      st#refresh_sketch () ;
      Filesystem_history.rename_device oldname name;
      details#rename_device oldname name;
      details#update_port_no name eth;
      defects#rename_device oldname name;
      defects#update_port_no name eth;
      st#update_cable_sensitivity ()

  end

  module Remove = struct

    (* As for properties, only not running machines can be removed. *)
    let dynlist = Properties.dynlist

    let dialog =
     fun name -> Talking.EDialog.ask_question ~help:None ~cancel:false
                   ~enrich:(mkenv [("name",name)])
                   ~gen_id:"answer"
                   ~title:(s_ "Remove")
                   ~question:(Printf.sprintf (f_ "Are you sure that you want to remove %s\nand all the cables connected to this %s?") name (s_ "machine"))
    let reaction r =
      let details = Network_details_interface.get_network_details_interface () in
      let defects = Defects_interface.get_defects_interface () in
      if (r#get "answer")="yes" then
        let name   = r#get("name") in
        st#network_change st#network#del_machine name;
        details#remove_device name;
        defects#remove_device name;
      else ()


  end

  module Startup = struct

    let dynlist    = Properties.dynlist
    let dialog     = Menu_factory.no_dialog
    let reaction r = (st#network#get_machine_by_name (r#get "name"))#startup

  end

  module Stop = struct

    (* For machines "stop" means "shutdown". *)
    let dynlist =
      fun () -> List.filter
                  (fun x -> (st#network#get_machine_by_name x)#can_gracefully_shutdown)
                  (st#network#get_machine_names)

    let dialog =
     fun name -> Talking.EDialog.ask_question ~help:None ~cancel:false
                   ~enrich:(mkenv [("name",name)])
                   ~gen_id:"answer"
                   ~title:(s_ "Shutdown")
                   ~question:(Printf.sprintf (f_ "Do you want to stop the machine %s?") name)

    let reaction r =
      if (r#get "answer") = "yes"
       then (st#network#get_machine_by_name (r#get "name"))#gracefully_shutdown
       else ()

  end

  module Suspend = struct

    let dynlist =
     fun () -> List.filter
                  (fun x -> (st#network#get_machine_by_name x)#can_suspend)
                  (st#network#get_machine_names)

    let dialog     = Menu_factory.no_dialog
    let reaction r = (st#network#get_machine_by_name (r#get "name"))#suspend

  end

  module Resume = struct

    let dynlist =
     fun () -> List.filter
                  (fun x -> (st#network#get_machine_by_name x)#can_resume)
                  (st#network#get_machine_names)

    let dialog     = Menu_factory.no_dialog
    let reaction r = (st#network#get_machine_by_name (r#get "name"))#resume

  end

  module Ungracefully_stop = struct

    let dynlist =
     fun () -> List.filter
                  (fun x -> (st#network#get_machine_by_name x)#can_poweroff)
                  (st#network#get_machine_names)

    let dialog =
     fun name -> Talking.EDialog.ask_question ~help:None ~cancel:false
                   ~enrich:(mkenv [("name",name)])
                   ~gen_id:"answer"
                   ~title:(s_ "Power-off")
                   ~question:(Printf.sprintf (f_ "WARNING: you asked to brutally stop the machine,\nlike in a power-off;\n however you can also gracefully stop machines.\n\nDo you want to power-off %s anyway?\n") name)

    let reaction r =
      if (r#get "answer") = "yes"
       then (st#network#get_machine_by_name (r#get "name"))#poweroff
       else ()

  end

 module Create_entries_for_MACHINE =
  Gui_toolbar_COMPONENTS_layouts.Layout_for_network_node_with_state (State) (Toolbar_entry) (Add) (Properties) (Remove) (Startup) (Stop) (Suspend) (Resume) (Ungracefully_stop)

end
