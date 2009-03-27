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


(** Toolbar entry for the component 'machine' *)

(* Shortcuts *)
type env  = string Environment.string_env
let mkenv = Environment.make_string_env

module Make_menus (State : sig val st:State.globalState end) = struct

  open State

  module Toolbar_entry = struct
   let imagefile = "ico.machine.palette.png"
   let tooltip   = "Machine"
  end

  module Add = struct
    let key      = Some (GdkKeysyms._M)
    let dialog   = let module M = Gui_dialog_MACHINE.Make (State) in M.dialog ~title:"Ajouter machine" ~update:None

    let reaction r = 
      let details = Network_details_interface.get_network_details_interface () in
      let defects = Defects_interface.get_defects_interface () in
      let (name,eth) = (r#get "name"),(r#get "eth") in
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
      st#network#addMachine m ;
      st#update_sketch ();
      st#update_state  ();
      st#update_cable_sensitivity ();
      Filesystem_history.add_device name ("machine-"^(r#get "distrib")) m#get_variant "machine"

  end

  module Properties = struct

    (* Only not running machines (which can startup) can be modified. *)
    let dynlist =
     fun () -> List.filter
                  (fun x -> (st#network#getMachineByName x)#can_startup)
                  (st#network#getMachineNames)

    let dialog =
     fun name -> let m = (st#network#getMachineByName name) in
                 let title = "Modifier machine" in
                 let module M = Gui_dialog_MACHINE.Make (State) in M.dialog ~title:(title^" "^name) ~update:(Some m)

    let reaction r =
      let details = Network_details_interface.get_network_details_interface () in
      let defects = Defects_interface.get_defects_interface () in
      let (name,oldname,eth) = (r#get "name"),(r#get "oldname"), (int_of_string (r#get "eth")) in
      let m = st#network#getMachineByName oldname in
      m#destroy; (* make sure the simulated object is in state 'no-device' *)
      st#network#changeNodeName oldname name  ;
      m#set_memory      (int_of_string (r#get "memory"))  ;
      m#set_eth_number  eth;
      (* Distribution and variant can not be changed; they are set once and for all at creation time: *)
      (* Instead the kernel can be changed later: *)
      m#set_kernel      (r#get "kernel")  ;
      m#set_terminal    (r#get "term"  )  ;
      st#refresh_sketch () ;
      Filesystem_history.rename_device oldname name;
      details#rename_device oldname name;
      details#update_ports_no name eth;
      defects#rename_device oldname name;
      defects#update_ports_no name eth;
      st#update_cable_sensitivity ()

  end

  module Remove = struct

    (* As for properties, only not running machines can be removed. *)
    let dynlist = Properties.dynlist

    let dialog =
     fun name -> Talking.EDialog.ask_question ~help:None ~cancel:false
                   ~enrich:(mkenv [("name",name)])
                   ~gen_id:"answer"
                   ~title:"Supprimer"
                   ~question:("Confirmez-vous la suppression de "^name^"\net de tous le cables éventuellement branchés à cette machine ?")

    let reaction r =
      let details = Network_details_interface.get_network_details_interface () in
      let defects = Defects_interface.get_defects_interface () in
      if (r#get "answer")="yes" then
        let name   = r#get("name") in
        st#network#delMachine name     ;
        st#update_sketch ()            ;
        st#update_state  ()            ;
        st#update_cable_sensitivity () ;
        details#remove_device name     ;
        defects#remove_device name     ;
      else ()


  end

  module Startup = struct

    let dynlist    = Properties.dynlist
    let dialog     = Menu_factory.no_dialog
    let reaction r = (st#network#getMachineByName (r#get "name"))#startup

  end

  module Stop = struct

    (* For machines "stop" means "shutdown". *)
    let dynlist =
      fun () -> List.filter
                  (fun x -> (st#network#getMachineByName x)#can_gracefully_shutdown)
                  (st#network#getMachineNames)

    let dialog =
     fun name -> Talking.EDialog.ask_question ~help:None ~cancel:false
                   ~enrich:(mkenv [("name",name)])
                   ~gen_id:"answer"
                   ~title:"Shutdown"
                   ~question:("Voulez-vous arrêter la machine "^name^" ?")

    let reaction r =
      if (r#get "answer") = "yes"
       then (st#network#getMachineByName (r#get "name"))#gracefully_shutdown
       else ()

  end

  module Suspend = struct

    let dynlist =
     fun () -> List.filter
                  (fun x -> (st#network#getMachineByName x)#can_suspend)
                  (st#network#getMachineNames)

    let dialog     = Menu_factory.no_dialog
    let reaction r = (st#network#getMachineByName (r#get "name"))#suspend

  end

  module Resume = struct

    let dynlist =
     fun () -> List.filter
                  (fun x -> (st#network#getMachineByName x)#can_resume)
                  (st#network#getMachineNames)

    let dialog     = Menu_factory.no_dialog
    let reaction r = (st#network#getMachineByName (r#get "name"))#resume

  end

  module Ungracefully_stop = struct

    let dynlist =
     fun () -> List.filter
                  (fun x -> (st#network#getMachineByName x)#can_poweroff)
                  (st#network#getMachineNames)

    let dialog =
     fun name -> Talking.EDialog.ask_question ~help:None ~cancel:false
                   ~enrich:(mkenv [("name",name)])
                   ~gen_id:"answer"
                   ~title:"Power off"
                   ~question:("ATTENTION: vous avez demandé un arrêt instantané de la machine,\ntel que provoqué par une interruption du courant électrique. \nLa procédure ordinaire est plutôt celle d'arrêter gracieusement les machines (shutdown). \n\nVoulez-vous tout de même débrancher le courant à "^name^" ?\n")

    let reaction r =
      if (r#get "answer") = "yes"
       then (st#network#getMachineByName (r#get "name"))#poweroff
       else ()

  end

 module Create_entries_for_MACHINE =
  Gui_toolbar_COMPONENTS_layouts.Layout_for_network_node_with_state (State) (Toolbar_entry) (Add) (Properties) (Remove) (Startup) (Stop) (Suspend) (Resume) (Ungracefully_stop)

end
