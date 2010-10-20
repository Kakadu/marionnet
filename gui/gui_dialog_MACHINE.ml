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

(** Gui completion for the dialog_MACHINE widget defined with glade. *)

(* Shortcuts *)
let mkenv = Environment.make_string_env

module Make (State:sig val st:State.globalState end) = struct

  open State

  (* User handler for dialog completion. *)
  let dialog ~title ~(update:Mariokit.Netmodel.machine option) () =

   let dialog=new Gui.dialog_MACHINE () in
   dialog#toplevel#set_title title;
   let d = dialog in (* Convenient alias *)
   let module Tk = Gui_dialog_toolkit.Make (struct let toplevel = d#toplevel end) in

   let vm_installations = Disk.get_machine_installations () in

   let kernel = Widget.ComboTextTree.fromList
      ~callback:None
      ~packing:(Some (dialog#table#attach ~left:2 ~top:6 ~right:4))
      (vm_installations#kernels#get_epithet_list)  in

    let distrib = Widget.ComboTextTree.fromListWithSlave
      ~masterCallback:None
      ~masterPacking: (Some (dialog#table#attach ~left:2 ~top:4 ~right:4))
      (* The user can't change filesystem and variant any more once the device has been created:*)
      (match update with
        None -> (vm_installations#filesystems#get_epithet_list)
      | Some m -> [m#get_epithet])
      ~slaveCallback: None
      ~slavePacking:  (Some (dialog#table#attach ~left:2 ~top:5 ~right:4))
      (fun epithet ->
        match update with
          None ->
            "none"::(vm_installations#variants_of epithet)#get_epithet_list
        | Some m ->
            (match m#get_variant with
             | None   -> ["none"]
             | Some x -> [x]
             )
        )  in

    let terminal = Widget.ComboTextTree.fromList
      ~callback:None
      ~packing:(Some (dialog#table#attach ~left:2 ~top:8 ~right:4))
      ((vm_installations#terminal_manager_of "unused epithet")#get_choice_list) in

   (* Labels *)
   let () = begin
     Tk.Label.set d#label_dialog_MACHINE_name     (s_ "Name" ) ;
     Tk.Label.set d#label_dialog_MACHINE_memory   (s_ "Memory<tt>\n(Mb)</tt>" ) ;
     Tk.Label.set d#label_dialog_MACHINE_ethnum   (s_ "Ethernet\ncards" ) ;
     Tk.Label.set d#label_dialog_MACHINE_distrib  (s_ "Distribution" ) ;
     Tk.Label.set d#label_dialog_MACHINE_variant  (s_ "Variant" ) ;
     Tk.Label.set d#label_dialog_MACHINE_kernel   (s_ "kernel" ) ;
     Tk.Label.set d#label_dialog_MACHINE_terminal (s_ "Terminal" ) ;
    end in

   (* Tooltips *)
   let () = begin
     Tk.Tooltip.set d#image_dialog_MACHINE          (s_ "Virtual machine" ) ;
     Tk.Tooltip.set d#image_dialog_MACHINE_hardware (s_ "Hardware specification." ) ;
     Tk.Tooltip.set d#image_dialog_MACHINE_software (s_ "Software specification." ) ;
     Tk.Tooltip.set d#image_dialog_MACHINE_uml      (s_ "User Mode Linux (UML) graphical interface" ) ;
     Tk.Tooltip.set_both d#label_dialog_MACHINE_name     d#name       (s_ "Virtual machine name. This name must be unique in the virtual network." ) ;
     Tk.Tooltip.set_both d#label_dialog_MACHINE_memory   d#memory     (s_ "Amount of RAM to be reserved for this machine." ) ;
     Tk.Tooltip.set_both d#label_dialog_MACHINE_ethnum   d#eth        (s_ "Number of ethernet cards (eth0, eth1 ...) of the virtual machine" ) ;
     Tk.Tooltip.set_both d#label_dialog_MACHINE_distrib  distrib#box  (s_ "GNU/Linux distribution installed on the virtual machine." ) ;
     Tk.Tooltip.set_both d#label_dialog_MACHINE_variant  distrib#slave#box (s_ "Initial hard disk state. The virtual machine will start by default with this variant of the chosen distribution." ) ;
     Tk.Tooltip.set_both d#label_dialog_MACHINE_kernel   kernel#box   (s_ "Linux kernel version used for this virtual machine." ) ;
     Tk.Tooltip.set_both d#label_dialog_MACHINE_terminal terminal#box (s_ "Type of terminal to use to control the virtual machine. Possible choices are: X HOST terminal (providing the possibility to launch graphical applications on the host X server) and X NEST (an independent graphic server displaying all the X windows of a virtual machines)." ) ;
    end in

   (* Set defaults. If we are updating, defaults are the old values. *)
   begin
   terminal#box#misc#set_sensitive false;  (* en attendant de l'implementer ou de l'eliminer... *)
   match update with
   | None   ->
	distrib#set_active_value "default";
	distrib#slave#set_active_value "none";
	kernel#set_active_value  "default"; (* !!!!!!!!!!! *)
	dialog#name#set_text (st#network#suggestedName "m");
	dialog#name#misc#grab_focus ()

   | Some m ->
       begin
	dialog#name#set_text    m#get_name;
	dialog#memory#set_value (float_of_int m#get_memory);
	dialog#eth#set_value    (float_of_int m#get_port_no);

	(* The user cannot remove receptacles used by a cable. *)
	let min_eth = (st#network#max_busy_port_index_of_node (m :> Mariokit.Netmodel.node))+1 in
	dialog#eth#adjustment#set_bounds ~lower:(float_of_int (max min_eth 1)) ()  ;

	distrib#set_active_value m#get_epithet;
	(match m#get_variant with
	 | None         -> distrib#slave#box#misc#set_sensitive false
	 | Some variant -> distrib#slave#set_active_value variant
	 );
	distrib#box#misc#set_sensitive       false;
	distrib#slave#box#misc#set_sensitive false;
	kernel#set_active_value        m#get_kernel;
	terminal#set_active_value      m#get_terminal;
       end
    end;

   (* Parse the widget in order to generate the environment. *)
   let env_of_dialog () =
     begin
     let n     = dialog#name#text in
     let (c,o) = match update with None -> ("add","") | Some m -> ("update",m#name) in
     let m     = (string_of_int dialog#memory#value_as_int) in
     let e     = (string_of_int dialog#eth#value_as_int) in
     let d     = distrib#selected in
     let v     = distrib#slave#selected in
     let k     = kernel#selected in
     let t     = terminal#selected in
     if not (StrExtra.wellFormedName n) then raise Talking.EDialog.IncompleteDialog  else
     let result =
       mkenv [("name",n) ; ("action",c)  ; ("oldname",o) ; ("memory",m) ; ("eth",e) ;
              ("distrib",d);
              ("kernel",k) ;
              ("term",t)  ]
     in
     let () = match v with
      | "none" -> ()
      | _      ->
         let variant_realpath =
           (vm_installations#variants_of d)#realpath_of_epithet v
         in
         result#add ("variant_name", v);
         result#add ("variant_realpath", variant_realpath);
     in
     result

     end in

   (* Call the dialog loop *)
   Tk.dialog_loop ~help:(Some Talking.Msg.help_machine_insert_update) dialog env_of_dialog st

end




