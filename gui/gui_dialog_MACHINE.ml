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


(** Gui completion for the dialog_MACHINE widget defined with glade. *)

(* Shortcuts *)
module Str = StrExtra.Str
let mkenv = Environment.make_string_env

module Make (State:sig val st:State.globalState end) = struct

  open State

  (* User handler for dialog completion. *)
  let dialog ~title ~(update:Mariokit.Netmodel.machine option) () =

   let dialog=new Gui.dialog_MACHINE () in
   dialog#toplevel#set_title title;
   let d = dialog in (* Convenient alias *)
   let module Tk = Gui_dialog_toolkit.Make (struct let toplevel = d#toplevel end) in

   let kernel = Widget.ComboTextTree.fromList
      ~callback:(Some Log.print_endline)
      ~packing:(Some (dialog#table#attach ~left:2 ~top:6 ~right:4))
      (Mariokit.MSys.kernelList ())  in

    let distrib = Widget.ComboTextTree.fromListWithSlave
      ~masterCallback:(Some Log.print_endline)
      ~masterPacking: (Some (dialog#table#attach ~left:2 ~top:4 ~right:4))
      (* The user can't change filesystem and variant any more once the device has been created:*)
      (match update with
        None -> (Mariokit.MSys.machine_filesystem_list ())
      | Some m -> [m#get_distrib])
      ~slaveCallback: (Some Log.print_endline)
      ~slavePacking:  (Some (dialog#table#attach ~left:2 ~top:5 ~right:4))
      (fun unprefixed_filesystem ->
        match update with
          None ->
            Mariokit.MSys.variant_list_of ("machine-" ^ unprefixed_filesystem) ()
        | Some m -> [m#get_variant])  in

    let terminal = Widget.ComboTextTree.fromList
      ~callback:(Some Log.print_endline)
      ~packing:(Some (dialog#table#attach ~left:2 ~top:8 ~right:4))
      Mariokit.MSys.termList in

   (* Labels *)
   let () = begin
     Tk.Label.set d#label_dialog_MACHINE_name     "Nom" ;
     Tk.Label.set d#label_dialog_MACHINE_memory   "Mémoire<tt>\n(Mb)</tt>" ;
     Tk.Label.set d#label_dialog_MACHINE_ethnum   "Cartes\nEthernet" ;
     Tk.Label.set d#label_dialog_MACHINE_distrib  "Distribution" ;
     Tk.Label.set d#label_dialog_MACHINE_variant  "Variante" ;
     Tk.Label.set d#label_dialog_MACHINE_kernel   "Noyau" ;
     Tk.Label.set d#label_dialog_MACHINE_terminal "Terminal" ;
    end in

   (* Tooltips *)
   let () = begin
     Tk.Tooltip.set d#image_dialog_MACHINE          "Machine virtuelle" ;
     Tk.Tooltip.set d#image_dialog_MACHINE_hardware "Spécifications matérielles." ;
     Tk.Tooltip.set d#image_dialog_MACHINE_software "Spécifications logicielles." ;
     Tk.Tooltip.set d#image_dialog_MACHINE_uml      "Spécifications du mode de fonctionnement User Mode Linux (UML)." ;
     Tk.Tooltip.set_both d#label_dialog_MACHINE_name     d#name       "Le nom de la machine virtuelle. Ce nom doit être unique dans le réseau virtuel." ;
     Tk.Tooltip.set_both d#label_dialog_MACHINE_memory   d#memory     "Quantité de mémoire vive (RAM) à reserver pour l'exécution de cette machine virtuelle." ;
     Tk.Tooltip.set_both d#label_dialog_MACHINE_ethnum   d#eth        "Nombre de cartes ethernet (eth0, eth1,...) de la machine virtuelle" ;
     Tk.Tooltip.set_both d#label_dialog_MACHINE_distrib  distrib#box  "Distribution GNU/Linux installée sur la machine virtuelle." ;
     Tk.Tooltip.set_both d#label_dialog_MACHINE_variant  distrib#slave#box "État initial du disque dur. La machine virtuelle sera démarrée par défaut dans cette variante de la distribution choisie." ;
     Tk.Tooltip.set_both d#label_dialog_MACHINE_kernel   kernel#box   "Version du noyau Linux à utiliser pour cette machine virtuelle." ;
     Tk.Tooltip.set_both d#label_dialog_MACHINE_terminal terminal#box "Type de terminal à utiliser pour contrôler la machine virtuelle. Les choix possibles sont : (X HOST) terminal avec possibilité de lancer des applications graphiques sur le serveur de l'hôte, et (X NEST) serveur graphique indépendant, regroupant toutes les fenêtres de la machine virtuelle." ;
    end in

   (* Set defaults. If we are updating, defaults are the old values. *)
   begin
   match update with
   | None   -> distrib#set_active_value "default";
               dialog#name#set_text (st#network#suggestedName "m");
               dialog#name#misc#grab_focus ()

   | Some m -> begin
                dialog#name#set_text           m#get_name                         ;
                dialog#memory#set_value        (float_of_int m#get_memory)        ;
                dialog#eth#set_value           (float_of_int m#get_eth_number)    ;

                (* The user cannot remove receptacles used by a cable. *)
                let min_eth = (st#network#maxBusyReceptacleIndex m#get_name Mariokit.Netmodel.Eth)+1 in
                Log.print_endline ("gui_dialog_MACHINE.cmo: defaults: min_eth = "^(string_of_int min_eth)) ;
                dialog#eth#adjustment#set_bounds ~lower:(float_of_int (max min_eth 1)) ()  ;

                distrib#set_active_value       m#get_distrib  ;
                distrib#slave#set_active_value m#get_variant  ;
                distrib#box#misc#set_sensitive       false    ;
                distrib#slave#box#misc#set_sensitive false    ;
                kernel#set_active_value        m#get_kernel   ;
                terminal#set_active_value      m#get_terminal ;
               end
   end;

   (* Parse the widget in order to generate the environment. *)
   let env_of_dialog () =
     begin
     let n     = dialog#name#text                                                   in
     let (c,o) = match update with None -> ("add","") | Some m -> ("update",m#name) in
     let m     = (string_of_int dialog#memory#value_as_int)                         in
     let e     = (string_of_int dialog#eth#value_as_int)                            in
     let d     = distrib#selected                                                   in
     let p     = distrib#slave#selected                                             in
     let k     = kernel#selected                                                    in
     let t     = terminal#selected                                                  in
     if not (Str.wellFormedName n) then raise Talking.EDialog.IncompleteDialog  else
     mkenv [("name",n) ; ("action",c)  ; ("oldname",o) ; ("memory",m) ; ("eth",e) ;
            ("distrib",d) ; ("patch",p)   ; ("kernel",k) ; ("term",t)  ]
     end in

   (* Call the dialog loop *)
   Tk.dialog_loop ~help:(Some Talking.Msg.help_machine_insert_update) dialog env_of_dialog st

end




