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


(** Gui completion for both dialog_CABLE widget defined with glade. *)

(* Shortcuts *)
module Str  = StrExtra.Str
module List = ListExtra.List
module Netmodel = Mariokit.Netmodel
let mkenv = Environment.make_string_env

module Make
 (State : sig val st:State.globalState end)
 (Kind  : sig val cablekind:Netmodel.cablekind end)
 = struct

  open State
  open Kind

  let dialog ~title ~(update:Netmodel.cable option) () =

   let dialog = new Gui.dialog_CABLE () in
   dialog#toplevel#set_title title;
   let d = dialog in (* Convenient alias *)
   let module Tk = Gui_dialog_toolkit.Make (struct let toplevel = d#toplevel end) in

   (* Labels *)
   let () = begin
     Tk.Label.set d#label_dialog_CABLE_name     "Nom";
     Tk.Label.set d#label_dialog_CABLE_label    "\nÉtiquette";
     Tk.Label.set d#label_dialog_CABLE_col_from "De";
     Tk.Label.set d#label_dialog_CABLE_col_to   "Vers";
     Tk.Label.set d#label_dialog_CABLE_row_name "Nom";
     Tk.Label.set d#label_dialog_CABLE_row_port "Port";
    end in

   let () = begin
     Tk.Tooltip.set d#label_dialog_CABLE_col_from "Le premier noeud du reseau connecté par le cable";
     Tk.Tooltip.set d#label_dialog_CABLE_col_to   "Le second noeud du reseau connecté par le cable";
     Tk.Tooltip.set d#label_dialog_CABLE_row_name "Le nom du noeud";
     Tk.Tooltip.set d#label_dialog_CABLE_row_port "Le port ethernet RJ45 du noeud";
     (* The following tips disappear when the user choose an entry => deplace them in Widget.ComboTextTree.fromListWithSlaveWithSlaveWithSlave
     Tk.Tooltip.set left#box  "Choisissez le nom du noeud à l'extrémité gauche";
     Tk.Tooltip.set right#box "Choisissez le nom du noeud à l'extrémité droite";
     Tk.Tooltip.set left#slave#box  "Choisissez le port du noeud à l'extrémité gauche";
     Tk.Tooltip.set right#slave#box "Choisissez le port du noeud à l'extrémité droite"; *)
    end in

   (match cablekind with
   | Netmodel.Direct ->
       Tk.Tooltip.set_both d#label_dialog_CABLE_name  d#cable_name  "Le nom du câble droit. Ce nom doit être unique dans le réseau virtuel. Suggestion : D1, D2, ... ";
       Tk.Tooltip.set_both d#label_dialog_CABLE_label d#cable_label "L'étiquette à ajouter au dessin du câble droit.";
       Tk.Tooltip.set_both d#image_dialog_CABLE_direct d#image_dialog_CABLE_direct_link "Cable RJ45 droit";

   | Netmodel.Crossover ->
       (* The cable dialog is defined with glade using a poor man technique of layers. *)
       dialog#image_dialog_CABLE_crossover     #misc#show ();
       dialog#image_dialog_CABLE_crossover_link#misc#show ();
       dialog#image_dialog_CABLE_direct        #misc#hide ();
       dialog#image_dialog_CABLE_direct_link   #misc#hide ();

       Tk.Tooltip.set_both d#label_dialog_CABLE_name  d#cable_name  "Le nom du cable croisé. Ce nom doit être unique dans le réseau virtuel. Suggestion : C1, C2, ... ";
       Tk.Tooltip.set_both d#label_dialog_CABLE_label d#cable_label (Tk.Tooltip.Text.append_label_suggestion_to "L'étiquette à ajouter au dessin du câble croisé.");
       Tk.Tooltip.set_both d#image_dialog_CABLE_crossover d#image_dialog_CABLE_crossover_link "Cable RJ45 croisé";

   | _ -> assert false
   );

   (* Slave dependent function for left and right combo:
      if we are updating we must consider current receptacles as availables! *)
   let freeReceptsOfNode =
     (fun x->
       List.sort
         compare
         (st#network#freeReceptaclesNamesOfNode x Netmodel.Eth)) in

   (* The free receptacles of the given node at the LEFT side of the cable. If we are updating
      we have to consider the old receptacle of the old nodename as available. *)
   let get_left_recept_of = match update with
   | None   -> freeReceptsOfNode
   | Some c -> (fun x->if   (x = c#get_left.Netmodel.nodename)
                       then c#get_left.Netmodel.receptname::(freeReceptsOfNode x)
                       else (freeReceptsOfNode x)) in

   (* The free receptacles of the given node at the RIGHT side of the cable. If we are updating
      we have to consider the old receptacle of the old nodename as available. *)
   let get_right_recept_of = match update with
   | None   -> freeReceptsOfNode
   | Some c -> (fun x->if   (x = c#get_right.Netmodel.nodename)
                       then c#get_right.Netmodel.receptname::(freeReceptsOfNode x)
                       else (freeReceptsOfNode x)) in

    let left = Widget.ComboTextTree.fromListWithSlaveWithSlaveWithSlave
                  ~masterCallback:(Some Log.print_endline)
                  ~masterPacking:(Some (dialog#cable_endpoints#attach ~left:1 ~top:1 ~right:2))
                  st#network#getNodeNames
                  ~slaveCallback:(Some Log.print_endline)
                  ~slavePacking:(Some (dialog#cable_endpoints#attach ~left:1 ~top:2 ~right:2))
                  get_left_recept_of
                  ~slaveSlaveCallback:(Some Log.print_endline)
                  ~slaveSlavePacking:(Some (dialog#cable_endpoints#attach ~left:3 ~top:1 ~right:4))
                  (fun n1 r1 -> st#network#getNodeNames)
                  ~slaveSlaveSlaveCallback:(Some Log.print_endline)
                  ~slaveSlaveSlavePacking:(Some (dialog#cable_endpoints#attach ~left:3 ~top:2 ~right:4))
                  (fun n1 r1 n2 ->
                     let l = (get_right_recept_of n2) in
                     if n1 = n2 then (List.substract l [r1]) else l) in

    let right=left#slave#slave in


   (* Set defaults. If we are updating, defaults are the old values. *)
   begin
   match update with
   | None   ->
               let prefix = if (cablekind=Netmodel.Direct) then "d" else "c" in
               dialog#cable_name#set_text (st#network#suggestedName prefix);
               dialog#cable_name#misc#grab_focus ()
   | Some c -> begin
                dialog#cable_name#set_text        c#get_name                      ;
                dialog#cable_label#set_text       c#get_label                     ;
                left#set_active_value             c#get_left.Netmodel.nodename    ;
                left#slave#set_active_value       c#get_left.Netmodel.receptname  ;
                right#set_active_value            c#get_right.Netmodel.nodename   ;
                right#slave#set_active_value      c#get_right.Netmodel.receptname ;
               end
   end;

   let env_of_dialog () =
     begin
     let n     = dialog#cable_name #text                                                 in
     let l     = dialog#cable_label#text                                                 in
     let (c,o) = match update with None -> ("add","") | Some x -> ("update",x#name)      in
     let ln    = left#selected                                                           in
     let lr    = left#slave#selected                                                     in
     let rn    = right#selected                                                          in
     let rr    = right#slave#selected                                                    in
     if (not (Str.wellFormedName n)) or (ln="") or (lr="") or (rn="") or (rr="")
     then raise Talking.EDialog.IncompleteDialog  else
     if (ln,lr) = (rn,rr)
     then (raise (Talking.EDialog.BadDialog
                    ("Connexion erronée",
                     "Le cable ne peut pas être branché au même endroit des deux côtés!"))) else
     let result = mkenv [("name",n)       ; ("action",c)         ; ("oldname",o)      ; ("label",l) ;
                         ("left_node",ln) ; ("left_recept",lr)   ; ("right_node",rn)  ; ("right_recept",rr)]
     in
     if (ln = rn)
     then (raise (Talking.EDialog.StrangeDialog
                    ("Connexion étrange",
                     "Le cable est branché sur le même composant.\n\
Les machines ont déjà l'interface de loopback pour cela.",result))) else result

     end in

   (* Call the Dialog loop *)
   Tk.dialog_loop ~help:(Some (Talking.Msg.help_cable_insert_update cablekind)) dialog env_of_dialog st

end
