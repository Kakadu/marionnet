(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007  Jean-Vincent Loddo
   Copyright (C) 2007, 2008  Luca Saiu

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

(** All dialogs are implemented here. This module provide the capability for user to talk with the application. 
    Specifically, the name "Talking" stands here for "Talking with user". *)

open PreludeExtra.Prelude;; (* We want synchronous terminal output *)
open Simple_dialogs;;
open Sugar;;
open StringExtra;;
open StrExtra;;
open ListExtra;;
open FilenameExtra;;
open UnixExtra;;
open StrExtra;;
open Environment;;
open Mariokit;;
open State;;
open Simulated_network;;
open Network_details_interface;;
open Defects_interface;;

let install_signal_handler signal =
  Sys.set_signal
    signal
    (Sys.Signal_handle
       (fun _ ->
       Printf.printf "\n\n\nWe got a signal. Now we're gonna exit like a good-mannered\n";
       Printf.printf "polite process which always calls exit(EXIT_SUCCESS).\n\n\n";
         flush_all ();
         exit 0));;

let commit_suicide signal =
  raise Exit;;
  (* let my_pid = Unix.getpid () in *)
  (* try *)
  (*   (\* Be sure that every write operation is synced before committing suicide: *\) *)
  (*   flush_all (); *)
  (*   Printf.printf "!! Sending the signal %i to the Marionnet process...\n" signal; flush_all (); *)
  (*   Unix.kill my_pid Sys.sigkill; *)
  (* with _ -> begin *)
  (*   Printf.printf "!! Sending the signal %i to the Marionnet process failed. Mmm. Very strange.\n" signal; *)
  (*   flush_all (); *)
  (* end;; *)

let make_names_and_thunks st verb what_to_do_with_a_node =
  List.map
    (fun node -> (verb ^ " " ^ node#get_name,
                  fun () ->
                    let progress_bar =
                      make_progress_bar_dialog
                        ~title:(verb ^ " " ^ node#get_name)
                        ~text_on_bar:"Patientez s'il vous plaît..." () in
                    (try
                      what_to_do_with_a_node node;
                    with e ->
                      Printf.printf "Warning (q): \"%s %s\" raised an exception (%s)\n"
                        verb
                        node#name
                        (Printexc.to_string e));
                    flush_all ();
                    destroy_progress_bar_dialog progress_bar))
    st#network#nodes;;

let do_something_with_every_node_in_sequence st verb what_to_do_with_a_node =
  List.iter
    (fun (name, thunk) ->
        Task_runner.the_task_runner#schedule ~name thunk)
    (make_names_and_thunks st verb what_to_do_with_a_node);;

let do_something_with_every_node_in_parallel st verb what_to_do_with_a_node =
  Task_runner.the_task_runner#schedule_parallel
    (make_names_and_thunks st verb what_to_do_with_a_node);;

let startup_everything st () =
  do_something_with_every_node_in_sequence
    st "Startup"
    (fun node -> node#startup_right_now);;
let shutdown_everything st () =
  do_something_with_every_node_in_parallel
    st "Shut down"
    (fun node -> node#gracefully_shutdown_right_now);;
let poweroff_everything st () =
  do_something_with_every_node_in_sequence
    st "Power-off"
    (fun node -> node#poweroff_right_now);;

(** Return true iff there is some node on or sleeping *)
let is_there_something_on_or_sleeping st () =
  let result = 
    List.exists
      (fun node -> node#can_gracefully_shutdown or node#can_resume)
      st#network#nodes
  in
  print_string ("Is there something running? " ^ (if result then "yes" else "no") ^ "\n");
  result;;


(* **************************************** *
              Module MSG
 * **************************************** *)


(** Some tools for building simple help, error, warning and info dialogs *)
module Msg = struct

 (** I moved some stuff into simple_dialogs.ml. It's useful for lots of other
     modules, not only for talking. --L. *)

 (** Specific help constructors*) 

 (** Why you have to choose a folder to work *)
 let help_repertoire_de_travail = 
   let title = "CHOISIR UN RÉPERTOIRE DE TRAVAIL" in
   let msg   = "Marionnet utilise un répertoire au choix \
pour chaque séance de travail. Tous les fichiers créés par Marionnet dans le répertoire de travail seront effacés \
à la sortie du programme. \
Si le logiciel est executé à partir du DVD Marionnet, il est conseillé d'utiliser un répertoire persistant \
(dans /mnt/hd*) pour ne pas occuper la memoire vive inutilement." in help title msg ;;

let help_machine_insert_update = 
   let title = "AJOUTER OU MODIFIER UNE MACHINE VIRTUELLE" in
   let msg   = "\
Dans cette fenêtre de dialogue vous pouvez définir le nom de \
la machine virtuelle et régler plusieur paramètres matériels \
et logiciels.

SECTION 'Hardware'

- Mémoire : quantité de mémoire vive (RAM) que le système \
doit réserver pour cette machine virtuelle (48Mo par défaut)

- Cartes Ethernet : nombre de cartes Ethernet (1 par défaut)

SECTION 'Software' :

- Distribution : la distribution GNU/Linux (Debian, Mandriva, Gentoo,..), à \
choisir parmi les disponibles dans le répertoire /usr/marionnet/filesystems

- Variante : une variante (ou patch) de la distribution choisie; une variante \
est un fichier COW (Copy On Write) qui représente une petite mise à jour da la \
distribution concernée. Les variantes sont à choisir parmi les disponibles dans \
les répertoires /usr/marionnet/filesystems/*_variants/. Vous pouvez créer vos \
propres variantes en exportant n'importe quel état de machine virtuelle dans l'onglet \
'Disques'

- Noyau : la version du noyau Linux, à choisir parmi les disponibles dans le \
répertoire /usr/marionnet/kernels

SECTION 'UML' : 

- Terminal : les choix possibles sont ici 'X HOST' et 'X NEST'; le premier \
choix permet de lancer des applications graphiques à partir d'un terminal textuel \
où l'utilisateur pourra prendre les commandes de la machine virtuelle \
(avec le login 'root' et mot de passe 'root'); le choix 'X NEST' permet \
d'avoir un véritable serveur graphique réservé à la machine virtuelle, avec \
des gestionnaire de fenêtres et de bureaux indépendants.\
" in help title msg ;;


 let help_hub_insert_update = 
   let title = "AJOUTER OU MODIFIER UN RÉPÉTEUR (HUB)" in
   let msg   = "\
Dans cette fenêtre de dialogue vous pouvez définir le nom d'un \
répetéur de trames Ethernet (hub) et régler plusieurs paramètres :

- Étiquette : chaîne de caractères utilisée pour décorer l'icône du \
répéteur dans l'image du réseau; ceci permet de savoir en un clin d'oeil \
le réseau Ethernet (par exemple '192.168.1.0/24') que le composant réalise \
ou doit réaliser; ce champ est utilisé exclusivement à des fins graphiques, il \
n'est pas pris en compte à des fins de configuration

- Nb de Ports : le nombre de ports du répéteur (4 par défaut); ce nombre ne doit \
pas être inutilement grand, le nombre de processus nécessaires à l'émulation de ce \
composant étant proportionnel au nombre de ses ports.
" in help title msg ;;

 let help_switch_insert_update = 
   let title = "AJOUTER OU MODIFIER UN COMMUTATEUR (SWITCH)" in
   let msg   = "\
Dans cette fenêtre de dialogue vous pouvez définir le nom d'un \
commutateur de trames Ethernet (switch) et régler plusieurs paramètres :

- Étiquette : chaîne de caractères utilisée pour décorer l'icône du \
commutateur dans l'image du réseau; ceci permet de savoir en un clin d'oeil \
le réseau Ethernet (par exemple '192.168.1.0/24') que le composant réalise \
ou doit réaliser; ce champ est utilisé exclusivement à des fins graphiques, il \
n'est pas pris en compte à des fins de configuration

- Nb de Ports : le nombre de ports du commutateur (4 par défaut); ce nombre ne doit \
pas être inutilement grand, le nombre de processus nécessaires à l'émulation de ce \
composant étant proportionnel au nombre de ses ports.
" in help title msg ;;

 let help_router_insert_update = 
   let title = "AJOUTER OU MODIFIER UN ROUTEUR" in
   let msg   = "\
Dans cette fenêtre de dialogue vous pouvez définir le nom d'un \
routeur de paquets IP (router) et régler plusieurs paramètres :

- Étiquette : chaîne de caractères utilisée pour décorer l'icône du \
routeur dans l'image du réseau; ce champ est utilisé exclusivement à \
des fins graphiques, il n'est pas pris en compte à des fins de configuration

- Nb de Ports : le nombre de ports du routeur (4 par défaut); ce nombre ne doit \
pas être inutilement grand, le nombre de processus nécessaires à l'émulation de ce \
composant étant proportionnel au nombre de ses ports.

L'émulation de ce composant est réalisée avec le logiciel 'quagga' dérivé du \
projet 'zebra'.

Chaque interface du routeur peut être configurée depuis \
l'onglet 'Interfaces'. Une fois démarré, le routeur répondra \
au protocole telnet sur toutes les interfaces configurées, sur les \
ports tcp suivants :

zebra\t\t2601/tcp\t\t# zebra vty
ripd\t\t\t2602/tcp\t\t# RIPd vty
ripngd\t\t2603/tcp\t\t# RIPngd vty
ospfd\t\t2604/tcp\t\t# OSPFd vty
bgpd\t\t2605/tcp\t\t# BGPd vty
ospf6d\t\t2606/tcp\t\t# OSPF6d vty
isisd\t\t2608/tcp\t\t# ISISd vty

Mot de passe : zebra\
" in help title msg ;;

 let help_device_insert_update = function
  | Netmodel.Hub    -> help_hub_insert_update
  | Netmodel.Switch -> help_switch_insert_update
  | Netmodel.Router -> help_router_insert_update
  | _ -> ignore
 ;;

 let help_cable_direct_insert_update = 
   let title = "AJOUTER OU MODIFIER UN CABLE DROIT" in
   let msg   = "\
Dans cette fenêtre de dialogue vous pouvez définir le nom d'un \
cable Ethernet droit et régler les paramètres suivants :

- Étiquette : chaîne de caractères utilisée pour décorer l'arc \
représentant le cable dans l'image du réseau

- Extrémités : les deux composants réseaux (machine, répéteur,..) reliés par le \
cable et les deux interfaces impliquées  \
(à choisir parmi les disponibles, c'est-à-dire parmi les existantes non occupées \
par d'autres cables)

ATTENTION : ce dialogue permet de définir des cables droits \
même lorsque ils ne pourront pas fonctionner (par exemple, entre \
deux machines); la possibilité de définir un cablage incorrect \
n'est pas lié à des raisons techniques mais porte un intérêt \
exclusivement pédagogique." in help title msg ;;

 let help_cable_crossed_insert_update = 
   let title = "AJOUTER OU MODIFIER UN CABLE CROISÉ" in
   let msg   = "\
Dans cette fenêtre de dialogue vous pouvez définir le nom d'un \
cable Ethernet croisé et régler les paramètres suivants :

- Étiquette : chaîne de caractères utilisée pour décorer l'arc \
représentant le cable dans l'image du réseau

- Extrémités : les deux composants réseaux (machine, répéteur,..) reliés par le \
cable et les deux interfaces impliquées  \
(à choisir parmi les disponibles, c'est-à-dire parmi les existantes non occupées \
par d'autres cables)

ATTENTION : ce dialogue permet de définir des cables croisé \
même lorsque ils ne pourront pas fonctionner (par exemple, entre \
une machine et un commutateur); la possibilité de définir un cablage incorrect \
n'est pas lié à des raisons techniques mais porte un intérêt \
exclusivement pédagogique." in help title msg ;;

 let help_cable_insert_update = function
  | Netmodel.Direct  -> help_cable_direct_insert_update
  | Netmodel.Crossed -> help_cable_crossed_insert_update
  | _ -> ignore
 ;;

 let help_cloud_insert_update = 
   let title = "AJOUTER OU MODIFIER UN NUAGE" in
   let msg   = "\
Dans cette fenêtre de dialogue vous pouvez définir le nom d'un \
'nuage'. Ce composant est un réseau Ethernet, dont la structure interne \
n'est pas connue, mais qui provoque des rétards ou d'autres anomalies lors d'un \
passage de trames entre ses deux extrémités.

Une fois le nuage défini, utilisez l'onglet 'Anomalies' pour régler les retards, \
les pertes de trames et les autres anomalies que vous souhaitez provoquer entre les deux \
extrémités du nuage." in help title msg ;;

 let help_socket_insert_update = 
   let title = "AJOUTER OU MODIFIER UNE PRISE ETHERNET" in
   let msg   = "\
Dans cette fenêtre de dialogue vous pouvez définir le nom d'une \
prise Ethernet. Ce composant permet de relier le réseau virtuel avec \
un réseau Ethernet (réel) duquel fait partie le système hôte, c'est-à-dire \
le système GNU/Linux qui exécute l'ensemble des composants virtuels. 

Le réseau réel est choisi de la façon suivante:

(1) si l'hôte a une route par défaut, le réseau est celui de la passerelle;

(2) sinon, le réseau est la réunion (bridge Linux) des réseaux de toutes les \
interfaces réseau actives de l'hôte.

La prise permet aux machines virtuelles d'accéder aux mêmes \
services réseau (DHCP, DNS, NFS,..) que l'hôte peut atteindre, \
par la passerelle, dans le cas n°1, ou par une de ses interfaces, \
dans le cas n°2.

Dans la plupart des cas, l'hôte a une route par défaut et peut atteindre \
le réseau Internet par cette passerelle. Dans ce cas, les machines \
virtuelles pourrons, elles aussi, accéder au réseau Internet. \
Ceci permet, par exemple, d'installer des programmes manquants sur \
les machines virtuelles, par une simple séquence de commandes:

$ dhclient eth0
$ apt-get install mon-programme-fétiche
 
La prise permet aussi de travailler en groupe dans une salle réseau \
en faisant communiquer des instances de Marionnet s'exécutant \
sur différents postes." in help title msg ;;

 let error_saving_while_something_up = 
  Simple_dialogs.error
   "Sauvegarde"
  "Le projet ne peut être enregistré maintenant. \
Un ou plusieurs composants réseau sont en cours d'exécution. \
S'il vous plaît arrêtez-les avant d'enregistrer."
 ;;

 (** Why you have to choose a name for your project *)
 let help_nom_pour_le_projet = 
   let title = "CHOISIR UN NOM POUR LE PROJET" in
   let msg   = "Marionnet regroupe tous les fichiers concernant un même projet dans un fichier dont l'extension \
standard est .mar. Il s'agit en réalité d'un fichier de type tar compressé (gzip) qui peut donc être ouvert \
indépendamment de ce logiciel." in help title msg ;;
end;; (* module Msg *)

(** Return the given pathname as it is, if it doesn't contain funny characters
    we don't want to bother supporting, like ' ', otherwise raise an exception.
    No check is performed on the pathname actual existence or permissions: *)
let check_pathname_validity pathname =
  if Str.Bool.match_string "^[a-zA-Z0-9_\\/\\-]+$" pathname then
    pathname
  else
    failwith "The pathname "^ pathname ^" contains funny characters, and we don't support it";;

(** Check that the given pathname is acceptable, and that it has extension ".mar" or
    no extension; if the argument has extension ".mar" then just return it; it it's
    otherwise valid but has no extension then return the argument with ".mar" appended;
    if it's invalid or has an extension different from ".mar" then show an appropriate
    error message and raise an exception. 
    This function is thought as a 'filter' thru which user-supplied filenames should
    be always sent before use *)
let check_project_path_name_validity_and_add_extension_if_needed path_name =
  let directory = Filename.dirname path_name in
  let directory =
    try
      check_pathname_validity directory
    with _ -> begin
      Simple_dialogs.error
        "Nom de répertoire incorrect" 
        ((Printf.sprintf "Le nom \"%s\" n'est pas un nom de répertoire valide." directory)^
         "\n\nLes noms de répertoires peuvent contenir seulement des lettres, chiffres, tirets et tirets bas.")
        ();
      failwith "the given directory name is invalid";      
    end in
  let path_name = Filename.basename path_name in
  let check_project_chopped_basename_validity chopped_basename =
    if Str.wellFormedName ~allow_dash:true chopped_basename then
      chopped_basename
    else begin
      Simple_dialogs.error
        "Nom de fichier incorrect" 
        ((Printf.sprintf "Le nom \"%s\" n'est pas un nom de fichier valide." chopped_basename)^
         "\n\nUn nom de fichier valide doit commencer par une lettre et peut contenir des lettres, chiffres, tirets et tirets bas.")
        ();
      failwith "the given file name is invalid";
    end in
  if Filename.check_suffix path_name ".mar" then
    (* path_name does end with ".mar"; just check that its chopped version is ok: *)
    Printf.sprintf
      "%s/%s.mar"
      directory
      (check_project_chopped_basename_validity (Filename.chop_extension path_name))
  else
    (* path_name doesn't end with ".mar": *)
    try
      let _ = Filename.chop_extension path_name in
      (* There is an extension but it's not ".mar"; fail: *)
      Simple_dialogs.error
        "Extension de fichier non admise"
        (Printf.sprintf
           "Le fichier \"%s\" doit avoir une extension \".mar\", ou aucune (dans ce cas l'extension \".mar\" sera automatiquement ajoutée)."
           path_name)
        ();
      failwith "the given file name has an extension but it's not \".mar\".";
    with Invalid_argument _ ->
      (* There is no extension; just check that the filename is otherwise valid, and
         add the extension: *)
      Printf.sprintf
        "%s/%s.mar"
        directory
        (check_project_chopped_basename_validity path_name);;


(* **************************************** *
              Module EDialog 
 * **************************************** *)


(** An EDialog (for Environnemnt Dialog) is a dialog which may returns an environnement in the
    form (id,value) suitable for functions implementing reactions *)
module EDialog = struct

(** An edialog is a dialog which returns an env as result if succeed *)
type edialog = unit -> (((string,string) env) option) ;;

(** Dialog related exceptions. *)
exception BadDialog     of string * string;;
exception StrangeDialog of string * string * ((string,string) env);;
exception IncompleteDialog;;

(** The (and) composition of edialogs is again an env option *)
let rec compose (dl:edialog list) () : ((('a,'b) env) option) =
  match dl with
  | []  -> raise (Failure "EDialog.compose") 
  | [d] -> d ()
  | d::l -> (match d () with
             | None   -> None
             | Some r -> (match (compose l ()) with 
                          | None   -> None
                          | Some z -> (Some (r#updatedBy z))
                          )
             )
;;

(** Alias for edialog composition *) 
let sequence = compose;;

(** Auxiliary functions for file/folder chooser dialogs *)

let default d = function | None -> d | Some v -> v
;;

(** To do: Spostare in ListExtra? *)
let is_string_prefix s1 s2 =
  let l1 = String.length s1 in
  let l2 = String.length s2 in
  l1 <= l2 && s1 = String.sub s2 0 l1
;;

(** Filters  *)

let image_filter () =
  let f = GFile.filter ~name:"Images" () in
  f#add_custom [ `MIME_TYPE ]
    (fun info ->
      let mime = List.assoc `MIME_TYPE info in
      is_string_prefix "image/" mime) ;
  f
;;

let all_files     () = let f = GFile.filter ~name:"All" () in f#add_pattern "*" ; f ;;
let script_filter () = GFile.filter ~name:"Scripts Shell/Python (*.sh *.py)"  ~patterns:[ "*.sh"; "*.py" ] () ;;
let mar_filter    () = GFile.filter ~name:"Marionnet projects (*.mar)" ~patterns:[ "*.mar"; ] () ;;
let xml_filter    () = GFile.filter ~name:"XML files (*.xml)" ~patterns:[ "*.xml"; "*.XML" ] () ;;
let jpeg_filter   () = GFile.filter ~name:"JPEG files (*.jpg *.jpeg)" ~patterns:[ "*.jpg"; "*.JPG"; "*.jpeg"; "*.JPEG" ] () let png_filter    () = GFile.filter ~name:"PNG files (*.png)" ~patterns:[ "*.png"; "*.PNG" ] () ;;

(** Filters for Marionnet *)
type marfilter = MAR | ALL | IMG | SCRIPT | XML | JPEG | PNG ;;

(** The kit of all defined filters *)
let allfilters = [ ALL ; MAR ; IMG ; SCRIPT ; XML ; JPEG ]
;;

let fun_filter_of = function
  | MAR    -> mar_filter    ()
  | IMG    -> image_filter  ()
  | SCRIPT -> script_filter ()
  | XML    -> xml_filter    ()
  | JPEG   -> jpeg_filter   ()
  | PNG    -> png_filter    ()
  | ALL    -> all_files     ()
;;

(** The edialog asking for file or folder. It returns a simple environment with an unique identifier
    [gen_id] bound to the selected name *)
let ask_for_file 

    ?(title="FILE SELECTION") 
    ?(valid:(string->bool)=(fun x->true)) 
    ?(filters = allfilters) 
    ?(action=`OPEN) 
    ?(gen_id="filename") 
    ?(help=None)() 
    =

  let dialog = GWindow.file_chooser_dialog 
      ~icon:Icon.icon_pixbuf
      ~action:action 
      ~title:(utf8 title)
      ~modal:true () in

  dialog#unselect_all ;
  if (help=None) then () else dialog#add_button_stock `HELP `HELP ; 
  dialog#add_button_stock `CANCEL `CANCEL ;
  dialog#add_button_stock `OK `OK;

  if (action=`SELECT_FOLDER)        then (try (dialog#add_shortcut_folder "/tmp") with _ -> ());
  if (action=`OPEN or action=`SAVE) then (List.iter (fun x -> dialog#add_filter (fun_filter_of x)) filters); 
  let result = (ref None) in
  let cont   = ref true in
  while (!cont = true) do  
  begin match dialog#run () with
  | `OK -> (match dialog#filename with
              | None   -> () 
              | Some fname -> if (valid fname) then 
                              begin cont := false; result := (Some (mkenv [(gen_id,fname)])) end
              )
  | `HELP -> (match help with 
              | Some f -> f ();
              | None -> ()
             )
  |  _ -> cont := false
  end 
  done;

  dialog#destroy ();
  !result
;;

(** Return true iff the the given directory exists and is on a filesystem supporting
    sparse files. This function doesn't check whether the directory is writable: *)
let does_directory_support_sparse_files pathname =
  (* All the intelligence of this method lies in the external script: *)
  try
    let command_line =
      Printf.sprintf
        "marionnet-can-directory-host-sparse-files '%s'"
        (check_pathname_validity pathname) in
    match Unix.system command_line with
      Unix.WEXITED 0 ->
        true
    | Unix.WEXITED _ | _ ->
        false
  with _ ->
    false;;

(** The edialog asking for an existing and writable directory. *)
let ask_for_existing_writable_folder_pathname_supporting_sparse_files ~title ?(help=None) =
  let valid = fun pathname -> 
    if (not (Sys.file_exists pathname)) or
       (not (Shell.dir_comfortable pathname)) or
       (not (does_directory_support_sparse_files pathname)) then begin
      Simple_dialogs.error 
         "Répertoire inexploitable"
         "Vous devez choisir un répertoire existant, modifiable et résidant sur un système de fichiers supportant les fichiers 'sparse' (ext2, ext3, reiserfs, NTFS, ...)"
         ();
      false;
    end else
      true in
  ask_for_file ~title ~valid ~filters:[] ~action:`SELECT_FOLDER ~gen_id:"foldername" ~help;;


(** The edialog asking for a fresh and writable filename. *)
let ask_for_fresh_writable_filename ~title ?(filters = allfilters) ?(help=None) =

  let valid = fun x -> 
    if (Sys.file_exists x) 
    then ((Simple_dialogs.error "Choix du nom" "Un fichier de même nom existe déjà!\n\nVous devez choisir un nouveau nom pour votre fichier." ()); false)
    else (prerr_endline ("valid: x="^x) ; (Shell.freshname_possible x)) in

  let result =
    ask_for_file ~title ~valid ~filters ~action:`SAVE ~gen_id:"filename" ~help in
  result;;

(** The edialog asking for an existing filename. *)
let ask_for_existing_filename ~title ?(filters = allfilters) ?(help=None) =

  let valid = fun x -> 
    if not (Sys.file_exists x) 
    then ((Simple_dialogs.error "Choix du fichier" "Le fichier n'existe pas!\nVous devez choisir un nom de fichier existant." ()); false)
    else (Shell.regfile_modifiable x) in

  ask_for_file ~title ~valid ~filters ~action:`OPEN ~gen_id:"filename" ~help 
;;

(** Generic constructor for question dialogs. *)
let ask_question ?(title="QUESTION")  ?(gen_id="answer")  ?(help=None) ?(cancel=false) ~(question:string)  () =
 
   let dialog=new Gui.dialog_QUESTION () in 

   if (help=None)    then () else dialog#toplevel#add_button_stock `HELP   `HELP ; 
   if (cancel=false) then () else dialog#toplevel#add_button_stock `CANCEL `CANCEL ; 

   dialog#toplevel#set_title (utf8 title);
(*   dialog#title#set_text     (utf8 (String.uppercase question));*)
   dialog#title#set_text     (utf8 question);
   dialog#title#set_use_markup true; 
   ignore
     (dialog#toplevel#event#connect#delete
        ~callback:(fun _ -> print_string "Sorry, no, you can't close the dialog. Please make a decision.\n"; true));
   
   let result = (ref None) in
   let cont   = ref true in
   while (!cont = true) do  
     match dialog#toplevel#run () with
     | `YES  -> begin cont := false; result := (Some (mkenv [(gen_id,"yes")])) end
     | `NO   -> begin cont := false; result := (Some (mkenv [(gen_id,"no" )])) end
     | `HELP -> (match help with 
                 | Some f -> f ();
                 | None -> ()
             )
     | `CANCEL when cancel -> begin
         cont := false;
         result := None
       end
     | _ ->
       cont := true; (* No, the user has to make a decision *)
   done;
   dialog#toplevel#destroy ();
   !result
   
;;
 

end;; (* EDialog *)


(* **************************************** *
        Module Talking_PROJET_NOUVEAU
 * **************************************** *)

(** Edialog construction, binding with the main window and associated reaction for
   the PROJET_NOUVEAU functionnality *)
module Talking_PROJET_NOUVEAU = struct         
 
 (** The name of this module (for debugging purposes) *)
 let  myname = "talking_PROJET_NOUVEAU" ;;

 (** The type of command provided by user by this graphical dialog *)
 class usercmd = fun (r: (string,string) env) -> object 
   method filename     : string = r#get "filename"
   method save_current : bool   = (r#get("save_current") = "yes")
 end;;

 (** The correspondent reaction of the application for the command given by user *)
 let react (st:globalState) (msg: (string,string) env option) =
   match msg with (* TO IMPLEMENT *)
 | Some r -> 
     begin
     try 
       shutdown_everything st ();
       let cmd = (new usercmd r) in
       let fname = cmd#filename in
       let fname =
         check_project_path_name_validity_and_add_extension_if_needed fname in
       prerr_endline (myname^".react: save_current="^(string_of_bool cmd#save_current)); 
       prerr_endline (myname^".react: filename="^fname); 
       (if (st#active_project) && (cmd#save_current) then st#save_project ());
       st#close_project () ;
       st#new_project fname ;
       st#mainwin#window_MARIONNET#set_title ("Marionnet - " ^ fname);
     with | _ -> raise (Failure (myname^".react: unexpected environnement received from dialogs"))
     end
 | None ->  begin
     prerr_endline (myname^".react: NOTHING TO DO")
 end
 ;;

 (** Performs the binding with the main window *)
 let bind (st:globalState) =
   let confirm () = if st#active_project then EDialog.ask_question 
       ~gen_id:"save_current"
       ~title:"FERMER" 
       ~question:"Voulez-vous enregistrer le projet courant ?"
       ~help:None
       ~cancel:true 
       () else (Some (mkenv [("save_current","no")])) in
   let ask_filename () = EDialog.ask_for_fresh_writable_filename 
       ~title:"NOM DU NOUVEAU PROJET" 
       ~filters:[EDialog.MAR;EDialog.ALL] 
       ~help:(Some Msg.help_nom_pour_le_projet) () in
   let result =
     st#mainwin#imgitem_PROJET_NOUVEAU#connect#activate 
       ~callback:(fun via -> react st ((EDialog.sequence [confirm; ask_filename]) via))
   in
   result
  ;;

end;; (* Talking_PROJET_NOUVEAU *)




(* **************************************** *
        Module Talking_PROJET_OUVRIR
 * **************************************** *)

(** Edialog construction, binding with the main window and associated reaction for
   the PROJET_OUVRIR functionnality *)
module Talking_PROJET_OUVRIR = struct         
 
 (** The name of this module (for debugging purposes) *)
 let  myname = "talking_PROJET_OUVRIR" ;;

 (** The type of command provided by user by this graphical dialog *)
 class usercmd = fun (r: (string,string) env) -> object 
   method filename     : string =  r#get("filename")
   method save_current : bool   = (r#get("save_current") = "yes")
 end;;

 (** The correspondent reaction of the application for the command given by user *)
 let react (st:globalState) (msg: (string,string) env option) = match msg with
 | Some r -> 
     begin
     try 
       shutdown_everything st ();
       let cmd = (new usercmd r) in 
       prerr_endline (myname^".react: save_current="^(string_of_bool cmd#save_current)); 
       prerr_endline (myname^".react: filename="^cmd#filename); 
       if (st#active_project) && (cmd#save_current) then st#save_project (); 
       
       prerr_endline ("*** in react open project: now call close_project");
       st#close_project () ;
       begin
         try 
           st#open_project cmd#filename;
           st#mainwin#window_MARIONNET#set_title ("Marionnet - " ^ cmd#filename);
         with e -> (Simple_dialogs.error "OUVRIR UN PROJET" ("Erreur en ouvrant le fichier "^cmd#filename) ()); raise e
       end;
     with e -> raise e 
         (*  | _ -> raise (Failure (myname^".react: unexpected environnement received from dialogs")) *)
     end
 | None -> 
     prerr_endline (myname^".react: NOTHING TO DO")
 ;;

 (** Performs the binding with the main window *)
 let bind (st:globalState) =

   let confirm () = if st#active_project then EDialog.ask_question 
       ~gen_id:"save_current"
       ~title:"FERMER" 
       ~question:"Voulez-vous enregistrer le projet courant ?"
       ~help:None
       ~cancel:true 
       () else (Some (mkenv [("save_current","no")])) in

   let ask_filename () = EDialog.ask_for_existing_filename 
       ~title:"OUVRIR UN PROJET MARIONNET EXISTANT" 
       ~filters:[EDialog.MAR;EDialog.ALL] 
       ~help:(Some Msg.help_nom_pour_le_projet) () in

   st#mainwin#imgitem_PROJET_OUVRIR#connect#activate 
     ~callback:(fun via -> react st ((EDialog.sequence [confirm; ask_filename]) via) )
  ;;

end;; (* Talking_PROJET_OUVRIR *)



(* **************************************** *
      Module Talking_PROJET_ENREGISTRER
 * **************************************** *)


(** Edialog construction, binding with the main window and associated reaction for
   the PROJET_ENREGISTRER functionnality *)
module Talking_PROJET_ENREGISTRER = struct         

 (** Performs the binding with the main window *)
 let bind (st:globalState) =
   st#mainwin#imgitem_PROJET_ENREGISTRER#connect#activate
     ~callback:(fun () -> if is_there_something_on_or_sleeping st () then 
                            Msg.error_saving_while_something_up ()
                          else
                            st#save_project ());;
end;;


(* **************************************** *
    Module Talking_PROJET_ENREGISTRER_SOUS
 * **************************************** *)

(** Edialog construction, binding with the main window and associated reaction for
   the PROJET_ENREGISTRER_SOUS functionnality *)
module Talking_PROJET_ENREGISTRER_SOUS = struct         
 
 (** The name of this module (for debugging purposes) *)
 let  myname = "talking_PROJET_ENREGISTRER_SOUS" ;;

 (** The type of command provided by user by this graphical dialog *)
 class usercmd = fun (r: (string,string) env) -> object 
   method filename   : string =
     check_project_path_name_validity_and_add_extension_if_needed (r#get "filename")
 end;;

 (** The correspondent reaction of the application for the command given by user *)
 let react (st:globalState) (msg: (string,string) env option) = match msg with (* TO IMPLEMENT *)
 | Some r -> 
     if is_there_something_on_or_sleeping st () then Msg.error_saving_while_something_up ()
     else begin
       try 
         let cmd = (new usercmd r) in 
         prerr_endline (myname^".react: filename="^cmd#filename);
         begin
           try 
             st#save_project_as cmd#filename;
             st#mainwin#window_MARIONNET#set_title ("Marionnet - " ^ cmd#filename);
           with _ -> (Simple_dialogs.error "PROJET ENREGISTRER SOUS" ("Échéc de la sauvegarde du project "^cmd#filename) ()) 
         end
       with | _ -> raise (Failure (myname^".react: unexpected environnement received from dialog"))
     end
 | None -> 
     prerr_endline (myname^".react: NOTHING TO DO")
 ;;

 (** Performs the binding with the main window *)
 let bind (st:globalState) =
 
   let ask_filename () = EDialog.ask_for_fresh_writable_filename 
       ~title:"ENREGISTRER SOUS" 
       ~filters:[EDialog.MAR;EDialog.ALL] 
       ~help:(Some Msg.help_nom_pour_le_projet) () in

   st#mainwin#imgitem_PROJET_ENREGISTRER_SOUS#connect#activate 
     ~callback:(fun via -> react st ((EDialog.compose [ask_filename]) via) )
   ;;

end;; (* Talking_PROJET_ENREGISTRER_SOUS *)



(* **************************************** *
    Module Talking_PROJET_COPIER_SOUS
 * **************************************** *)

(** Edialog construction, binding with the main window and associated reaction for
   the PROJET_COPIER_SOUS functionnality *)
module Talking_PROJET_COPIER_SOUS = struct         
 
 (** The name of this module (for debugging purposes) *)
 let  myname = "talking_PROJET_COPIER_SOUS" ;;

 (** The type of command provided by user by this graphical dialog *)
 class usercmd = fun (r: (string,string) env) -> object 
   method filename   : string =
     check_project_path_name_validity_and_add_extension_if_needed (r#get "filename")
 end;;

 (** The correspondent reaction of the application for the command given by user *)
 let react (st:globalState) (msg: (string,string) env option) = match msg with (* TO IMPLEMENT *)
 | Some r -> 
     if is_there_something_on_or_sleeping st () then Msg.error_saving_while_something_up ()
     else begin
       try 
         let cmd = (new usercmd r) in 
         prerr_endline (myname^".react: filename="^cmd#filename); 
         begin
           try 
             st#copy_project_into cmd#filename
           with _ -> (Simple_dialogs.error "PROJECT COPY INTO" ("Error copying the project into "^cmd#filename) ()) 
         end
       with | _ -> raise (Failure (myname^".react: unexpected environnement received from dialog"))
     end
 | None -> 
     prerr_endline (myname^".react: NOTHING TO DO")
 ;;

 (** Performs the binding with the main window *)
 let bind (st:globalState) =
 
   let ask_filename () = EDialog.ask_for_fresh_writable_filename 
       ~title:"COPIER SOUS" 
       ~filters:[EDialog.MAR;EDialog.ALL] 
       ~help:(Some Msg.help_nom_pour_le_projet) () in

   st#mainwin#imgitem_PROJET_COPIER_SOUS#connect#activate 
     ~callback:(fun via -> react st ((EDialog.compose [ask_filename]) via) )
   ;;

end;; (* Talking_PROJET_COPIER_SOUS *)



(* **************************************** *
        Module Talking_PROJET_FERMER
 * **************************************** *)


(** Edialog construction, binding with the main window and associated reaction for
   the PROJET_FERMER functionnality *)
module Talking_PROJET_FERMER = struct         

 (** The name of this module (for debugging purposes) *)
 let  myname = "talking_PROJET_FERMER" ;;

 (** The type of command provided by user by this graphical dialog *)
 class usercmd = fun (r: (string,string) env) -> object 
   method answer  : string = r#get("answer")
 end;;

 (** The correspondent reaction of the application for the command given by user *)
 let react (st:globalState) (msg: (string,string) env option) = match msg with (* TO IMPLEMENT *)
 | Some r -> 
     begin
     try 
       shutdown_everything st ();
       let cmd = (new usercmd r) in 
       prerr_endline (myname^".react: answer="^cmd#answer); 
       if (st#active_project) && (cmd#answer="yes") then st#save_project ();
       st#close_project ();
       st#mainwin#window_MARIONNET#set_title ("Marionnet"); (* no project name *)
     with | _ -> raise (Failure (myname^".react: unexpected environnement received from dialog"))
     end
 | None -> 
     prerr_endline (myname^".react: NOTHING TO DO")
 ;;

 (** Performs the binding with the main window *)
 let bind (st:globalState) =
 
   let confirm () = EDialog.ask_question 
       ~title:"FERMER" 
       ~question:"Voulez-vous enregistrer le projet courant ?"
       ~help:None
       ~cancel:true 
       () in

   st#mainwin#imgitem_PROJET_FERMER#connect#activate 
     ~callback:(fun via -> react st ((EDialog.compose [confirm]) via))

 ;;

end;; (* Talking_PROJET_FERMER *)


(* **************************************** *
    Module Talking_PROJET_IMPORTER_RESEAU
 * **************************************** *)

(** Edialog construction, binding with the main window and associated reaction for
   the PROJET_IMPORTER_RESEAU functionnality *)
module Talking_PROJET_IMPORTER_RESEAU = struct         
 
 (** The correspondent reaction of the application for the command given by user *)
 let react (st:globalState) (msg: (string,string) env option) = match msg with
 | Some r -> 
     let filename =  r#get("filename") in
     prerr_endline ("talking_PROJET_IMPORTER_RESEAU.react: filename="^filename); 
     begin
       try 
         st#import_network ~dotAction:(st#dotoptions#reset_defaults) filename ;
         st#flash ~delay:6000 (utf8 ("La définition xml du réseau a été importée avec succés depuis le fichier "^filename));
       with e -> (Simple_dialogs.error "IMPORTER UNE DÉFINITION XML" ("Erreur d'importation du fichier "^filename) ()); raise e
     end
 | None -> prerr_endline ("talking_PROJET_IMPORTER_RESEAU.react: NOTHING TO DO")
 ;;

 (** Performs the binding with the main window *)
 let bind (st:globalState) =

   let ask_filename () = EDialog.ask_for_existing_filename 
       ~title:"IMPORTER UNE DÉFINITION XML DU RESEAU" 
       ~filters:[EDialog.XML;EDialog.ALL] 
       ~help:None () in

   st#mainwin#imgitem_PROJET_IMPORTER_RESEAU#connect#activate 
     ~callback:(fun via -> react st ((EDialog.sequence [ask_filename]) via) )
  ;;

end;; (* Talking_PROJET_IMPORTER_RESEAU *)



(* **************************************** *
    Module Talking_PROJET_EXPORTER_RESEAU
 * **************************************** *)

(** Edialog construction, binding with the main window and associated reaction for
   the PROJET_EXPORTER_RESEAU functionnality *)
module Talking_PROJET_EXPORTER_RESEAU = struct         
 
 (** The correspondent reaction of the application for the command given by user *)
 let react (st:globalState) (msg: (string,string) env option) = match msg with
 | Some r -> 
     let filename =  r#get("filename") in
     prerr_endline ("talking_PROJET_EXPORTER_RESEAU.react: filename="^filename); 
     begin
       try
         Netmodel.Xml.save_network st#network filename;
         st#flash ~delay:6000 (utf8 ("La définition xml du réseau a été exportée avec succés dans le fichier "^filename));
       with e -> (Simple_dialogs.error "EXPORTER LA DÉFINITION XML" ("Echcc durant l'exportation vers le fichier "^filename) ()); raise e
     end
 | None -> prerr_endline ("talking_PROJET_EXPORTER_RESEAU.react: NOTHING TO DO")
 ;;

 (** Performs the binding with the main window *)
 let bind (st:globalState) =

   let ask_filename () = EDialog.ask_for_fresh_writable_filename 
       ~title:"EXPORTER LA DÉFINITION XML DU RESEAU" 
       ~filters:[EDialog.XML;EDialog.ALL] 
       ~help:None () in

   st#mainwin#imgitem_PROJET_EXPORTER_RESEAU#connect#activate 
     ~callback:(fun via -> react st ((EDialog.sequence [ask_filename]) via) )
  ;;

end;; (* Talking_PROJET_EXPORTER_RESEAU *)


(* **************************************** *
    Module Talking_PROJET_EXPORTER_IMAGE
 * **************************************** *)

(** Edialog construction, binding with the main window and associated reaction for
   the PROJET_EXPORTER_IMAGE functionnality *)
module Talking_PROJET_EXPORTER_IMAGE = struct         
 
 (** The correspondent reaction of the application for the command given by user *)
 let react (st:globalState) (msg: (string,string) env option) = match msg with
 | Some r -> 
     let filename =  r#get("filename") in
     prerr_endline ("talking_PROJET_EXPORTER_IMAGE.react: filename="^filename); 
     begin
       try
         let command = ("cp "^st#pngSketchFile^" "^filename) in
         begin
         print_string "About to call Unix.run...\n"; flush_all ();
         match Unix.run command with
         |  (_ , Unix.WEXITED 0) -> st#flash ~delay:6000 (utf8 ("Image du réseau exportée avec succès dans le fichier "^filename))
         |  _                    -> raise (Failure ("Echec durant l'exportation de l'image du réseau vers le fichier "^filename))
         end
       with e -> (Simple_dialogs.error "EXPORTER L'IMAGE DU RESEAU" ("Echec durant l'exportation vers le fichier "^filename) ()); raise e
     end
 | None -> prerr_endline ("talking_PROJET_EXPORTER_IMAGE.react: NOTHING TO DO")
 ;;

 (** Performs the binding with the main window *)
 let bind (st:globalState) =

   let ask_filename () = EDialog.ask_for_fresh_writable_filename 
       ~title:"EXPORTER L'IMAGE DU RESEAU" 
       ~filters:[EDialog.PNG;EDialog.ALL] 
       ~help:None () in

   st#mainwin#imgitem_PROJET_EXPORTER_IMAGE#connect#activate 
     ~callback:(fun via -> react st ((EDialog.sequence [ask_filename]) via) )
  ;;

end;; (* Talking_PROJET_EXPORTER_IMAGE *)


(* **************************************** *
        Module Talking_PROJET_QUITTER
 * **************************************** *)


(** Edialog construction, binding with the main window and associated reaction for
   the PROJET_QUITTER functionnality *)
module Talking_PROJET_QUITTER = struct         

 (** The name of this module (for debugging purposes) *)
 let  myname = "talking_PROJET_QUITTER" ;;

 (** The type of command provided by user by this graphical dialog *)
 class usercmd = fun (r: (string,string) env) -> object 
   method answer  : string = r#get("answer")
 end;;

 let quit (st:globalState) =
   print_string ">>>>>>>>>>QUIT: BEGIN<<<<<<<<\n";
   (* Shutdown all devices and synchronously wait until they actually terminate: *)
   shutdown_everything st ();
   Task_runner.the_task_runner#wait_for_all_currently_scheduled_tasks;
   st#close_project (); (* destroy the temporary project directory *)
   prerr_endline (myname^".react: Calling mrPropre...");
   st#mrPropre ();
   GMain.Main.quit (); (* Finalize the GUI *)
   
   print_string "Killing the task runner thread...\n";
   Task_runner.the_task_runner#terminate;
   print_string "Killing the death monitor thread...\n";
   Death_monitor.stop_polling_loop ();
   print_string "Killing the blinker thread...\n";
   st#network#ledgrid_manager#kill_blinker_thread;
   print_string "...ok, the blinker thread was killed (from talking.ml).\n";
   print_string "Sync, then kill our process (To do: this is a very ugly kludge)\n";
   flush_all ();
   print_string "Synced.\n";
   (* install_signal_handler Sys.sigint;
      install_signal_handler Sys.sigterm; *)
   commit_suicide Sys.sigkill; (* this always works :-) *)
   print_string "!!! This should never be shown.\n";;

 (** The correspondent reaction of the application for the command given by user *)
 let react (st:globalState) (msg: (string,string) env option) = 
   print_string ">>>>>>>>>>QUITTING: THERE SHOULD BE NOTHING BEFORE THIS<<<<<<<<\n";
   begin
     match msg with (* TO IMPLEMENT *)
     | Some r -> 
         begin           
           try 
             let cmd = (new usercmd r) in 
             prerr_endline (myname^".react: answer="^cmd#answer);
             if (st#active_project) && (cmd#answer="yes") then
               st#save_project ();
             quit st;
           with | _ -> raise (Failure (myname^".react: unexpected environnement received from dialog"))
         end
     | None -> begin
         print_string ">>>>>>>>>>*NOT* QUITTING: the user chose 'cancel'<<<<<<<<\n";
         prerr_endline (myname^".react: NOTHING TO DO")
       end
   end;
;;

 (** Performs the binding with the main window *)
 let bind (st:globalState) =
 
   let confirm user_can_cancel () = EDialog.ask_question 
       ~title:"QUITTER" 
       ~question:"Voulez-vous enregistrer\nle projet courant avant de quitter ?"
       ~help:None
       ~cancel:user_can_cancel
       () in

   let cb x = (fun via ->
                 if (st#active_project) then
                   react st ((EDialog.compose [confirm x]) via)
                 else
                   react st (Some (mkenv [("answer","no")]))) in
   let _ = st#mainwin#imgitem_PROJET_QUITTER#connect#activate ~callback:(cb true) in
   let _ = st#mainwin#toplevel#connect#destroy ~callback:(cb false) in 
   ()

 ;;

end;; (* Talking_PROJET_QUITTER *)



(* **************************************** *
      Module Talking_OPTIONS_CWD
 * **************************************** *)

(** Edialog construction, binding with the main window and associated reaction for
   the OPTION_WORKING_DIR functionnality *)
module Talking_OPTIONS_CWD = struct         
 
 (** The name of this module (for debugging purposes) *)
 let  myname = "talking_OPTION_CWD" ;;

 (** The type of command provided by user by this graphical dialog *)
 class usercmd = fun (r: (string,string) env) -> object 
   method foldername : string = r#get("foldername")
 end;;

 (** The correspondent reaction of the application for the command given by user *)
 let react (st:globalState) (msg: (string,string) env option) = match msg with
 | Some r -> 
     begin
     try 
         let cmd = (new usercmd r) in 
         prerr_endline (myname^".react: foldername="^cmd#foldername);
         st#set_wdir   cmd#foldername;
     with | _ -> raise (Failure (myname^".react: unexpected environnement received from dialogs"))
     end
 | None -> 
     begin
       prerr_endline (myname^".react: WORKING DIRECTORY IS STILL SET TO "^st#get_wdir)
     end
 ;;

 (** Performs the binding with the main window *)
 let bind (st:globalState) =

   let ask_foldername () = EDialog.ask_for_existing_writable_folder_pathname_supporting_sparse_files 
       ~title:"CHOISIR UN RÉPERTOIRE DE TRAVAIL"
       ~help:(Some Msg.help_repertoire_de_travail) () in

   st#mainwin#imgitem_OPTION_CWD#connect#activate 
     ~callback:(fun via -> react st ((EDialog.compose [ask_foldername]) via))

  ;;

end;; (* Talking_OPTION_CWD *)


(* **************************************** *
        Module Talking_AIDE_APROPOS
 * **************************************** *)

(** Edialog construction, binding with the main window and associated reaction for
   the PROJET_AIDE_APROPOS functionnality *)
module Talking_PROJET_AIDE_APROPOS = struct         

 (** Performs the binding with the main window *)
 let bind (st:globalState) =

   let expect_ok () = 
     (let dial = (new Gui.dialog_A_PROPOS ()) in
      let _ = dial#closebutton#connect#clicked ~callback:(dial#toplevel#destroy) in ()) in
     
     let _ = st#mainwin#imgitem_AIDE_APROPOS#connect#activate ~callback:(fun via -> expect_ok via)
     in ()
 ;;
   
end;; (* Talking_PROJET_AIDE_APROPOS *)




(* **************************************** *
      Module Talking_MATERIEL_SKEL
 * **************************************** *)


(** Some generic tools for further Talking_MATERIEL_* modules. *)
module Talking_MATERIEL_SKEL = struct         

 (* Generic dialog loop for INSERT/UPDATE (machine/hub/switch/router etc). 
    The inserted or updated name must be unique in the network. *)
 let dialog_loop ?(help=None) dialog (scan_dialog:unit->(string,string) env) (st:State.globalState) = 

   let result = (ref None) in
   let cont   =  ref true  in
   while (!cont = true) do  
     begin match dialog#toplevel#run () with
     | `OK   -> begin 
                 try 
                 let r = scan_dialog () in

                 let (action,name,oldname) = (r#get("action"),r#get("name"),r#get("oldname")) in

                 (* OK only if the name is not already used in the network (and not empty). *)
                 if ((action="add")    && (st#network#nameExists name)) or
                    ((action="update") && (not (name=oldname)) && (st#network#nameExists name))

                 then 
                   (Simple_dialogs.error "Choix du nom" 
                              ("Le nom '"^name^"' est déja réservé dans le réseau virtuel. "^
                               "Les noms des composants du réseau virtuel doivent être uniques.") ())

                 else 
                   (result := Some r ; cont := false)
                 
                 with 
                 | EDialog.IncompleteDialog -> cont := true
                 | (EDialog.BadDialog     (title,msg))   -> (Simple_dialogs.error   title   msg ()) 
                 | (EDialog.StrangeDialog (title,msg,r)) -> (*(Msg.warning title msg ()); *)
                       begin
                       match EDialog.ask_question ~gen_id:"answer" ~title:"CONFIRMER" 
                       ~question:(msg^"\nVous confirmez cette connexion ?") ~help:None ~cancel:false ()
                       with
                       | Some e -> if (e#get("answer")="yes") 
                                   then (result := Some r ; cont := false) 
                                   else cont := true
                       | None   -> (*raise (Failure "Unexpected result of dialog ask_question")*)
				   cont := true (* Consider as the answer "no" *)
                       end
                end

     | `HELP -> (match help with 
                 | Some f -> f ();
                 | None -> ()
                 )

     |  _    -> result := None ; cont := false 

     end 
   done;

   (* Close the dialog and return its result. *)
   dialog#toplevel#destroy ();
   !result

  ;;

 (** The dialog for SHUTDOWN. *)
 let ask_confirm_shutdown x =
     EDialog.ask_question
      ~gen_id:"answer"  
      ~title:"Shutdown" 
      ~question:("Voulez-vous éteindre "^x^"?")
      ~help:None
      ~cancel:false 
 ;;

 (** The dialog for POWER OFF. *)
 let ask_confirm_poweroff x =
     EDialog.ask_question
      ~gen_id:"answer"  
      ~title:"Power off" 
      ~question:("Voulez-vous débrancher le courant à "^x^" ?\n"^
                 "Il est aussi possible de l'arrêter gracieusement (shutdown).")
      ~help:None
      ~cancel:false 
 ;;

 (** Generic binding. *)
 let bind = fun st 
     ajout msg_ajout   
     modif modif_menu msg_modif
     ask_element
     react_insert_update 

     elim elim_menu msg_elim
     ask_confirm_elim
     react_elim

     startup startup_menu react_startup
     shutdown shutdown_menu react_shutdown
     poweroff poweroff_menu react_poweroff
     suspend suspend_menu react_suspend
     resume resume_menu react_resume
     getElementNames
     getElementByName ->

    (* INSERT *)
     ajout#connect#activate 
     ~callback:(fun via -> 
       react_insert_update st ((ask_element ~title:msg_ajout ~update:None st) via)) ;

    (* UPDATE *)
    Widget.DynamicSubmenu.make 
     ~submenu:modif_menu
     ~menu:modif
     ~dynList:getElementNames
     ~action:(fun x ->fun via -> 
       let e = getElementByName x in
       react_insert_update st ((ask_element ~title:(msg_modif^" "^x) ~update:(Some e) st) via)) 
     () ;

    (* ELIM *)
    let ask_confirm_elim_name x = 
      EDialog.compose [ (fun u -> Some (mkenv [("name",x)])) ; ask_confirm_elim x ] in
    Widget.DynamicSubmenu.make 
     ~submenu:elim_menu
     ~menu:elim
     ~dynList:getElementNames
     ~action:(fun x ->fun via -> react_elim st ((ask_confirm_elim_name x) via)) 
     () ;

    (* STARTUP *)
    Widget.DynamicSubmenu.make 
     ~submenu:startup_menu
     ~menu:startup
     ~dynList:(fun () -> (List.filter
                            (fun x -> (getElementByName x)#can_startup)
                            (getElementNames ())))
     ~action:(fun x ->fun via -> react_startup st x via) 
     () ;

    (* SHUTDOWN *)
    Widget.DynamicSubmenu.make 
     ~submenu:shutdown_menu
     ~menu:shutdown
     ~dynList:(fun () -> (List.filter
                            (fun x -> (getElementByName x)#can_gracefully_shutdown)
                            (getElementNames ())))
(*      ~action:(fun x ->fun via -> react_shutdown st x via) *)
     ~action:(fun x -> fun via ->
       (match ask_confirm_shutdown x () with
         Some(e) ->
           (print_string "The answer is "; print_string (e#get "answer"); print_string "\n";
            if (e#get "answer") = "yes" then
              react_shutdown st x via)
(*        | _ -> assert false)*)
        | _ -> () )  (* Ok Luca ? *)
        )
     () ;

    (* POWEROFF *)
    let ask_confirm_poweroff x =
      EDialog.compose [ (fun u -> Some (mkenv [("name",x)])) ; ask_confirm_poweroff x ] in
    Widget.DynamicSubmenu.make 
     ~submenu:poweroff_menu
     ~menu:poweroff
     ~dynList:(fun () -> (List.filter
                            (fun x -> (getElementByName x)#can_poweroff)
                            (getElementNames ())))
(*      ~action:(fun x ->fun via -> react_poweroff st x via) *)
     ~action:(fun x -> fun via ->
       (match ask_confirm_poweroff x () with
         Some(e) ->
           (print_string "The answer is "; print_string (e#get "answer"); print_string "\n";
            if (e#get "answer") = "yes" then
              react_poweroff st x via)
        | _ -> ()))
     () ;

    (* SUSPEND *)
    Widget.DynamicSubmenu.make 
     ~submenu:suspend_menu
     ~menu:suspend
     ~dynList:(fun () -> (List.filter
                            (fun x -> (getElementByName x)#can_suspend)
                            (getElementNames ())))
     ~action:(fun x ->fun via -> react_suspend st x via) 
     () ;

    (* RESUME *)
    Widget.DynamicSubmenu.make 
     ~submenu:resume_menu
     ~menu:resume
     ~dynList:(fun () -> (List.filter
                            (fun x -> (getElementByName x)#can_resume)
                            (getElementNames ())))
     ~action:(fun x ->fun via -> react_resume st x via) 
     () ;


   ()

  ;; (* end of generic bind *)

end (* end of module Talking_MATERIEL_SKEL *)
;;



(* **************************************** *
      Module Talking_MATERIEL_MACHINE
 * **************************************** *)

(** Edialog construction, binding with the main window and associated reaction for
   the MATERIEL_MACHINE functionnalities *)
module Talking_MATERIEL_MACHINE = struct         
 
 (** The name of this module (for debugging purposes) *)
 let  myname = "talking_MATERIEL_MACHINE" ;;

 (** The class for INSERT/UPDATE user command (deliver by the ask_machine dialog)  *)
 class usercmd = fun (r: (string,string) env) -> object 
   method name    : string = r#get("name")
   method action  : string = r#get("action")
   method oldname : string = r#get("oldname")
   method memory  : string = r#get("memory")
   method eth     : string = r#get("eth")
   method ttyS    : string = r#get("ttyS")
   method distrib : string = r#get("distrib")
   method patch   : string = r#get("patch")
   method kernel  : string = r#get("kernel")
   method term    : string = r#get("term")
 end;; (* end of class usercmd *)

 (** The reaction for MACHINE INSERT/UPDATE user command. *)
 let react_insert_update (st:globalState) (msg: (string,string) env option) = match msg with
 | Some r -> 
     begin
       let cmd = (new usercmd r) in 
       prerr_endline (myname^".react: name    = "^cmd#name);
       prerr_endline (myname^".react: action  = "^cmd#action);
       prerr_endline (myname^".react: oldname = "^cmd#oldname);
       prerr_endline (myname^".react: memory  = "^cmd#memory);
       prerr_endline (myname^".react: eth     = "^cmd#eth);
       prerr_endline (myname^".react: ttyS    = "^cmd#ttyS);
       prerr_endline (myname^".react: distrib = "^cmd#distrib);
       prerr_endline (myname^".react: patch   = "^cmd#patch);
       prerr_endline (myname^".react: kernel  = "^cmd#kernel);
       prerr_endline (myname^".react: term    = "^cmd#term);
       let details = get_network_details_interface () in
       let defects = get_defects_interface () in
       begin
         match cmd#action with 
         | "add"    -> 
             details#add_device cmd#name "machine" (int_of_string cmd#eth);
             defects#add_device cmd#name "machine" (int_of_string cmd#eth);
             let m = (new Netmodel.machine ~network:st#network ~name:cmd#name 
                           ~mem:(int_of_string cmd#memory) 
                           ~ethnum:(int_of_string cmd#eth) 
                           ~ttySnum:(int_of_string cmd#ttyS) 
                           ~distr:cmd#distrib
                           ~variant:cmd#patch
                           ~ker:cmd#kernel 
                           ~ter:cmd#term ()) in
             (* Don't store the variant as a symlink: *)
             m#resolve_variant;
             st#network#addMachine m ;
             st#update_sketch ();
             st#update_state  ();
             st#update_cable_sensitivity ();
             Filesystem_history.add_device cmd#name ("machine-"^cmd#distrib) m#get_variant "machine";
         | "update" -> 
             let m = st#network#getMachineByName cmd#oldname in
             m#destroy; (* make sure the simulated object is in state 'no-device' *)
             st#network#changeNodeName cmd#oldname cmd#name  ;
             m#set_memory      (int_of_string cmd#memory)  ;
             m#set_eth_number  (int_of_string cmd#eth   )  ;
             m#set_ttyS_number (int_of_string cmd#ttyS  )  ;
             (* Distribution and variant can not be changed; they are set once and for all at
                creation time: *)
              (*
             m#set_distrib     cmd#distrib ;
             m#set_state       cmd#patch   ;
              *)
             (* Instead the kernel can be changed later: *)
             m#set_kernel      cmd#kernel  ;
             m#set_terminal    cmd#term    ;
             st#refresh_sketch () ; 
             Filesystem_history.rename_device cmd#oldname cmd#name;
             details#rename_device cmd#oldname cmd#name;
             details#update_ports_no cmd#name (int_of_string cmd#eth);
             defects#rename_device cmd#oldname cmd#name;
             defects#update_ports_no cmd#name (int_of_string cmd#eth);
             st#update_cable_sensitivity ();
         | _  -> raise (Failure (myname^".react: unexpected action"))
       end
     end
 | None -> prerr_endline (myname^".react: NOTHING TO DO")
     
 ;; (* end of react_insert_update *)


 (** The dialog for MACHINE INSERT/UPDATE. *)
 let ask_machine ~title ~(update:Netmodel.machine option) (st:globalState) () =
 
   let dialog=new Gui.dialog_MACHINE () in 

   (* MACHINE Dialog definition *)

   dialog#toplevel#set_title (utf8 title);

   let kernel = Widget.ComboTextTree.fromList
      ~callback:(Some prerr_endline)
      ~packing:(Some (dialog#table#attach ~left:2 ~top:6 ~right:4)) 
      (MSys.kernelList ())  in

    let distrib = Widget.ComboTextTree.fromListWithSlave 
      ~masterCallback:(Some prerr_endline)
      ~masterPacking: (Some (dialog#table#attach ~left:2 ~top:4 ~right:4)) 
      (* The user can't change filesystem and variant any more once the device
        has been created:*)
      (match update with
        None -> (MSys.machine_filesystem_list ())
      | Some m -> [m#get_distrib])
      ~slaveCallback: (Some prerr_endline)
      ~slavePacking:  (Some (dialog#table#attach ~left:2 ~top:5 ~right:4))
      (fun unprefixed_filesystem ->
        match update with
          None ->
            MSys.variant_list_of ("machine-" ^ unprefixed_filesystem) ()
        | Some m -> [m#get_variant])  in
    let terminal = Widget.ComboTextTree.fromList
      ~callback:(Some prerr_endline)
      ~packing:(Some (dialog#table#attach ~left:2 ~top:8 ~right:4)) 
      MSys.termList in

   (* Set defaults. If we are updating, defaults are the old values. *)
   begin
   match update with
   | None   -> distrib#set_active_value "default";
               dialog#name#set_text (st#network#suggestedName "m"); 
               dialog#name#misc#grab_focus () 
   | Some m -> begin
                dialog#name#set_text           m#get_name                  ;
                dialog#memory#set_value        (float_of_int m#get_memory) ;

                dialog#eth#set_value           (float_of_int m#get_eth_number)    ;
                dialog#ttyS#set_value          (float_of_int m#get_ttyS_number)   ;

                (* The user cannot remove receptacles used by a cable. *)
                let min_eth = (st#network#maxBusyReceptacleIndex m#get_name Netmodel.Eth)+1 in
                prerr_endline (myname^".defaults: min_eth = "^(string_of_int min_eth)) ;               
                dialog#eth#adjustment#set_bounds ~lower:(float_of_int (max min_eth 1)) () ;

                let min_ttyS = (st#network#maxBusyReceptacleIndex m#get_name Netmodel.TtyS)+1 in
                prerr_endline (myname^".defaults: min_ttyS = "^(string_of_int min_ttyS))  ;              
                dialog#ttyS#adjustment#set_bounds ~lower:(float_of_int (max min_ttyS 1)) () ;

                distrib#set_active_value       m#get_distrib               ;
                distrib#slave#set_active_value m#get_variant              ;

                kernel#set_active_value        m#get_kernel                ;
                terminal#set_active_value      m#get_terminal ;
               end
   end;
  
   (* MACHINE Dialog parser *)
   let scan_dialog () = 
     begin
     let n     = dialog#name#text                                                   in
     let (c,o) = match update with None -> ("add","") | Some m -> ("update",m#name) in
     let m     = (string_of_int dialog#memory#value_as_int)                         in
     let e     = (string_of_int dialog#eth#value_as_int)                            in
     let s     = (string_of_int dialog#ttyS#value_as_int)                           in
     let d     = distrib#selected                                                   in
     let p     = distrib#slave#selected                                             in
     let k     = kernel#selected                                                    in 
     let t     = terminal#selected                                                  in 

     if not (Str.wellFormedName n) then raise EDialog.IncompleteDialog  else

            mkenv [("name",n) ; ("action",c)  ; ("oldname",o) ; ("memory",m) ; ("eth",e) ;  
              ("ttyS",s) ; ("distrib",d) ; ("patch",p)   ; ("kernel",k) ; ("term",t)  ]
     end in

   (* Call the Dialog loop *)
   Talking_MATERIEL_SKEL.dialog_loop ~help:(Some Msg.help_machine_insert_update) dialog scan_dialog st

 ;; (* end of MACHINE INSERT/UPDATE dialog *)


 (** The reaction for MACHINE ELIMINATE user command. *)
 let react_elim (st:globalState) (msg: (string,string) env option) = 
   match msg with
   | Some r -> 
       begin (* TO IMPLEMENT *)
         let answer = r#get("answer") in
         let name   = r#get("name")   in
         let details = get_network_details_interface () in
         let defects = get_defects_interface () in
         if (answer="yes") then begin 
           prerr_endline (myname^".react_elim: removing machine "^name);  
           (st#network#delMachine name) ;
           st#update_sketch () ;
           st#update_state  () ;
           st#update_cable_sensitivity ();
           details#remove_device name;
           defects#remove_device name;
         end
         else ()
       end 
   |  None -> raise (Failure "react_elim")

 ;; (* end of react_elim *)

 (** The dialog for MACHINE ELIMINATE. *)
 let ask_confirm_machine_elim x = 
     EDialog.ask_question
      ~gen_id:"answer"  
      ~title:"ELIMINER" 
      ~question:("Confirmez-vous l'élimination de "^x^"\net de tous le cables éventuellement branchés à cette machine ?")
      ~help:None
      ~cancel:false 
 ;; (* end of ask_confirm_machine_elim *)


 (** MACHINE bindings with the main window *)
 let bind (st:globalState) =

   Talking_MATERIEL_SKEL.bind st
     st#mainwin#imagemenuitem_MACHINE_AJOUT "MACHINE AJOUT"
     st#mainwin#imagemenuitem_MACHINE_MODIF st#mainwin#imagemenuitem_MACHINE_MODIF_menu "MACHINE PROPRIÉTÉS"
     ask_machine 
     react_insert_update 
     st#mainwin#imagemenuitem_MACHINE_ELIM  st#mainwin#imagemenuitem_MACHINE_ELIM_menu  "MACHINE SUPPRIMER"
     ask_confirm_machine_elim
     react_elim

     st#mainwin#imagemenuitem_MACHINE_STARTUP st#mainwin#imagemenuitem_MACHINE_STARTUP_menu
     (fun st name () ->
       let m = st#network#getMachineByName name in
       m#startup)
     st#mainwin#imagemenuitem_MACHINE_SHUTDOWN st#mainwin#imagemenuitem_MACHINE_SHUTDOWN_menu
     (fun st name () ->
       let m = st#network#getMachineByName name in
       m#gracefully_shutdown)
     st#mainwin#imagemenuitem_MACHINE_POWEROFF st#mainwin#imagemenuitem_MACHINE_POWEROFF_menu
     (fun st name () ->
       let m = st#network#getMachineByName name in
       m#poweroff)
     st#mainwin#imagemenuitem_MACHINE_SUSPEND st#mainwin#imagemenuitem_MACHINE_SUSPEND_menu
     (fun st name () ->
       let m = st#network#getMachineByName name in
       m#suspend)
     st#mainwin#imagemenuitem_MACHINE_RESUME st#mainwin#imagemenuitem_MACHINE_RESUME_menu
     (fun st name () ->
       let m = st#network#getMachineByName name in
       m#resume)
     
     (fun u -> st#network#getMachineNames)
     st#network#getMachineByName

 ;; (* end of MACHINE bind *)

end;; (* Talking_MATERIEL_MACHINE *)


(* **************************************** *
      Module Talking_MATERIEL_DEVICE
 * **************************************** *)

(** Edialog construction, binding with the main window and associated reaction for
   the _MATERIEL_DEVICE functionnalities *)
module Talking_MATERIEL_DEVICE = struct         
 
  type dialog_DEVICE  = 
    <  name         : GEdit.entry            ;
       label        : GEdit.entry            ;
       eth          : GEdit.spin_button      ;
       toplevel     : GWindow.dialog_any  >
  ;;

 (** The name of this module (for debugging purposes) *)
 let  myname = "talking_MATERIEL_DEVICE" ;;

 (** The class for INSERT/UPDATE user command (deliver by the ask_device dialog)  *)
 class usercmd = fun (r: (string,string) env) -> object 
   method name      : string = r#get("name")
   method action    : string = r#get("action")
   method oldname   : string = r#get("oldname")
   method label     : string = r#get("label")
   method eth       : string = r#get("eth")
 end;; (* end of class usercmd *)

 (** The reaction for DEVICE INSERT/UPDATE user command. *)
 let react_insert_update devkind (st:globalState) (msg: (string,string) env option) = match msg with
 | Some r -> 
     begin
       let cmd = (new usercmd r) in 
       prerr_endline (myname^".react: name      = "^cmd#name);
       prerr_endline (myname^".react: action    = "^cmd#action);
       prerr_endline (myname^".react: oldname   = "^cmd#oldname);
       prerr_endline (myname^".react: label     = "^cmd#label);
       prerr_endline (myname^".react: eth       = "^cmd#eth);
       let details = get_network_details_interface () in
       let defects = get_defects_interface () in
       begin
         match cmd#action with 
         | "add"    -> 
             (match devkind with
               Netmodel.Router ->
                 (* Ok, add the router: *)
                 details#add_device cmd#name "router" (int_of_string cmd#eth);
             | _ ->
                 ());
             defects#add_device cmd#name (Netmodel.string_of_devkind devkind) (int_of_string cmd#eth);
             let d = (new Netmodel.device ~network:st#network ~name:cmd#name ~label:cmd#label 
                            ~devkind
                            ~variant:(if Netmodel.is_there_a_router_variant () then
                                        "default"
                                      else
                                        Strings.no_variant_text)
                            (int_of_string cmd#eth) ()) in 
             d#resolve_variant; (* don't store the variant as a symlink *)
             st#network#addDevice d;
             st#update_cable_sensitivity ();
             st#update_sketch ();
             st#update_state  ();
             st#update_cable_sensitivity ();
         | "update" -> 
             let d = st#network#getDeviceByName cmd#oldname in
             d#destroy;
             let connected_ports = st#network#ledgrid_manager#get_connected_ports ~id:(d#id) () in
             st#network#ledgrid_manager#destroy_device_ledgrid ~id:(d#id) ();
             st#network#changeNodeName cmd#oldname cmd#name  ;
             d#set_label    cmd#label                   ;
             d#set_eth_number ~prefix:"port"  (int_of_string cmd#eth)  ;
             st#refresh_sketch () ;
             st#network#make_device_ledgrid d;
             Filesystem_history.rename_device cmd#oldname cmd#name;
             (match devkind with
               Netmodel.Router ->
                 details#rename_device cmd#oldname cmd#name;
                 details#update_ports_no cmd#name (int_of_string cmd#eth);
             | _ ->
                 ());
             defects#rename_device cmd#oldname cmd#name;
             defects#update_ports_no cmd#name (int_of_string cmd#eth);
             st#update_cable_sensitivity ();
         | _  -> raise (Failure (myname^".react: unexpected action"))
       end
     end
 | None -> 
     begin
       prerr_endline (myname^".react: NOTHING TO DO")
     end
 ;; (* end of react_insert_update DEVICE *)


 (** The dialog for DEVICE INSERT/UPDATE. *)
 let ask_device devkind ~title ~(update:Netmodel.device option) (st:globalState) () =
 
   let dialog =
     if (devkind=Netmodel.Hub) then
       ((new Gui.dialog_HUB    ()) :> dialog_DEVICE )  
     else if (devkind=Netmodel.Switch) then
       ((new Gui.dialog_SWITCH ()) :> dialog_DEVICE )
     else if (devkind=Netmodel.Router) then
       ((new Gui.dialog_ROUTER ()) :> dialog_DEVICE )
     else
       assert false
   in 

   (* DEVICE Dialog definition *)

   dialog#toplevel#set_title (utf8 title);

   (* Set defaults. If we are updating, defaults are the old values. *)
   begin
   match update with
   | None   -> let prefix = (match devkind with 
                | Netmodel.Hub -> "H" | Netmodel.Switch -> "S" | Netmodel.Router -> "R" 
                | _ -> assert false) in
               dialog#name#set_text (st#network#suggestedName prefix); 
               dialog#name#misc#grab_focus () 
   | Some h -> begin
                dialog#name#set_text                  h#get_name                  ;
                dialog#label#set_text                 h#get_label                 ;
                dialog#eth#set_value                  (float_of_int h#get_eth_number);

                (* The user cannot remove receptacles used by a cable. *)
                let min_eth = (st#network#maxBusyReceptacleIndex h#get_name Netmodel.Eth)+1 in
                let min_multiple_of_4 = (ceil ((float_of_int min_eth) /. 4.0)) *. 4.0 in
                prerr_endline (myname^".defaults: min_eth = "^(string_of_int min_eth)) ;               
                dialog#eth#adjustment#set_bounds ~lower:(max min_multiple_of_4 4.0) () ;
               end
   end;

  
   (* DEVICE Dialog parser *)
   let scan_dialog () = 
     begin
     let n     = dialog#name#text                                                        in
     let (c,o) = match update with None -> ("add","") | Some h -> ("update",h#name)      in
     let l     = dialog#label#text                                                       in
     let eth   = (string_of_int dialog#eth#value_as_int)                                 in

     if not (Str.wellFormedName n) then raise EDialog.IncompleteDialog  else

            mkenv [("name",n)  ; ("action",c)   ; ("oldname",o)      ; ("label",l)        ; ("eth",eth);]
     end in

   (* Call the Dialog loop *)
   Talking_MATERIEL_SKEL.dialog_loop ~help:(Some (Msg.help_device_insert_update devkind)) dialog scan_dialog st

 ;; (* end of DEVICE INSERT/UPDATE dialog *)


 (** The reaction for DEVICE ELIMINATE user command. *)
 let react_elim (st:globalState) (msg: (string,string) env option) = 
   match msg with
   | Some r -> 
       let details = get_network_details_interface () in
       let defects = get_defects_interface () in
       begin
         let answer = r#get("answer") in
         let name   = r#get("name")   in
         if (answer="yes") then begin 
           prerr_endline (myname^".react_elim: removing device "^name);  
           (st#network#delDevice name) ;
           st#update_sketch ();
           st#update_state  ();
           (* This does nothing if the tree doesn't exist, which is what we want here: *)
           details#remove_device name;
           defects#remove_device name;
           st#update_cable_sensitivity ();
         end
         else ()
       end 
   |  None -> raise (Failure "DEVICE react_elim")

 ;; (* end of react_elim *)

 (** The dialog for DEVICE ELIMINATE. *)
 let ask_confirm_device_elim devkind x = 
   let what = (Netmodel.string_of_devkind devkind) in
   EDialog.ask_question 
       ~gen_id:"answer"  
       ~title:"ELIMINER" 
       ~question:("Vous confirmez l'élimination de "^x^"\net de tous le cables éventuellement branchés à ce "^what^" ?")
       ~help:None
       ~cancel:false 
 ;; (* end of ask_confirm_device_elim *)


 (** DEVICE bindings with the main window *)
 let bind devkind (st:globalState) =
   
   match devkind with

   | Netmodel.Hub -> 

       Talking_MATERIEL_SKEL.bind st
         st#mainwin#imagemenuitem_HUB_AJOUT "HUB AJOUT"
         st#mainwin#imagemenuitem_HUB_MODIF st#mainwin#imagemenuitem_HUB_MODIF_menu "HUB PROPRIÉTÉS"
         (ask_device Netmodel.Hub) 
         (react_insert_update Netmodel.Hub)
         st#mainwin#imagemenuitem_HUB_ELIM  st#mainwin#imagemenuitem_HUB_ELIM_menu  "HUB SUPPRIMER"
         (ask_confirm_device_elim Netmodel.Hub)
         react_elim

     st#mainwin#imagemenuitem_HUB_STARTUP st#mainwin#imagemenuitem_HUB_STARTUP_menu
     (fun st name () ->
       let m = st#network#getDeviceByName name in
       m#startup)
     st#mainwin#imagemenuitem_HUB_SHUTDOWN st#mainwin#imagemenuitem_HUB_SHUTDOWN_menu
     (fun st name () ->
       let m = st#network#getDeviceByName name in
       m#gracefully_shutdown)
     st#mainwin#imagemenuitem_HUB_POWEROFF st#mainwin#imagemenuitem_HUB_POWEROFF_menu
     (fun st name () ->
       let m = st#network#getDeviceByName name in
       m#poweroff)
     st#mainwin#imagemenuitem_HUB_SUSPEND st#mainwin#imagemenuitem_HUB_SUSPEND_menu
     (fun st name () ->
       let m = st#network#getDeviceByName name in
       m#suspend)
     st#mainwin#imagemenuitem_HUB_RESUME st#mainwin#imagemenuitem_HUB_RESUME_menu
     (fun st name () ->
       let m = st#network#getDeviceByName name in
       m#resume)

         (fun u -> st#network#getHubNames)
         st#network#getDeviceByName

   | Netmodel.Switch -> 

       Talking_MATERIEL_SKEL.bind st
         st#mainwin#imagemenuitem_SWITCH_AJOUT "SWITCH AJOUT"
         st#mainwin#imagemenuitem_SWITCH_MODIF st#mainwin#imagemenuitem_SWITCH_MODIF_menu "SWITCH PROPRIÉTÉS"
         (ask_device Netmodel.Switch) 
         (react_insert_update Netmodel.Switch)
         st#mainwin#imagemenuitem_SWITCH_ELIM  st#mainwin#imagemenuitem_SWITCH_ELIM_menu  "SWITCH SUPPRIMER"
         (ask_confirm_device_elim Netmodel.Switch)
         react_elim

     st#mainwin#imagemenuitem_SWITCH_STARTUP st#mainwin#imagemenuitem_SWITCH_STARTUP_menu
     (fun st name () ->
       let m = st#network#getDeviceByName name in
       m#startup)
     st#mainwin#imagemenuitem_SWITCH_SHUTDOWN st#mainwin#imagemenuitem_SWITCH_SHUTDOWN_menu
     (fun st name () ->
       let m = st#network#getDeviceByName name in
       m#gracefully_shutdown)
     st#mainwin#imagemenuitem_SWITCH_POWEROFF st#mainwin#imagemenuitem_SWITCH_POWEROFF_menu
     (fun st name () ->
       let m = st#network#getDeviceByName name in
       m#poweroff)
     st#mainwin#imagemenuitem_SWITCH_SUSPEND st#mainwin#imagemenuitem_SWITCH_SUSPEND_menu
     (fun st name () ->
       let m = st#network#getDeviceByName name in
       m#suspend)
     st#mainwin#imagemenuitem_SWITCH_RESUME st#mainwin#imagemenuitem_SWITCH_RESUME_menu
     (fun st name () ->
       let m = st#network#getDeviceByName name in
       m#resume)

         (fun u -> st#network#getSwitchNames)
         st#network#getDeviceByName

   | Netmodel.Router -> 

       Talking_MATERIEL_SKEL.bind st
         st#mainwin#imagemenuitem_ROUTER_AJOUT "ROUTEUR AJOUT"
         st#mainwin#imagemenuitem_ROUTER_MODIF st#mainwin#imagemenuitem_ROUTER_MODIF_menu "ROUTEUR PROPRIÉTÉS"
         (ask_device Netmodel.Router) 
         (react_insert_update Netmodel.Router)
         st#mainwin#imagemenuitem_ROUTER_ELIM  st#mainwin#imagemenuitem_ROUTER_ELIM_menu  "ROUTEUR SUPPRIMER"
         (ask_confirm_device_elim Netmodel.Router)
         react_elim

     st#mainwin#imagemenuitem_ROUTER_STARTUP st#mainwin#imagemenuitem_ROUTER_STARTUP_menu
     (fun st name () ->
       let m = st#network#getDeviceByName name in
       m#startup)
     st#mainwin#imagemenuitem_ROUTER_SHUTDOWN st#mainwin#imagemenuitem_ROUTER_SHUTDOWN_menu
     (fun st name () ->
       let m = st#network#getDeviceByName name in
       m#gracefully_shutdown)
     st#mainwin#imagemenuitem_ROUTER_POWEROFF st#mainwin#imagemenuitem_ROUTER_POWEROFF_menu
     (fun st name () ->
       let m = st#network#getDeviceByName name in
       m#poweroff)
     st#mainwin#imagemenuitem_ROUTER_SUSPEND st#mainwin#imagemenuitem_ROUTER_SUSPEND_menu
     (fun st name () ->
       let m = st#network#getDeviceByName name in
       m#suspend)
     st#mainwin#imagemenuitem_ROUTER_RESUME st#mainwin#imagemenuitem_ROUTER_RESUME_menu
     (fun st name () ->
       let m = st#network#getDeviceByName name in
       m#resume)

         (fun u -> st#network#getRouterNames)
         st#network#getDeviceByName

   |  _ -> ()

 ;;


end;; (* Talking_MATERIEL_DEVICE *)







(* **************************************** *
      Module Talking_MATERIEL_CABLE_RJ45
 * **************************************** *)

(** Edialog construction, binding with the main window and associated reaction for
   the MATERIEL_\{DROIT,CROISE\} functionnalities *)
module Talking_MATERIEL_CABLE_RJ45 = struct         

  open Netmodel;;
 
  type dialog_CABLE_RJ45  = 
    <  name         : GEdit.entry            ;
       label        : GEdit.entry            ;
       table_link   : GPack.table            ;
       toplevel     : GWindow.dialog_any     >
  ;;

 (** The name of this module (for debugging purposes) *)
 let  myname = "talking_MATERIEL_CABLE_RJ45" ;;

 (** The class for INSERT/UPDATE user command (deliver by the ask_device dialog)  *)
 class usercmd = fun (r: (string,string) env) -> object 
   method name         : string = r#get("name")
   method action       : string = r#get("action")
   method oldname      : string = r#get("oldname")
   method label        : string = r#get("label")
   method left_node    : string = r#get("left_node")
   method right_node   : string = r#get("right_node")
   method left_recept  : string = r#get("left_recept")
   method right_recept : string = r#get("right_recept")
 end;; (* end of class usercmd *)

 (** The reaction for CABLE_RJ45 INSERT/UPDATE user command. *)
 let react_insert_update cablekind (st:globalState) (msg: (string,string) env option) = match msg with
 | Some r -> 
     begin
       let cmd = (new usercmd r) in 
       prerr_endline (myname^".react: name         = "^cmd#name);
       prerr_endline (myname^".react: action       = "^cmd#action);
       prerr_endline (myname^".react: oldname      = "^cmd#oldname);
       prerr_endline (myname^".react: label        = "^cmd#label);
       prerr_endline (myname^".react: left_node    = "^cmd#left_node);
       prerr_endline (myname^".react: left_recept  = "^cmd#left_recept);
       prerr_endline (myname^".react: right_node   = "^cmd#right_node);
       prerr_endline (myname^".react: right_recept = "^cmd#right_recept);

       let sock1 = { Netmodel.nodename = cmd#left_node  ; Netmodel.receptname = cmd#left_recept  } in
       let sock2 = { Netmodel.nodename = cmd#right_node ; Netmodel.receptname = cmd#right_recept } in
       let left_device, left_port = sock1.nodename, sock1.receptname in
       let right_device, right_port = sock2.nodename, sock2.receptname in
       let left_endpoint_name = Printf.sprintf "to %s (%s)" left_device left_port in
       let right_endpoint_name = Printf.sprintf "to %s (%s)" right_device right_port in
       let defects = get_defects_interface () in
       begin
         match cmd#action with 
         | "add"    -> 
             defects#add_cable cmd#name (string_of_cablekind cablekind) left_endpoint_name right_endpoint_name;
             let c = (new Netmodel.cable ~network:st#network ~name:cmd#name ~label:cmd#label ~cablekind ~left:sock1 ~right:sock2 ()) in 
             st#network#addCable c;
             st#refresh_sketch () ;
             st#update_cable_sensitivity ();
             ()
         | "update" -> 
             let c = st#network#getCableByName cmd#oldname in
             defects#rename_cable cmd#oldname cmd#name;
             defects#rename_cable_endpoints cmd#name left_endpoint_name right_endpoint_name;
             st#network#delCable cmd#oldname;
             (* Make a new cable; it should have a different identity from the old one, and it's
                important that it's initialized anew, to get the reference counter right: *)
             let c = (new Netmodel.cable ~network:st#network ~name:cmd#name ~label:cmd#label ~cablekind ~left:sock1 ~right:sock2 ()) in 
             st#network#addCable c;
             st#refresh_sketch () ;
             st#update_cable_sensitivity ();
             ()
         | _  -> raise (Failure (myname^".react: unexpected action"))
       end
     end
 | None -> 
     begin
       prerr_endline (myname^".react: NOTHING TO DO")
     end
 ;; (* end of react_insert_update CABLE_RJ45 *)


 (** The dialog for CABLE_RJ45 INSERT/UPDATE. *)
 let ask_cable cablekind ~title ~(update:Netmodel.cable option) (st:globalState) () =
(* Printf.printf "OK-Q 0\n"; flush_all ();  *)
   let dialog = if (cablekind=Netmodel.Direct)
                then ((new Gui.dialog_DROIT  ()) :> dialog_CABLE_RJ45 )  
                else ((new Gui.dialog_CROISE ()) :> dialog_CABLE_RJ45 )  
                in 

   (* CABLE_RJ45 Dialog definition *)

(* Printf.printf "OK-Q 1\n"; flush_all ();  *)
   dialog#toplevel#set_title (utf8 title);

   (* Slave dependent function for left and right combo: 
      if we are updating we must consider current receptacles as availables! *)
   let freeReceptsOfNode =
     (fun x->
       List.sort
         compare
         (st#network#freeReceptaclesNamesOfNode x Netmodel.Eth)) in

(* Printf.printf "OK-Q 2\n"; flush_all ();  *)
   (* The free receptacles of the given node at the LEFT side of the cable. If we are updating
      we have to consider the old receptacle of the old nodename as available. *) 
   let get_left_recept_of = match update with
   | None   -> freeReceptsOfNode
   | Some c -> (fun x->if   (x = c#get_left.Netmodel.nodename)
                       then c#get_left.Netmodel.receptname::(freeReceptsOfNode x) 
                       else (freeReceptsOfNode x)) in

(* Printf.printf "OK-Q 3\n"; flush_all (); *)
   (* The free receptacles of the given node at the RIGHT side of the cable. If we are updating
      we have to consider the old receptacle of the old nodename as available. *) 
   let get_right_recept_of = match update with
   | None   -> freeReceptsOfNode
   | Some c -> (fun x->if   (x = c#get_right.Netmodel.nodename)
                       then c#get_right.Netmodel.receptname::(freeReceptsOfNode x) 
                       else (freeReceptsOfNode x)) in
   
(* Printf.printf "OK-Q 4\n"; flush_all ();  *)
    let left = Widget.ComboTextTree.fromListWithSlaveWithSlaveWithSlave 
                  ~masterCallback:(Some prerr_endline)
                  ~masterPacking:(Some (dialog#table_link#attach ~left:1 ~top:1 ~right:2))
                  st#network#getNodeNames
                  ~slaveCallback:(Some prerr_endline)
                  ~slavePacking:(Some (dialog#table_link#attach ~left:1 ~top:2 ~right:2))
                  get_left_recept_of
                  ~slaveSlaveCallback:(Some prerr_endline)
                  ~slaveSlavePacking:(Some (dialog#table_link#attach ~left:3 ~top:1 ~right:4))
                  (fun n1 r1 -> st#network#getNodeNames)
                  ~slaveSlaveSlaveCallback:(Some prerr_endline)
                  ~slaveSlaveSlavePacking:(Some (dialog#table_link#attach ~left:3 ~top:2 ~right:4))
                  (fun n1 r1 n2 -> 
                     let l = (get_right_recept_of n2) in
                     if n1 = n2 then (List.substract l [r1]) else l) in
    
(* Printf.printf "OK-Q 5\n"; flush_all ();  *)
    let right=left#slave#slave in

   (* Set defaults. If we are updating, defaults are the old values. *)
   begin
   match update with
   | None   -> 
               let prefix = if (cablekind=Netmodel.Direct) then "d" else "c" in
               dialog#name#set_text (st#network#suggestedName prefix); 
               dialog#name#misc#grab_focus () 
   | Some c -> begin
                dialog#name#set_text              c#get_name                      ;
                dialog#label#set_text             c#get_label                     ;

                left#set_active_value             c#get_left.Netmodel.nodename    ;
                left#slave#set_active_value       c#get_left.Netmodel.receptname  ; 
                right#set_active_value            c#get_right.Netmodel.nodename   ;
                right#slave#set_active_value      c#get_right.Netmodel.receptname ; 
               end
   end;

   (* CABLE_RJ45 Dialog parser *)
   let scan_dialog () = 
     begin
     let n     = dialog#name#text                                                        in
     let (c,o) = match update with None -> ("add","") | Some x -> ("update",x#name)      in
     let l     = dialog#label#text                                                       in

     let ln    = left#selected                                                           in
     let lr    = left#slave#selected                                                     in
     let rn    = right#selected                                                          in
     let rr    = right#slave#selected                                                    in
   
     if (not (Str.wellFormedName n)) or (ln="") or (lr="") or (rn="") or (rr="") then raise EDialog.IncompleteDialog  else

     if (ln,lr) = (rn,rr) 
     then (raise (EDialog.BadDialog   
                    ("Connexion erronée", 
                     "Le cable ne peut pas être branché au même endroit des deux côtés!"))) else

     let result = mkenv [("name",n)       ; ("action",c)         ; ("oldname",o)      ; ("label",l) ;   
                         ("left_node",ln) ; ("left_recept",lr)   ; ("right_node",rn)  ; ("right_recept",rr)]
     in

     if (ln = rn) 
     then (raise (EDialog.StrangeDialog 
                    ("Connexion étrange", 
                     "Le cable est branché sur le même composant.\n\
Les machines ont déjà l'interface de loopback pour cela.",result))) else result

     end in

   (* Call the Dialog loop *)
   Talking_MATERIEL_SKEL.dialog_loop ~help:(Some (Msg.help_cable_insert_update cablekind)) dialog scan_dialog st

 ;; (* end of CABLE_RJ45 INSERT/UPDATE dialog *)


 (** The reaction for CABLE_RJ45 ELIMINATE user command. *)
 let react_elim (st:globalState) (msg: (string,string) env option) = 
   match msg with
   | Some r -> 
       let defects = get_defects_interface () in
       begin (* TO IMPLEMENT *)
         let answer = r#get("answer") in
         let name   = r#get("name")   in
         if (answer="yes") then begin 
          prerr_endline (myname^".react_elim: removing cable "^name);  
          (st#network#delCable name);
          st#refresh_sketch ();
          defects#remove_cable name;
          st#update_cable_sensitivity ();
        end
         else ()
       end 
   |  None -> raise (Failure "CABLE_RJ45 react_elim")

 ;; (* end of react_elim *)

 (** The dialog for CABLE_RJ45 ELIMINATE. *)
 let ask_confirm_cable_elim cablekind x = 
   let what = (Netmodel.string_of_cablekind cablekind) in
   EDialog.ask_question 
       ~gen_id:"answer"  
       ~title:"ELIMINER" 
       ~question:("Vous confirmez l'élimination du cable "^x^" ("^what^" cable) ?")
       ~help:None
       ~cancel:false 
 ;; (* end of ask_confirm_cable_elim *)


 (** CABLE_RJ45 bindings with the main window *)
 let bind cablekind (st:globalState) =
   
   match cablekind with

   | Netmodel.Direct -> 

       Talking_MATERIEL_SKEL.bind st
         st#mainwin#imagemenuitem_DROIT_AJOUT "CABLE RJ45 DROIT AJOUT"
         st#mainwin#imagemenuitem_DROIT_MODIF st#mainwin#imagemenuitem_DROIT_MODIF_menu "CABLE RJ45 DROIT PROPRIÉTÉS"
         (ask_cable Netmodel.Direct) 
         (react_insert_update Netmodel.Direct)
         st#mainwin#imagemenuitem_DROIT_ELIM  st#mainwin#imagemenuitem_DROIT_ELIM_menu  "CABLE RJ45 DROIT SUPPRIMER"
         (ask_confirm_cable_elim Netmodel.Direct)
         react_elim

(* Ugly temporary kludge to work around my laziness in using named parameters: *)
     st#mainwin#imagemenuitem_DROIT_USELESS1 st#mainwin#imagemenuitem_DROIT_USELESS1_menu
     (fun st x () -> print_string ("This should never be printed (2)\n"))
     st#mainwin#imagemenuitem_DROIT_USELESS2 st#mainwin#imagemenuitem_DROIT_USELESS2_menu
     (fun st x () -> print_string ("This should never be printed (3)\n"))
     st#mainwin#imagemenuitem_DROIT_USELESS3 st#mainwin#imagemenuitem_DROIT_USELESS3_menu
     (fun st x () -> print_string ("This should never be printed (4)\n"))
(* Here comes the 'real' binding: *)
     st#mainwin#imagemenuitem_DROIT_SUSPEND st#mainwin#imagemenuitem_DROIT_SUSPEND_menu
     (fun st name () ->
       let c = st#network#getCableByName name in
       c#suspend)
     st#mainwin#imagemenuitem_DROIT_RESUME st#mainwin#imagemenuitem_DROIT_RESUME_menu
     (fun st name () ->
       let c = st#network#getCableByName name in
       c#resume)

         (fun u -> st#network#getDirectCableNames)
         st#network#getCableByName
   | Netmodel.Crossed -> 

       Talking_MATERIEL_SKEL.bind st
         st#mainwin#imagemenuitem_CROISE_AJOUT "CABLE RJ45 CROISÉ AJOUT"
         st#mainwin#imagemenuitem_CROISE_MODIF st#mainwin#imagemenuitem_CROISE_MODIF_menu "CABLE RJ45 CROISÉ PROPRIÉTÉS"
         (ask_cable Netmodel.Crossed) 
         (react_insert_update Netmodel.Crossed)
         st#mainwin#imagemenuitem_CROISE_ELIM  st#mainwin#imagemenuitem_CROISE_ELIM_menu  "CABLE RJ45 CROISÉ SUPPRIMER"
         (ask_confirm_cable_elim Netmodel.Crossed)
         react_elim

(* Ugly temporary kludge to work around my laziness in using named parameters: *)
     st#mainwin#imagemenuitem_CROISE_USELESS1 st#mainwin#imagemenuitem_CROISE_USELESS1_menu
     (fun st x () -> print_string ("This should never be printed (2)\n"))
     st#mainwin#imagemenuitem_CROISE_USELESS2 st#mainwin#imagemenuitem_CROISE_USELESS2_menu
     (fun st x () -> print_string ("This should never be printed (3)\n"))
     st#mainwin#imagemenuitem_CROISE_USELESS3 st#mainwin#imagemenuitem_CROISE_USELESS3_menu
     (fun st x () -> print_string ("This should never be printed (4)\n"))
(* Here comes the 'real' binding: *)
     st#mainwin#imagemenuitem_CROISE_SUSPEND st#mainwin#imagemenuitem_CROISE_SUSPEND_menu
     (fun st name () ->
       let c = st#network#getCableByName name in
       st#refresh_sketch ();
       c#suspend)
     st#mainwin#imagemenuitem_CROISE_RESUME st#mainwin#imagemenuitem_CROISE_RESUME_menu
     (fun st name () ->
       let c = st#network#getCableByName name in
       st#refresh_sketch ();
       c#resume)

         (fun u -> st#network#getCrossedCableNames)
         st#network#getCableByName

   | _ -> () 

 ;;


end;; (* Talking_MATERIEL_CABLE_RJ45 *)








(* **************************************** *
      Module Talking_MATERIEL_CABLE_NULLMODEM
 * **************************************** *)

(** Edialog construction, binding with the main window and associated reaction for
   the MATERIEL_\{DROIT,CROISE\} functionnalities *)
module Talking_MATERIEL_CABLE_NULLMODEM = struct         
 
 (** The name of this module (for debugging purposes) *)
 let  myname = "talking_MATERIEL_CABLE_NULLMODEM" ;;

 (** The class for INSERT/UPDATE user command (deliver by the ask_device dialog)  *)
 class usercmd = fun (r: (string,string) env) -> object 
   method name         : string = r#get("name")
   method action       : string = r#get("action")
   method oldname      : string = r#get("oldname")
   method label        : string = r#get("label")
   method left_node    : string = r#get("left_node")
   method right_node   : string = r#get("right_node")
   method left_recept  : string = r#get("left_recept")
   method right_recept : string = r#get("right_recept")
 end;; (* end of class usercmd *)

 (** The reaction for CABLE_NULLMODEM INSERT/UPDATE user command. *)
 let react_insert_update (st:globalState) (msg: (string,string) env option) = match msg with
 | Some r -> 
     begin
       let cmd = (new usercmd r) in 
       prerr_endline (myname^".react: name         = "^cmd#name);
       prerr_endline (myname^".react: action       = "^cmd#action);
       prerr_endline (myname^".react: oldname      = "^cmd#oldname);
       prerr_endline (myname^".react: label        = "^cmd#label);
       prerr_endline (myname^".react: left_node    = "^cmd#left_node);
       prerr_endline (myname^".react: left_recept  = "^cmd#left_recept);
       prerr_endline (myname^".react: right_node   = "^cmd#right_node);
       prerr_endline (myname^".react: right_recept = "^cmd#right_recept);

       let sock1 = { Netmodel.nodename = cmd#left_node  ; Netmodel.receptname = cmd#left_recept  } in
       let sock2 = { Netmodel.nodename = cmd#right_node ; Netmodel.receptname = cmd#right_recept } in
       let defects = get_defects_interface () in
       begin
         match cmd#action with 
         | "add"    -> 
             let c = (new Netmodel.cable ~network:st#network ~name:cmd#name ~label:cmd#label ~cablekind:Netmodel.NullModem ~left:sock1 ~right:sock2 ()) in 
             (* To do: uncomment this if we ever implement defects in serial cables: *)
             (* defects#add_cable cmd#name "nullmodem"; *)
             st#network#addCable c;
             st#refresh_sketch () ;
             st#update_cable_sensitivity ();
         | "update" -> 
             let c = st#network#getCableByName cmd#oldname in
             c#set_name     cmd#name  ;
             c#set_label    cmd#label ;
             c#set_left     sock1     ;
             c#set_right    sock2     ;
             st#refresh_sketch () ;
             st#update_cable_sensitivity ();
             ()
         | _  -> raise (Failure (myname^".react: unexpected action"))
       end
     end
 | None -> 
     begin
       prerr_endline (myname^".react: NOTHING TO DO")
     end
 ;; (* end of react_insert_update CABLE_NULLMODEM *)


 (** The dialog for CABLE_NULLMODEM INSERT/UPDATE. *)
 let ask_cable ~title ~(update:Netmodel.cable option) (st:globalState) () =
 
   let dialog = new Gui.dialog_SERIE () in 

   (* CABLE_NULLMODEM Dialog definition *)

   dialog#toplevel#set_title (utf8 title);

   (* Slave dependent function for left and right combo: 
      if we are updating we must consider current receptacles as availables! *)
   let freeReceptsOfNode =
     (fun x->
       List.sort
         compare
         (st#network#freeReceptaclesNamesOfNode x Netmodel.TtyS)) in

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
                  ~masterCallback:(Some prerr_endline)
                  ~masterPacking:(Some (dialog#table_link#attach ~left:1 ~top:1 ~right:2))
                  (st#network#getMachineNames)

                  ~slaveCallback:(Some prerr_endline)
                  ~slavePacking:(Some (dialog#table_link#attach ~left:1 ~top:2 ~right:2))
                  get_left_recept_of

                  ~slaveSlaveCallback:(Some prerr_endline)
                  ~slaveSlavePacking:(Some (dialog#table_link#attach ~left:3 ~top:1 ~right:4))
                  (fun n1 r1 -> (List.substract st#network#getMachineNames [n1]))

                  ~slaveSlaveSlaveCallback:(Some prerr_endline)
                  ~slaveSlaveSlavePacking:(Some (dialog#table_link#attach ~left:3 ~top:2 ~right:4))
                  (fun n1 r1 n2 -> (get_right_recept_of n2))
                     
    in
    
    let right=left#slave#slave in

   (* Set defaults. If we are updating, defaults are the old values. *)
   begin
   match update with
   | None   -> dialog#name#set_text (st#network#suggestedName "s"); 
               dialog#name#misc#grab_focus () 
   | Some c -> begin
                dialog#name#set_text              c#get_name                      ;
                dialog#label#set_text             c#get_label                     ;

                left#set_active_value             c#get_left.Netmodel.nodename    ;
                left#slave#set_active_value       c#get_left.Netmodel.receptname  ; 
                right#set_active_value            c#get_right.Netmodel.nodename   ;
                right#slave#set_active_value      c#get_right.Netmodel.receptname ; 
               end
   end;

  
   (* CABLE_NULLMODEM Dialog parser *)
   let scan_dialog () = 
     begin
     let n     = dialog#name#text                                                        in
     let (c,o) = match update with None -> ("add","") | Some x -> ("update",x#name)      in
     let l     = dialog#label#text                                                       in

     let ln    = left#selected                                                           in
     let lr    = left#slave#selected                                                     in
     let rn    = right#selected                                                          in
     let rr    = right#slave#selected                                                    in

     if (not (Str.wellFormedName n)) or (ln="") or (lr="") or (rn="") or (rr="") then raise EDialog.IncompleteDialog  else

     let result = mkenv [("name",n)       ; ("action",c)         ; ("oldname",o)      ; ("label",l) ;   
                         ("left_node",ln) ; ("left_recept",lr)   ; ("right_node",rn)  ; ("right_recept",rr) ] in result

     end in

   (* Call the Dialog loop *)
   Talking_MATERIEL_SKEL.dialog_loop ~help:None dialog scan_dialog st

 ;; (* end of CABLE_NULLMODEM INSERT/UPDATE dialog *)


 (** The reaction for CABLE_NULLMODEM ELIMINATE user command. *)
 let react_elim (st:globalState) (msg: (string,string) env option) = 
   match msg with
   | Some r -> 
       let defects = get_defects_interface () in
       begin (* TO IMPLEMENT *)
         let answer = r#get("answer") in
         let name   = r#get("name")   in
         if (answer="yes") then begin 
           prerr_endline (myname^".react_elim: removing serial cable "^name);  
           (st#network#delCable name);
           st#refresh_sketch ();
           defects#remove_cable name;
           st#update_cable_sensitivity ();
         end
         else ()
       end 
   |  None -> raise (Failure "CABLE_NULLMODEM react_elim")

 ;; (* end of react_elim *)

 (** The dialog for CABLE_NULLMODEM ELIMINATE. *)
 let ask_confirm_cable_elim x = 
   let what = (Netmodel.string_of_cablekind Netmodel.NullModem) in
   EDialog.ask_question 
       ~gen_id:"answer"  
       ~title:"ELIMINER" 
       ~question:("Vous confirmez l'élimination du cable "^x^" ("^what^" cable) ?")
       ~help:None
       ~cancel:false 
 ;; (* end of ask_confirm_cable_elim *)


 (** CABLE_NULLMODEM bindings with the main window *)
 let bind (st:globalState) =
   
       Talking_MATERIEL_SKEL.bind st
         st#mainwin#imagemenuitem_SERIE_AJOUT "CABLE SERIE AJOUT"
         st#mainwin#imagemenuitem_SERIE_MODIF st#mainwin#imagemenuitem_SERIE_MODIF_menu "CABLE SERIE PROPRIÉTÉS"
         ask_cable 
         react_insert_update
         st#mainwin#imagemenuitem_SERIE_ELIM  st#mainwin#imagemenuitem_SERIE_ELIM_menu  "CABLE SERIE SUPPRIMER"
         ask_confirm_cable_elim
         react_elim

     st#mainwin#imagemenuitem_SERIE_STARTUP st#mainwin#imagemenuitem_SERIE_STARTUP_menu
     (fun st x () -> print_string ("react_startup: global-state >"^ x ^"<\n"))
     st#mainwin#imagemenuitem_SERIE_SHUTDOWN st#mainwin#imagemenuitem_SERIE_SHUTDOWN_menu
     (fun st x () -> print_string ("react_shutdown: global-state >"^ x ^"<\n"))
     st#mainwin#imagemenuitem_SERIE_POWEROFF st#mainwin#imagemenuitem_SERIE_POWEROFF_menu
     (fun st x () -> print_string ("react_poweroff: global-state >"^ x ^"<\n"))
     st#mainwin#imagemenuitem_SERIE_SUSPEND st#mainwin#imagemenuitem_SERIE_SUSPEND_menu
     (fun st x () -> print_string ("react_suspend: global-state >"^ x ^"<\n"))
     st#mainwin#imagemenuitem_SERIE_RESUME st#mainwin#imagemenuitem_SERIE_RESUME_menu
     (fun st x () -> print_string ("react_resume: global-state >"^ x ^"<\n"))

         (fun u -> st#network#getSerialCableNames)
         st#network#getCableByName

 ;;


end;; (* Talking_MATERIEL_CABLE_NULLMODEM *)


(* **************************************** *
      Module Talking_MATERIEL_NUAGE
 * **************************************** *)

(** Edialog construction, binding with the main window and associated reaction for
   the _MATERIEL_NUAGE functionnalities *)
module Talking_MATERIEL_NUAGE = struct         
 
 (** The name of this module (for debugging purposes) *)
 let  myname = "talking_MATERIEL_NUAGE" ;;

 (** The class for INSERT/UPDATE user command (deliver by the ask_cloud dialog)  *)
 class usercmd = fun (r: (string,string) env) -> object (self)
   method name      : string = r#get("name")
   method label     : string = r#get("label")
   method action    : string = r#get("action")
   method oldname   : string = r#get("oldname")
 end;; (* end of class usercmd *)

 (** The reaction for NUAGE INSERT/UPDATE user command. *)
 let react_insert_update (st:globalState) (msg: (string,string) env option) = match msg with
 | Some r -> 
     begin
       let cmd = (new usercmd r) in 
       prerr_endline (myname^".react: name      = "^cmd#name);
       prerr_endline (myname^".react: label     = "^cmd#label);
       prerr_endline (myname^".react: action    = "^cmd#action);
       prerr_endline (myname^".react: oldname   = "^cmd#oldname);
       let details = get_network_details_interface () in
       let defects = get_defects_interface () in
       begin
         match cmd#action with 
         | "add"    -> 
(*              details#add_device cmd#name "cloud" 2; *)
             defects#add_device ~defective_by_default:true cmd#name "cloud" 2;
             let c = (new Netmodel.cloud ~network:st#network ~name:cmd#name ~label:cmd#label ()) in 
             st#network#addCloud c;
             st#update_sketch () ;
             st#update_state  ();
             st#update_cable_sensitivity ();
             ()
         | "update" -> 
             let c = st#network#getCloudByName cmd#oldname in
             st#network#changeNodeName cmd#oldname cmd#name  ;
             st#refresh_sketch () ;
             st#update_state  ();
             c#destroy;
(*              details#rename_device cmd#oldname cmd#name; *)
             defects#rename_device cmd#oldname cmd#name;
             st#update_cable_sensitivity ();
             ()
         | _  -> raise (Failure (myname^".react: unexpected action"))
       end
     end
 | None -> 
     begin
       prerr_endline (myname^".react: NOTHING TO DO")
     end
 ;; (* end of react_insert_update NUAGE *)


 (** The dialog for NUAGE INSERT/UPDATE. *)
 let ask_cloud ~title ~(update:Netmodel.cloud option) (st:globalState) () =
 
   let dialog = new Gui.dialog_NUAGE ()  in 

   (* NUAGE Dialog definition *)

   dialog#toplevel#set_title (utf8 title);

   (* Visibility of gw2 and gw3 *)

   (* Set defaults. If we are updating, defaults are the old values. *)
   begin
   match update with
   | None   -> dialog#name#set_text (st#network#suggestedName "N"); 
               dialog#name#misc#grab_focus () 
   | Some c -> begin
                dialog#name#set_text  c#get_name  ;
                dialog#label#set_text c#get_label ;
               end
   end;

  
   (* NUAGE Dialog parser *)
   let scan_dialog () = 
     begin
     let n     = dialog#name#text                                                        in
     let l     = dialog#label#text                                                       in
     let (c,o) = match update with None -> ("add","") | Some c -> ("update",c#name)      in

     if not (Str.wellFormedName n) then
       raise EDialog.IncompleteDialog
     else
       mkenv [("name",n)  ; ("label",l)  ; ("action",c)   ; ("oldname",o)  ; ]
     end in

   (* Call the Dialog loop *)
   Talking_MATERIEL_SKEL.dialog_loop ~help:(Some Msg.help_cloud_insert_update) dialog scan_dialog st

 ;; (* end of NUAGE INSERT/UPDATE dialog *)


 (** The reaction for NUAGE ELIMINATE user command. *)
 let react_elim (st:globalState) (msg: (string,string) env option) = 
   match msg with
   | Some r -> 
       begin (* TO IMPLEMENT *)
         let answer = r#get("answer") in
         let name   = r#get("name")   in
         let details = get_network_details_interface () in
         let defects = get_defects_interface () in
         if (answer="yes") then begin 
          prerr_endline (myname^".react_elim: removing cloud "^name);  
          (st#network#delCloud name);
           st#refresh_sketch () ;
           (* details#remove_device name; *)
           defects#remove_device name;
           st#update_cable_sensitivity ();
         end
         else ()
       end 
   |  None -> raise (Failure "CLOUD react_elim")

 ;; (* end of react_elim *)

 (** The dialog for NUAGE ELIMINATE. *)
 let ask_confirm_cloud_elim x = 
   EDialog.ask_question 
       ~gen_id:"answer"  
       ~title:"ELIMINER" 
       ~question:("Vous confirmez l'élimination de "^x^"\net de tous le cables éventuellement branchés ?")
       ~help:None
       ~cancel:false 
 ;; (* end of ask_confirm_cloud_elim *)


 (** NUAGE bindings with the main window *)
 let bind (st:globalState) =
   
       Talking_MATERIEL_SKEL.bind st
         st#mainwin#imagemenuitem_NUAGE_AJOUT "RÉSEAU INDÉTERMINÉ AJOUT"
         st#mainwin#imagemenuitem_NUAGE_MODIF st#mainwin#imagemenuitem_NUAGE_MODIF_menu "RÉSEAU INDÉTERMINÉ PROPRIÉTÉS"
         ask_cloud 
         react_insert_update
         st#mainwin#imagemenuitem_NUAGE_ELIM  st#mainwin#imagemenuitem_NUAGE_ELIM_menu  "RÉSEAU INDÉTERMINÉ SUPPRIMER"
         ask_confirm_cloud_elim
         react_elim

     st#mainwin#imagemenuitem_NUAGE_STARTUP st#mainwin#imagemenuitem_NUAGE_STARTUP_menu
     (fun st name () -> let n = st#network#getCloudByName name in n#startup)
     st#mainwin#imagemenuitem_NUAGE_SHUTDOWN st#mainwin#imagemenuitem_NUAGE_SHUTDOWN_menu
     (fun st name () -> let n = st#network#getCloudByName name in n#gracefully_shutdown)
     st#mainwin#imagemenuitem_NUAGE_POWEROFF st#mainwin#imagemenuitem_NUAGE_POWEROFF_menu
     (fun st name () -> let n = st#network#getCloudByName name in n#poweroff)
     st#mainwin#imagemenuitem_NUAGE_SUSPEND st#mainwin#imagemenuitem_NUAGE_SUSPEND_menu
     (fun st name () -> let n = st#network#getCloudByName name in n#suspend)
     st#mainwin#imagemenuitem_NUAGE_RESUME st#mainwin#imagemenuitem_NUAGE_RESUME_menu
     (fun st name () -> let n = st#network#getCloudByName name in n#resume)

         (fun u -> st#network#getCloudNames)
         st#network#getCloudByName
 ;;


end;; (* Talking_MATERIEL_NUAGE *)




(* **************************************** *
      Module Talking_MATERIEL_GWINTERNET
 * **************************************** *)

(** Edialog construction, binding with the main window and associated reaction for
   the _MATERIEL_GWINTERNET functionnalities *)
module Talking_MATERIEL_GWINTERNET = struct         
 
 (** The name of this module (for debugging purposes) *)
 let  myname = "talking_MATERIEL_GWINTERNET" ;;

 (** The class for INSERT/UPDATE user command (deliver by the ask_gateway dialog)  *)
 class usercmd = fun (r: (string,string) env) -> object (self)
   method name      : string = r#get("name")
   method label     : string = r#get("label")
   method action    : string = r#get("action")
   method oldname   : string = r#get("oldname")
 end;; (* end of class usercmd *)

 (** The reaction for GWINTERNET INSERT/UPDATE user command. *)
 let react_insert_update (st:globalState) (msg: (string,string) env option) = match msg with
 | Some r -> 
     begin
       let cmd = (new usercmd r) in 
       prerr_endline (myname^".react: name      = "^cmd#name);
       prerr_endline (myname^".react: label     = "^cmd#label);
       prerr_endline (myname^".react: action    = "^cmd#action);
       prerr_endline (myname^".react: oldname   = "^cmd#oldname);
       let details = get_network_details_interface () in
       let defects = get_defects_interface () in
       begin
         match cmd#action with 
         | "add"    -> 
(*              details#add_device cmd#name "gateway" 1; *)
             defects#add_device cmd#name "gateway" 1;
             let g = (new Netmodel.gateway ~network:st#network ~name:cmd#name ~label:cmd#label ()) in 
             st#network#addGateway g;
             st#update_sketch () ;
             st#update_state  ();
             st#update_cable_sensitivity ();
             ()
         | "update" -> 
             let g = st#network#getGatewayByName cmd#oldname in
             st#network#changeNodeName cmd#oldname cmd#name  ;
             st#refresh_sketch () ;
             st#update_state  ();
             g#destroy;
(*              details#rename_device cmd#oldname cmd#name; *)
             defects#rename_device cmd#oldname cmd#name;
             st#update_cable_sensitivity ();
             ()
         | _  -> raise (Failure (myname^".react: unexpected action"))
       end
     end
 | None -> 
     begin
       prerr_endline (myname^".react: NOTHING TO DO")
     end
 ;; (* end of react_insert_update GWINTERNET *)


 (** The dialog for GWINTERNET INSERT/UPDATE. *)
 let ask_gateway ~title ~(update:Netmodel.gateway option) (st:globalState) () =
 
   let dialog = new Gui.dialog_GWINTERNET ()  in 

   (* GWINTERNET Dialog definition *)

   dialog#toplevel#set_title (utf8 title);

   (* Set defaults. If we are updating, defaults are the old values. *)
   begin
   match update with
   | None   -> dialog#name#set_text (st#network#suggestedName "E"); (* E stands for extern *) 
               dialog#name#misc#grab_focus () 
   | Some c -> begin
                dialog#name#set_text  c#get_name  ;
                dialog#label#set_text c#get_label ;
               end
   end;

  
   (* GWINTERNET Dialog parser *)
   let scan_dialog () = 
     begin
     let n     = dialog#name#text                                                        in
     let l     = dialog#label#text                                                       in
     let (c,o) = match update with None -> ("add","") | Some c -> ("update",c#name)      in

     let ip_1 = string_of_float dialog#ip_1#value                                      in
     let ip_2 = string_of_float dialog#ip_2#value                                      in
     let ip_3 = string_of_float dialog#ip_3#value                                      in
     let ip_4 = string_of_float dialog#ip_4#value                                      in
     let nm   = string_of_float dialog#nm#value                                        in

     if not (Str.wellFormedName n) then raise EDialog.IncompleteDialog  else

            mkenv [("name",n)  ; ("label",l)  ; ("action",c)   ; ("oldname",o)  ;    
                   ("ip_1",ip_1); ("ip_2",ip_2); ("ip_3",ip_3);  ("ip_4",ip_4); ("nm",nm) ]   
     end in

   (* Call the Dialog loop *)
   Talking_MATERIEL_SKEL.dialog_loop ~help:(Some Msg.help_socket_insert_update) dialog scan_dialog st

 ;; (* end of GWINTERNET INSERT/UPDATE dialog *)


 (** The reaction for GWINTERNET ELIMINATE user command. *)
 let react_elim (st:globalState) (msg: (string,string) env option) = 
   match msg with
   | Some r -> 
       let details = get_network_details_interface () in
       let defects = get_defects_interface () in
       begin (* TO IMPLEMENT *)
         let answer = r#get("answer") in
         let name   = r#get("name")   in
         if (answer="yes") then begin 
           prerr_endline (myname^".react_elim: removing gateway "^name);  
           (st#network#delGateway name); 
           st#update_sketch ();
           st#update_state  ();
(*            details#remove_device name; *)
           defects#remove_device name;
           st#update_cable_sensitivity ();
         end
         else ()
       end 
   |  None -> raise (Failure "GWINTERNET react_elim")

 ;; (* end of react_elim *)

 (** The dialog for GWINTERNET ELIMINATE. *)
 let ask_confirm_gateway_elim x = 
   EDialog.ask_question 
       ~gen_id:"answer"  
       ~title:"ELIMINER" 
       ~question:("Vous confirmez l'élimination de "^x^"\net de tous le cables éventuellement branchés ?")
       ~help:None
       ~cancel:false 
 ;; (* end of ask_confirm_gateway_elim *)


 (** GWINTERNET bindings with the main window *)
 let bind (st:globalState) =
   
       Talking_MATERIEL_SKEL.bind st
         st#mainwin#imagemenuitem_GWINTERNET_AJOUT "PRISE INTERNET AJOUT"
         st#mainwin#imagemenuitem_GWINTERNET_MODIF st#mainwin#imagemenuitem_GWINTERNET_MODIF_menu "PRISE INTERNET PROPRIÉTÉS"
         ask_gateway 
         react_insert_update
         st#mainwin#imagemenuitem_GWINTERNET_ELIM  st#mainwin#imagemenuitem_GWINTERNET_ELIM_menu "PRISE INTERNET SUPPRIMER"
         ask_confirm_gateway_elim
         react_elim

     st#mainwin#imagemenuitem_GWINTERNET_STARTUP st#mainwin#imagemenuitem_GWINTERNET_STARTUP_menu
     (fun st name () ->
       let g = st#network#getGatewayByName name in
       g#startup)
     st#mainwin#imagemenuitem_GWINTERNET_SHUTDOWN st#mainwin#imagemenuitem_GWINTERNET_SHUTDOWN_menu
     (fun st name () ->
       let g = st#network#getGatewayByName name in
       g#gracefully_shutdown)
     st#mainwin#imagemenuitem_GWINTERNET_POWEROFF st#mainwin#imagemenuitem_GWINTERNET_POWEROFF_menu
     (fun st name () ->
       let g = st#network#getGatewayByName name in
       g#poweroff)
     st#mainwin#imagemenuitem_GWINTERNET_SUSPEND st#mainwin#imagemenuitem_GWINTERNET_SUSPEND_menu
     (fun st name () ->
       let g = st#network#getGatewayByName name in
       g#suspend)
     st#mainwin#imagemenuitem_GWINTERNET_RESUME st#mainwin#imagemenuitem_GWINTERNET_RESUME_menu
     (fun st name () ->
       let g = st#network#getGatewayByName name in
       g#resume)

         (fun u -> st#network#getGatewayNames)
         st#network#getGatewayByName
 ;;


end;; (* Talking_MATERIEL_GWINTERNET *)




(* **************************************** *
      Module Talking_ADJUSTMENT
 * **************************************** *)

(** Edialog construction, binding with the main window and associated reaction for
   the ADJUSTMENT functionnalities *)
module Talking_ADJUSTMENT = struct         

 (** Binding function *)
 let bind (st:globalState) =
 
  let (win,opt,net) = (st#mainwin, st#dotoptions, st#network) in 

  (** Reaction for the iconsize adjustment *)
  let iconsize_react () = if opt#are_gui_callbacks_disable then () else
     begin
     let size = opt#read_gui_iconsize () in
     st#flash ~delay:4000 (utf8 ("Taille des icones fixée à la valeur "^size^" (default=large)"));     
     st#refresh_sketch () ;
     ()
     end in

  (** Reaction for the shuffle adjustment *)
  let shuffle_react () =
     begin
      opt#set_shuffler (List.shuffleIndexes (net#nodes));
      let namelist = net#getNodeNames => ( (List.permute opt#get_shuffler) || String.Text.to_string ) in
      st#flash ~delay:4000 (utf8 ("Icones réagencées de façon aléatoire : "^namelist));     
      st#refresh_sketch () ;
      ()
      end in

  (** Reaction for the unshuffle adjustment *)
  let unshuffle_react () =
     begin
      opt#reset_shuffler ();
      let namelist = (net#getNodeNames => String.Text.to_string) in 
      st#flash ~delay:4000 (utf8 ("Icones dans l'agencement prédéfini : "^namelist));     
      st#refresh_sketch () ;
      ()
      end in

  (** Reaction for the rankdir adjustments *)
  let rankdir_react x () =
     begin
      let old = st#dotoptions#rankdir in
      st#dotoptions#set_rankdir x; 
      let msg = match x with 
      | "TB" -> "Tracer les arcs du haut vers le bas (default)" 
      | "LR" -> "Tracer les arcs de la gauche vers la droite" 
      | _    -> "Not valid Rankdir" in 
      st#flash ~delay:4000 (utf8 msg);     
      if x<>old then st#refresh_sketch () ;
      ()
      end in

  (** Reaction for the nodesep adjustment *)
  let nodesep_react () = if opt#are_gui_callbacks_disable then () else
     begin
      let y = opt#read_gui_nodesep () in
      st#flash (utf8 ("Taille minimale des arcs (distance entre noeuds) fixées à la valeur "^
                     (string_of_float y)^" (default=0.5)"));     
      st#refresh_sketch () ;
      ()
     end in

  (** Reaction for the labeldistance adjustment *)
  let labeldistance_react () = if opt#are_gui_callbacks_disable then () else
     begin
      let y = opt#read_gui_labeldistance () in
      st#flash (utf8 ("Distance entre sommets et étiquettes fixée à la valeur "^
                     (string_of_float y)^" (default=1.6)"));     
      st#refresh_sketch () ;
      ()
     end in

  (** Reaction for the extrasize_x adjustment *)
  let extrasize_react () = if opt#are_gui_callbacks_disable then () else
     begin
      let x = () => (opt#read_gui_extrasize || int_of_float || string_of_int) in
      st#flash (utf8 ("Taille du canevas fixée à +"^x^
                      "% de la valeur suffisante à contenir le graphe (default=0%)"));     
      st#refresh_sketch () ;
      ()
     end in


  (** Reaction for a rotate adjustment *)
  let rotate_react (st:globalState) x () = 
     begin
      st#network#invertedCableToggle x ;
      st#flash (utf8 ("Cable "^x^" (re)inversé"));     
      st#refresh_sketch () ;
      ()
     end in


  (* Binding iconsize adjustment *)
  win#adj_iconsize#connect#value_changed        iconsize_react       => ignore ;
  win#adj_shuffle#connect#clicked               shuffle_react        => ignore ;
  win#adj_unshuffle#connect#clicked             unshuffle_react      => ignore ;
  win#adj_rankdir_TB#connect#clicked           (rankdir_react "TB")  => ignore ;
  win#adj_rankdir_LR#connect#clicked           (rankdir_react "LR")  => ignore ;
  win#adj_nodesep#connect#value_changed         nodesep_react        => ignore ;
  win#adj_labeldistance#connect#value_changed   labeldistance_react  => ignore ;
  win#adj_extrasize#connect#value_changed       extrasize_react      => ignore ;


 (** Generic binding for rotate menus. *)
 let bind_rotate_menu (st:globalState) what what_menu react_rotate getElementNames =

    let set_active cname = (List.mem cname st#network#invertedCables) in
   
    (Widget.DynamicSubmenu.make ~set_active  ~submenu:what_menu  ~menu:what  ~dynList:getElementNames
                               ~action:(fun x ->fun via -> react_rotate st x via) ()) ;   ()  in 


 (* Binding for DROIT *)
 bind_rotate_menu st 
     st#mainwin#imagemenuitem_ROTATE_DROIT 
     st#mainwin#imagemenuitem_ROTATE_DROIT_menu 
     rotate_react 
     (fun u -> st#network#getDirectCableNames) ;

 (* Binding for CROISE *)
 bind_rotate_menu st 
     st#mainwin#imagemenuitem_ROTATE_CROISE 
     st#mainwin#imagemenuitem_ROTATE_CROISE_menu 
     rotate_react
     (fun u -> st#network#getCrossedCableNames) ;

 (* Binding for SERIE *)
 bind_rotate_menu st 
     st#mainwin#imagemenuitem_ROTATE_SERIE
     st#mainwin#imagemenuitem_ROTATE_SERIE_menu 
     rotate_react
     (fun u -> st#network#getSerialCableNames) ;

  ()

 ;; (* end of Talking_ADJUSTMENT.bind *)


end;; (* Talking_ADJUSTMENT *)



(* **************************************** *
      Module Talking_BOTTOM_BUTTONS
 * **************************************** *)

(** Edialog construction, binding with the main window and associated reaction for
   the BOTTOM_BUTTONS functionnalities *)
module Talking_BOTTOM_BUTTONS = struct         
 (** Binding function *)
 let bind (st:globalState) =
 
  let (win,opt,net) = (st#mainwin, st#dotoptions, st#network) in 
  ignore (win#startup_everything#connect#clicked
    ~callback:(fun () -> startup_everything st ()));
  ignore (win#shutdown_everything#connect#clicked
    ~callback:(fun () ->
      match Simple_dialogs.confirm_dialog
          ~question:"Etes-vous sûr de vouloir arrêter\ntous les composants en exécution ?"
          () with
        Some true -> shutdown_everything st ()
      | Some false -> ()
(*      | None -> assert false));*)
      | None -> ()));
  ignore (win#poweroff_everything#connect#clicked
    ~callback:(fun () ->
      match Simple_dialogs.confirm_dialog
          ~question:("Etes-vous sûr de vouloir débrancher le courant\nà tous les composants en exécution (power off) ?\n\n"^
                     "Il est aussi possible de les arrêter gracieusement (shutdown)...")
          () with
        Some true -> poweroff_everything st ()
      | Some false -> ()
(*      | None -> assert false));;*)
      | None -> () ));;
end;; (* Talking_BOTTOM_BUTTONS *)

(* **************************************** *
           Module Talking_OTHER_STUFF
 * **************************************** *)

module Talking_OTHER_STUFF = struct
 (** Binding function *)
 let bind (st:globalState) =
   let (win,opt,net) = (st#mainwin, st#dotoptions, st#network) in 
   let autogenerate_ip_addresses =
     win#imgitem_autogenerate_ip_addresses in
   let workaround_wirefilter_problem =
     win#imgitem_workaround_wirefilter_problem in
   (* Also set the default here, so that we don't have to make the Glade interface
      coherent with the defaults in Global_options: *)
   autogenerate_ip_addresses#set_active
     Global_options.autogenerate_ip_addresses_default;
   workaround_wirefilter_problem#set_active
     Global_options.workaround_wirefilter_problem_default;
   (* Here come the actual bindings: *)
   let _ =
     autogenerate_ip_addresses#connect#toggled
       ~callback:(fun () ->
         Printf.printf "You toggled the option (IP)\n"; flush_all ();
         Global_options.set_autogenerate_ip_addresses
           autogenerate_ip_addresses#active) in
   let _ =
     workaround_wirefilter_problem#connect#toggled
       ~callback:(fun () ->
         Printf.printf "You toggled the option (wf)\n"; flush_all ();
         Global_options.set_workaround_wirefilter_problem
           workaround_wirefilter_problem#active) in
   ();;
end;; (* Talking_OTHER_STUFF *)
