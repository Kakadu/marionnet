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

(* Shortcuts *)
let mkenv = Environment.make_string_env ;;

let commit_suicide signal =
  raise Exit;;

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

 let help_cable_crossover_insert_update =
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
  | Netmodel.Direct    -> help_cable_direct_insert_update
  | Netmodel.Crossover -> help_cable_crossover_insert_update
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

(** Check that the given pathname is acceptable, and that it has the correct extension or
    no extension; if the argument has the corret extension then just return it; it it's
    otherwise valid but has no extension then return the argument with the extension
    appended; if it's invalid or has a wrong extension then show an appropriate
    error message and raise an exception.
    This function is thought as a 'filter' thru which user-supplied filenames should
    be always sent before use. The optional argument extension should be a string with
    *no* dot *)
let check_path_name_validity_and_add_extension_if_needed ?(extension="mar") path_name =
  let directory = Filename.dirname path_name in
  let correct_extension = "." ^ extension in
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
  let check_chopped_basename_validity chopped_basename =
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
  if Filename.check_suffix path_name correct_extension then
    (* path_name does end with the correct extension; just check that its chopped version is ok: *)
    Printf.sprintf
      "%s/%s%s"
      directory
      (check_chopped_basename_validity (Filename.chop_extension path_name))
      correct_extension
  else
    (* path_name doesn't end with the correct extension: *)
    try
      let _ = Filename.chop_extension path_name in
      (* There is an extension but it's not the correct one; fail: *)
      Simple_dialogs.error
        "Extension de fichier non admise"
        (Printf.sprintf
           "Le fichier \"%s\" doit avoir une extension \"%s\", ou aucune (dans ce cas l'extension \"%s\" sera automatiquement ajoutée)."
           path_name
           correct_extension
           correct_extension)
        ();
      failwith ("the given file name has an extension but it's not \"" ^ correct_extension ^ "\".");
    with Invalid_argument _ ->
      (* There is no extension; just check that the filename is otherwise valid, and
         add the extension: *)
      Printf.sprintf
        "%s/%s%s"
        directory
        (check_chopped_basename_validity path_name)
        correct_extension;;


(* **************************************** *
              Module EDialog
 * **************************************** *)


(** An EDialog (for Environnemnt Dialog) is a dialog which may returns an environnement in the
    form (id,value) suitable for functions implementing reactions *)
module EDialog = struct

(** An edialog is a dialog which returns an env as result if succeed *)
type edialog = unit -> ((string string_env) option) ;;

(** Dialog related exceptions. *)
exception BadDialog     of string * string;;
exception StrangeDialog of string * string * (string string_env);;
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
let jpeg_filter   () = GFile.filter ~name:"JPEG files (*.jpg *.jpeg)" ~patterns:[ "*.jpg"; "*.JPG"; "*.jpeg"; "*.JPEG" ] ();;
let png_filter    () = GFile.filter ~name:"PNG files (*.png)" ~patterns:[ "*.png"; "*.PNG" ] () ;;

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

    ?(enrich=mkenv [])
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
  ignore (dialog#set_current_folder (Initialization.cwd_at_startup_time));

  dialog#set_default_response `OK;

  if (action=`SELECT_FOLDER)        then (try (dialog#add_shortcut_folder "/tmp") with _ -> ());
  if (action=`OPEN or action=`SAVE) then (List.iter (fun x -> dialog#add_filter (fun_filter_of x)) filters);
  let result = (ref None) in
  let cont   = ref true in
  while (!cont = true) do
  begin match dialog#run () with
  | `OK -> (match dialog#filename with
              | None   -> ()
              | Some fname -> if (valid fname) then
                              begin cont := false; enrich#add (gen_id,fname); result := Some enrich end
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
let ask_for_existing_writable_folder_pathname_supporting_sparse_files
 ?(enrich=mkenv [])
 ?(help=None)
 ~title
 () =
  let valid = fun pathname ->
    if (not (Sys.file_exists pathname)) or
       (not (Shell.dir_comfortable pathname)) or
       (not (does_directory_support_sparse_files pathname)) then
        begin
         Simple_dialogs.error
          "Répertoire inexploitable"
          "Vous devez choisir un répertoire existant, modifiable et résidant sur un système de fichiers supportant les fichiers 'sparse' (ext2, ext3, reiserfs, NTFS, ...)"
          ();
         false;
        end
    else true
  in ask_for_file ~enrich ~title ~valid ~filters:[] ~action:`SELECT_FOLDER ~gen_id:"foldername" ~help () ;;


(** The edialog asking for a fresh and writable filename. *)
let ask_for_fresh_writable_filename
 ?(enrich=mkenv [])
 ~title
 ?(filters = allfilters)
 ?(help=None) =

  let valid = fun x ->
    if (Sys.file_exists x)
    then ((Simple_dialogs.error "Choix du nom" "Un fichier de même nom existe déjà!\n\nVous devez choisir un nouveau nom pour votre fichier." ()); false)
    else (Log.print_endline ("valid: x="^x) ; (Shell.freshname_possible x)) in

  let result =
    ask_for_file ~enrich ~title ~valid ~filters ~action:`SAVE ~gen_id:"filename" ~help in
  result;;

(** The edialog asking for an existing filename. *)
let ask_for_existing_filename ?(enrich=mkenv []) ~title ?(filters = allfilters) ?(help=None) () =

  let valid = fun x ->
    if not (Sys.file_exists x)
    then ((Simple_dialogs.error "Choix du fichier" "Le fichier n'existe pas!\nVous devez choisir un nom de fichier existant." ()); false)
    else (Shell.regfile_modifiable x) in

  ask_for_file ~enrich ~title ~valid ~filters ~action:`OPEN ~gen_id:"filename" ~help ()
;;

(** Generic constructor for question dialogs.
    With the 'enrich' optional parameter the dialog can enrich a given environnement. Otherwise
    it creates a new one. *)
let ask_question ?(enrich=mkenv []) ?(title="QUESTION") ?(gen_id="answer")  ?(help=None) ?(cancel=false) ~(question:string)  () =

   let dialog=new Gui.dialog_QUESTION () in

   if (help=None)    then () else dialog#toplevel#add_button_stock `HELP   `HELP ;
   if (cancel=false) then () else dialog#toplevel#add_button_stock `CANCEL `CANCEL ;

   dialog#toplevel#set_title (utf8 title);
(*   dialog#title#set_text     (utf8 (String.uppercase question));*)
   dialog#title_QUESTION#set_text     (utf8 question);
   dialog#title_QUESTION#set_use_markup true;
   ignore
     (dialog#toplevel#event#connect#delete
        ~callback:(fun _ -> Log.print_string "Sorry, no, you can't close the dialog. Please make a decision.\n"; true));

   let result = (ref None) in
   let cont   = ref true in
   while (!cont = true) do
     match dialog#toplevel#run () with
     | `YES  -> begin cont := false; enrich#add (gen_id,"yes"); result := Some enrich end
     | `NO   -> begin cont := false; enrich#add (gen_id,"no" ); result := Some enrich end
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
        Module Talking_PROJECT_NEW
 * **************************************** *)

module Talking_PROJECT_NEW = struct

 (** The name of this module (for debugging purposes) *)
 let  myname = "talking_PROJECT_NEW" ;;

 (** The type of command provided by user by this graphical dialog *)
 class usercmd = fun (r: (string,string) env) -> object (self)
   method filename     : string = r#get "filename"
   method save_current : bool   = (r#get("save_current") = "yes")
   method log ~header =
     begin
       Log.print_endline (header^"filename      = "^self#filename);
       Log.print_endline (header^"save_current  = "^(string_of_bool self#save_current));
     end
 end;;

 (** The correspondent reaction of the application for the command given by user *)
 let react (st:globalState) (msg: (string,string) env option) =
   match msg with
   | None -> Log.print_endline (myname^".react: NOTHING TO DO")
   | Some r ->
     begin
     try
       st#shutdown_everything ();
       let cmd = (new usercmd r) in
       cmd#log ~header:(myname^".react: ");
       let fname = check_path_name_validity_and_add_extension_if_needed cmd#filename in
       Log.print_endline (myname^".react: fname="^fname);
       (if (st#active_project) && (cmd#save_current) then st#save_project ());
       st#close_project () ;
       st#new_project fname ;
       st#mainwin#window_MARIONNET#set_title (Command_line.window_title ^ " - " ^ fname);
     with | _ -> raise (Failure (myname^".react: unexpected environnement received from dialogs"))
     end
 ;;

 let callback (st:globalState) =
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

   fun () -> react st ((EDialog.sequence [confirm; ask_filename]) ())
  ;;

end;; (* Talking_PROJECT_NEW *)




(* **************************************** *
        Module Talking_PROJET_OPEN
 * **************************************** *)

module Talking_PROJECT_OPEN = struct

 (** The name of this module (for debugging purposes) *)
 let  myname = "talking_PROJECT_OPEN" ;;

 (** The type of command provided by user by this graphical dialog *)
 class usercmd = Talking_PROJECT_NEW.usercmd ;;

 (** The correspondent reaction of the application for the command given by user *)
 let react (st:globalState) (msg: (string,string) env option) = match msg with
 | None   ->  Log.print_endline (myname^".react: NOTHING TO DO")
 | Some r ->
     begin
     try
       st#shutdown_everything ();
       let cmd = (new usercmd r) in
       cmd#log ~header:(myname^".react: ");
       if (st#active_project) && (cmd#save_current) then st#save_project ();
       Log.print_endline ("*** in react open project: now call close_project");
       st#close_project () ;
       begin
         try
           st#open_project cmd#filename;
           st#mainwin#window_MARIONNET#set_title (Command_line.window_title ^ " - " ^ cmd#filename);
         with e -> (Simple_dialogs.error "OUVRIR UN PROJET" ("Erreur en ouvrant le fichier "^cmd#filename) ()); raise e
       end;
     with e -> raise e
     end
 ;;

 (** Performs the binding with the main window *)
 let callback (st:globalState) =

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

   fun token -> react st ((EDialog.sequence [confirm; ask_filename]) token)
  ;;

end;; (* Talking_PROJET_OPEN *)



(* **************************************** *
      Module Talking_PROJECT_SAVE
 * **************************************** *)

module Talking_PROJECT_SAVE = struct

 let callback (st:globalState) () =
   if st#is_there_something_on_or_sleeping ()
	then Msg.error_saving_while_something_up ()
        else st#save_project ();;
end;;


(* **************************************** *
    Module Talking_PROJET_SAVE_AS
 * **************************************** *)

module Talking_PROJECT_SAVE_AS = struct

 (** The name of this module (for debugging purposes) *)
 let  myname = "talking_PROJECT_SAVE_AS" ;;

 (** The type of command provided by user by this graphical dialog *)
 class usercmd = fun (r: (string,string) env) -> object (self)
   method filename : string = check_path_name_validity_and_add_extension_if_needed (r#get "filename")
   method log ~header = Log.print_endline (header^"filename = "^self#filename);
 end;;

 (** The correspondent reaction of the application for the command given by user *)
 let react (st:globalState) (msg: (string,string) env option) = match msg with (* TO IMPLEMENT *)
 | None   -> Log.print_endline (myname^".react: NOTHING TO DO")
 | Some r ->
     if st#is_there_something_on_or_sleeping () then Msg.error_saving_while_something_up ()
     else begin
       try
         let cmd = (new usercmd r) in
         cmd#log ~header:(myname^".react: ");
         begin
           try
             st#save_project_as cmd#filename;
             st#mainwin#window_MARIONNET#set_title (Command_line.window_title ^ " - " ^ cmd#filename);
           with _ -> (Simple_dialogs.error "PROJET ENREGISTRER SOUS" ("Échéc de la sauvegarde du project "^cmd#filename) ())
         end
       with | _ -> raise (Failure (myname^".react: unexpected environnement received from dialog"))
     end
 ;;


 let callback (st:globalState) =

   let ask_filename () = EDialog.ask_for_fresh_writable_filename
       ~title:"ENREGISTRER SOUS"
       ~filters:[EDialog.MAR;EDialog.ALL]
       ~help:(Some Msg.help_nom_pour_le_projet) () in

   fun token -> react st ((EDialog.compose [ask_filename]) token)
   ;;

end;; (* Talking_PROJECT_SAVE_AS *)



(* **************************************** *
    Module Talking_PROJET_COPY_INTO
 * **************************************** *)

module Talking_PROJECT_COPY_INTO = struct

 (** The name of this module (for debugging purposes) *)
 let  myname = "talking_PROJECT_COPY_INTO" ;;

 (** The type of command provided by user by this graphical dialog *)
 class usercmd = Talking_PROJECT_SAVE_AS.usercmd ;;

 (** The correspondent reaction of the application for the command given by user *)
 let react (st:globalState) (msg: (string,string) env option) = match msg with (* TO IMPLEMENT *)
 | None   -> Log.print_endline (myname^".react: NOTHING TO DO")
 | Some r ->
     if st#is_there_something_on_or_sleeping () then Msg.error_saving_while_something_up ()
     else begin
       try
         let cmd = (new usercmd r) in
         cmd#log ~header:(myname^".react: ");
         begin
           try
             st#copy_project_into cmd#filename
           with _ -> (Simple_dialogs.error "PROJECT COPY INTO" ("Error copying the project into "^cmd#filename) ())
         end
       with | _ -> raise (Failure (myname^".react: unexpected environnement received from dialog"))
     end
 ;;


 let callback (st:globalState) =

   let ask_filename () = EDialog.ask_for_fresh_writable_filename
       ~title:"COPIER SOUS"
       ~filters:[EDialog.MAR;EDialog.ALL]
       ~help:(Some Msg.help_nom_pour_le_projet) () in

   fun token -> react st ((EDialog.compose [ask_filename]) token)
 ;;

end;; (* Talking_PROJET_COPY_INTO *)



(* **************************************** *
        Module Talking_PROJECT_CLOSE
 * **************************************** *)


module Talking_PROJECT_CLOSE = struct

 (** The name of this module (for debugging purposes) *)
 let  myname = "talking_PROJECT_CLOSE" ;;

 (** The type of command provided by user by this graphical dialog *)
 class usercmd = fun (r: (string,string) env) -> object (self)
   method answer  : string = r#get("answer")
   method log ~header = Log.print_endline (header^"answer = "^self#answer)
 end;;

 (** The correspondent reaction of the application for the command given by user *)
 let react (st:globalState) (msg: (string,string) env option) = match msg with
 | None   -> Log.print_endline (myname^".react: NOTHING TO DO")
 | Some r ->
     begin
     try
       st#shutdown_everything ();
       let cmd = (new usercmd r) in
       cmd#log ~header:(myname^".react: ");
       if (st#active_project) && (cmd#answer="yes") then st#save_project ();
       st#close_project ();
       st#mainwin#window_MARIONNET#set_title Command_line.window_title; (* no project name *)
     with | _ -> raise (Failure (myname^".react: unexpected environnement received from dialog"))
     end
 ;;


 let callback (st:globalState) =

   let confirm () = EDialog.ask_question
       ~title:"FERMER"
       ~question:"Voulez-vous enregistrer le projet courant ?"
       ~help:None
       ~cancel:true
       () in

   fun token -> react st ((EDialog.compose [confirm]) token)
 ;;

end;; (* Talking_PROJECT_CLOSE *)


(* **************************************** *
    Module Talking_PROJECT_EXPORT_NETWORK_IMAGE
 * **************************************** *)

module Talking_PROJECT_EXPORT_NETWORK_IMAGE = struct

 (** The name of this module (for debugging purposes) *)
 let  myname = "Talking_PROJECT_EXPORT_NETWORK_IMAGE" ;;

 (** The correspondent reaction of the application for the command given by user *)
 let react (st:globalState) (msg: (string,string) env option) = match msg with
 | None   -> Log.print_endline (myname^".react: NOTHING TO DO")
 | Some r ->
     let filename =
       check_path_name_validity_and_add_extension_if_needed ~extension:"png" (r#get "filename") in
     Log.print_endline ("talking_PROJET_EXPORTER_IMAGE.react: filename="^filename);
     begin
       try
         let command = ("cp "^st#pngSketchFile^" "^filename) in
         begin
         Log.print_string "About to call Unix.run...\n"; flush_all ();
         match Unix.run command with
         |  (_ , Unix.WEXITED 0) -> st#flash ~delay:6000 (utf8 ("Image du réseau exportée avec succès dans le fichier "^filename))
         |  _                    -> raise (Failure ("Echec durant l'exportation de l'image du réseau vers le fichier "^filename))
         end
       with e -> (Simple_dialogs.error "EXPORTER L'IMAGE DU RESEAU" ("Echec durant l'exportation vers le fichier "^filename) ()); raise e
     end
 ;;

 (** Performs the binding with the main window *)
 let callback (st:globalState) =

   let ask_filename () = EDialog.ask_for_fresh_writable_filename
       ~title:"EXPORTER L'IMAGE DU RESEAU"
       ~filters:[EDialog.PNG;EDialog.ALL]
       ~help:None () in

   fun token -> react st ((EDialog.sequence [ask_filename]) token)
  ;;

end;; (* Talking_PROJECT_EXPORT_NETWORK_IMAGE *)


(* **************************************** *
        Module Talking_PROJECT_QUIT
 * **************************************** *)


module Talking_PROJECT_QUIT = struct

 (** The name of this module (for debugging purposes) *)
 let  myname = "talking_PROJECT_QUIT" ;;

 (** The type of command provided by user by this graphical dialog *)
 class usercmd = fun (r: (string,string) env) -> object
   method answer  : string = r#get("answer")
 end;;

 let quit (st:globalState) =
   Log.print_string ">>>>>>>>>>QUIT: BEGIN<<<<<<<<\n";
   (* Shutdown all devices and synchronously wait until they actually terminate: *)
   st#shutdown_everything ();
   Task_runner.the_task_runner#wait_for_all_currently_scheduled_tasks;
   st#close_project (); (* destroy the temporary project directory *)
   Log.print_endline (myname^".react: Calling mrPropre...");
   st#mrPropre ();
   GMain.Main.quit (); (* Finalize the GUI *)

   Log.print_string "Killing the task runner thread...\n";
   Task_runner.the_task_runner#terminate;
   Log.print_string "Killing the death monitor thread...\n";
   Death_monitor.stop_polling_loop ();
   Log.print_string "Killing the blinker thread...\n";
   st#network#ledgrid_manager#kill_blinker_thread;
   Log.print_string "...ok, the blinker thread was killed (from talking.ml).\n";
   Log.print_string "Sync, then kill our process (To do: this is a very ugly kludge)\n";
   flush_all ();
   Log.print_string "Synced.\n";
   (* install_signal_handler Sys.sigint;
      install_signal_handler Sys.sigterm; *)
   commit_suicide Sys.sigkill; (* this always works :-) *)
   Log.print_string "!!! This should never be shown.\n";;

 (** The correspondent reaction of the application for the command given by user *)
 let react (st:globalState) (msg: (string,string) env option) =
   Log.print_string ">>>>>>>>>>QUITTING: THERE SHOULD BE NOTHING BEFORE THIS<<<<<<<<\n";
   begin
     match msg with
     | Some r ->
         begin
           try
             let cmd = (new usercmd r) in
             Log.print_endline (myname^".react: answer="^cmd#answer);
             if (st#active_project) && (cmd#answer="yes") then
               st#save_project ();
             quit st;
           with | _ -> raise (Failure (myname^".react: unexpected environnement received from dialog"))
         end
     | None -> begin
         Log.print_string ">>>>>>>>>>*NOT* QUITTING: the user chose 'cancel'<<<<<<<<\n";
         Log.print_endline (myname^".react: NOTHING TO DO")
       end
   end;
 ;;

 let make_callback (st:globalState) ~user_can_cancel =
  let confirm () = EDialog.ask_question
       ~title:"QUITTER"
       ~question:"Voulez-vous enregistrer\nle projet courant avant de quitter ?"
       ~help:None
       ~cancel:user_can_cancel
       ()
   in
    fun token ->
       if (st#active_project)
        then react st ((EDialog.compose [confirm]) token)
        else react st (Some (mkenv [("answer","no")]))
 ;;

 let callback (st:globalState) = make_callback st ~user_can_cancel:true
 ;;

end;; (* Talking_PROJECT_QUIT *)
