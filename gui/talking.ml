(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007, 2009  Jean-Vincent Loddo
   Copyright (C) 2007, 2008  Luca Saiu
   Updated in 2009 by Luca Saiu

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

(** All dialogs are implemented here. This module provide the capability for user
    to talk with the application. Specifically, the name "Talking" stands here
    for "Talking with user". *)

#load "include_as_string_p4.cmo"
;;

(* Shortcuts *)
let mkenv = Environment.make_string_env ;;

(* **************************************** *
              Module MSG
 * **************************************** *)

open Gettext;;

(** Some tools for building simple help, error, warning and info dialogs *)
module Msg = struct

 (** I moved some stuff into simple_dialogs.ml. It's useful for lots of other
     modules, not only for talking. --L. *)

 (** Specific help constructors*)

 (** Why you have to choose a folder to work *)
 let help_repertoire_de_travail =
   let title = (s_ "CHOOSE A WORKING DIRECTORY") in
   let msg   = (s_ "Marionnet can use a directory of your choice for its temporary files. \
Every file created in the directory will be deleted at exit time. \
If the program is run from the Marionnet live DVD, you are advised to \
use a persistent directory (in /mnt/hd*), in order to not waste \
your system physical memory.") in Simple_dialogs.help title msg ;;

let help_machine_insert_update =
   let title = (s_ "ADD OR MODIFY A VIRTUAL MACHINE") in
   let msg   = (s_ "\
In this dialog window you can define the name of the virtual \
machine and set several hardware and software parameters.\n\n\
SECTION 'Hardware'\n\
- Memory: amount of memory (RAM) that will be reserved on \
the host for this virtual machine (default 48 Mb)\n\n\
- Ethernet Card: number of Ethernet cards (defalut 1)\n\n\
SECTION 'Software':\n\n\
- Distribution: the GNU/Linux distribution (Debian, Mandriva, Gentoo,..), \
chosen among those available in the filesystem directory\n\n\
- Variant: a variant (or patch) of the given distribution; a variant is a \
COW (Copy On Write) file that represents a small update of the used distribution.\
Available variants are in the variants/ subdirectory of the filesystem directory. \
You can make your own variants by exporting any virtual machine state in the 'Disks' \
tab.\n\n\
- Kernel: the Linux kernel version, chosen among the ones available in the kernels/ \
subdirectory\n\n\
SECTION 'UML':\n\n\
- Terminal: the possible choices are 'X HOST' and 'X NEST'; the first one \
allows to run graphic applications from a text terminal where the user \
can operate the virtual machine (with user 'root' and password 'root'); \
the second allows to have a real graphic server reserved for the virtual \
machine, with independent windows manager and desktops environments.")
   in Simple_dialogs.help title msg ;;

 let help_switch_insert_update =
   let title = (s_ "ADD OR MODIFY A SWITCH") in
   let msg   = (s_ "\
In this dialog window you can define the name of an Ethernet switch \
and set parameters for it:\n\n\
- Label: a string appearing near the switch icon in the network graph; it may \
allow, for example, to know at a glance the Ethernet network realized by the device; \
this field is exclusively for graphic purposes, is not taken in consideration \
for the configuration.\n\n\
- Nb of Ports: the number of ports of the switch (default 4); this number must \
not be increased without a reason, because the number of processes needed for the \
device emulation is proportional to his ports number.")
   in Simple_dialogs.help title msg ;;


 let help_device_insert_update = function
  | Mariokit.Netmodel.Switch -> help_switch_insert_update
  | _ -> ignore
 ;;

 let help_cable_direct_insert_update =
   let title = (s_ "ADD OR MODIFY A STRAIGHT CABLE") in
   let msg   = (s_ "\
In this dialog window you can define the name of a straight Ethernet cable \
and set its parameters:\n\n\
- Label: a string appearing near the edge representing the cable in the \
network graph\n\n\
- Devices: the two network devices (machine, hub, ...) linked by the cable and \
their two connected interfaces\n\n\
WARNING: this dialog allows to define straight cables even in contexts where \
they won't work (for example between two machines); allowing users to define 'wrong' \
connections may be of some pedagogical interest.")
   in Simple_dialogs.help title msg ;;

 let help_cable_crossover_insert_update =
   let title = (s_ "ADD OR MODIFY A CROSSOVER CABLE") in
   let msg   = (s_ "\
In this dialog window you can define the name of a crossover Ethernet cable \
and set its parameters:\n\n\
- Label: a string appearing near the edge representing the cable in the \
network graph\n\n\
- Devices: the two network devices (machine, hub, ...) linked by the cable and \
their two connected interfaces\n\n\
WARNING: this dialog allows to define crossover cables even in contexts where \
they won't work (for example between two machines); allowing users to define 'wrong' \
connections may be of some pedagogical interest.")
   in Simple_dialogs.help title msg ;;

 let help_cable_insert_update = function
  | Mariokit.Netmodel.Direct    -> help_cable_direct_insert_update
  | Mariokit.Netmodel.Crossover -> help_cable_crossover_insert_update
 ;;

 let help_cloud_insert_update =
   let title = (s_ "ADD OR MODIFY A CLOUD" )in
   let msg   = (s_ "In this dialog window you can define the name of a cloud. \
This component is an Ethernet network with an unknown internal \
structure introducing delays and other anomalies when packets \
pass through.\n\
Once the cloud is defined, use the tab 'Anomalies' to control delays, \
frame loss and the other anomalies.")
   in Simple_dialogs.help title msg ;;

 let help_world_bridge_insert_update =
   let title = (s_ "ADD OR MODIFY A WORLD BRIDGE") in
   (* TODO: rename "ethernet socket" => "world bridge" in all translations!*)
   let msg   = (s_ "\
In this dialog window you can define the name of an Ethernet socket \
and set parameters for it. This component allows to connect the virtual \
network to a Linux bridge whose name is defined by the user via the \
configuration variable called MARIONNET_BRIDGE (in marionnet.conf or provide on \
the command line).\n\n\
If the bridge is correctly set on the host (before starting the network), virtual \
machines will be able to access to the same network services (DHCP, DNS, NFS, \
...) that the host can access on its local network; if the host is on the Internet \
then also the virtual machines linked to the socket will be.\n \n \
To create a bridge on your (real) host using the same network as eth0 (by \
example) you need to : 1) create a bridge with the name define in marionnet.conf \
by MARIONNET_BRIDGE, 2) put and configure eth0 (on your real host) in the \
bridge and 3) put an IP address on the bridge (with dhclient or ifconfig/route).\n\n\
In such a case, after having start the virtual network in marionnet you can \
configure an ethernet card of a virtual machines which is connect to the \
Ethernet socket (or on the same network) in order to give access to your \
local network to it.\n\n \
The socket also allows team-work in a network laboratory, by creating a \
connection between Marionnet instances running on different machines. \
For more information about bridge et Ethernet socket configuration, please \
see the Marionnet Wiki on the marionnet.org website.")
   in Simple_dialogs.help title msg ;;

 let error_saving_while_something_up =
  Simple_dialogs.error
    (s_ "Warning")
   (s_ "The project can't be saved right now. \
One or more network components are still running. \
Please stop them before saving.")
 ;;

 (** Why you have to choose a name for your project *)
 let help_nom_pour_le_projet =
   let title = (s_ "CHOOSE A NAME FOR THE PROJECT") in
   let msg   = (s_ "\
Marionnet saves every files belonging to a project in a file with extension .mar. \
It is a standard gzipped tarball which can also be opened with standard tools.")
   in Simple_dialogs.help title msg ;;
end;; (* module Msg *)

(** Return the given pathname as it is, if it doesn't contain funny characters
    we don't want to bother supporting, like ' ', otherwise raise an exception.
    No check is performed on the pathname actual existence or permissions: *)
let check_pathname_validity pathname =
  if StrExtra.Bool.match_string "^[a-zA-Z0-9_\\/\\-]+$" pathname then
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
        (s_ "Invalid directory name")
        (Printf.sprintf (f_ "The name \"%s\" is not a valid directory.\n\nDirectory names \
must contain only letters, numbers, dashes ('-') and underscores ('_').") directory)
        ();
      failwith "the given directory name is invalid";
    end in
  let path_name = Filename.basename path_name in
  let check_chopped_basename_validity chopped_basename =
    if StrExtra.wellFormedName ~allow_dash:true chopped_basename then
      chopped_basename
    else begin
      Simple_dialogs.error
        (s_ "Invalid file name")
        (Printf.sprintf (f_ "The name \"%s\" is not a valid file name.\n\nA valid file \
name must start with a letter and can contain letters, numbers, dashes ('-') and underscores ('_').") chopped_basename)
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
        (s_ "Invalid file extension")
        (Printf.sprintf
           (f_ "The file \"%s\" must have an extension \"%s\", or no extension at all (in which case the extension \"%s\" will be added automatically).")
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
type env = string Environment.string_env
type edialog = unit -> env option

(** Dialog related exceptions. *)
exception BadDialog     of string * string;;
exception StrangeDialog of string * string * (string Environment.string_env);;
exception IncompleteDialog;;

(** The (and) composition of edialogs is again an env option *)
let rec compose (dl:edialog list) () (*: ((('a,'b) Environment.env) option)*) =
  match dl with
  | []  -> raise (Failure "EDialog.compose")
  | [d] -> d ()
  | d::l -> (match d () with
             | None   -> None
             | Some (r:env) -> (match (compose l ()) with
                          | None   -> None
                          | Some z -> Some (Environment.string_env_updated_by r z)
                          )
             )
;;

(** Alias for edialog composition *)
let sequence = compose;;

(** Auxiliary functions for file/folder chooser dialogs *)

let default d = function | None -> d | Some v -> v
;;

(** Filters  *)

let image_filter () =
  let f = GFile.filter ~name:"Images" () in
  f#add_custom [ `MIME_TYPE ]
    (fun info ->
      let mime = List.assoc `MIME_TYPE info in
      StringExtra.is_prefix "image/" mime) ;
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
      ~title
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
  (* All the intelligence of this method lies in the external script, loaded
     at preprocessing time: *)
  let content = INCLUDE_AS_STRING "scripts/can-directory-host-sparse-files.sh" in
  try
    match UnixExtra.script content [(check_pathname_validity pathname)] with
    | (0,_,_) -> true
    |   _     -> false
  with _ -> false
;;


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
           (s_ "Invalid directory")
           (s_ "Choose a directory which is existing, modifiable and hosted on a filesystem supporting sparse files (ext2, ext3, reiserfs, NTFS, ...)")
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

  let valid x =
    if (Sys.file_exists x)
    then ((Simple_dialogs.error
             (s_ "Name choice")
             (s_ "A file with the same name already exists!\n\nChoose another name for your file.")
             ()); false)
    else (Shell.freshname_possible x)
  in
  let result =
    ask_for_file ~enrich ~title ~valid ~filters ~action:`SAVE ~gen_id:"filename" ~help in
  result;;

(** The edialog asking for an existing filename. *)
let ask_for_existing_filename ?(enrich=mkenv []) ~title ?(filters = allfilters) ?(help=None) () =

  let valid = fun x ->
    if not (Sys.file_exists x)
    then ((Simple_dialogs.error
             (s_ "File choice")
             (s_ "The file doesn't exists!\nYou must choose an exiting file name.")
             ()); false)
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

   dialog#toplevel#set_title title;
   dialog#title_QUESTION#set_use_markup true;
   dialog#title_QUESTION#set_label question;
   ignore
     (dialog#toplevel#event#connect#delete
        ~callback:(fun _ -> Log.printf "Sorry, no, you can't close the dialog. Please make a decision.\n"; true));

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
