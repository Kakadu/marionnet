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


(** The main module of the application. Here the global state is defined, all
    bindings between widgets of the main window and dialogs are created, and
    finally the GTK main loop is launched. *)

open PreludeExtra.Prelude;; (* We want synchronous terminal output *)
open StdLabels;;
open Gui;;

open Environment;;
open State;;
open Talking;;
open Filesystem_history;;
open Network_details_interface;;
open Defects_interface;;
open Texts_interface;;

Random.self_init ();;

(** The global state containing the main window (st#mainwin) and all relevant dynamic 
    attributes of the application *)
let st = new globalState () ;;

(** Add a global thunk allowing to invoke the sketch refresh method, visible from many
    modules: *)
Mariokit.set_refresh_sketch_thunk
  (fun () -> st#refresh_sketch ());;

st#gui_coherence ();;

(* ***************************************** *
         Bindings for MENU's entries 
 * ***************************************** *)

(** Bindings for menu PROJET *)
Talking_PROJET_NOUVEAU.bind          st ;;
Talking_PROJET_OUVRIR.bind           st ;;
Talking_PROJET_FERMER.bind           st ;;
Talking_PROJET_IMPORTER_RESEAU.bind  st ;;
Talking_PROJET_EXPORTER_RESEAU.bind  st ;;
Talking_PROJET_EXPORTER_IMAGE.bind    st ;;
Talking_PROJET_QUITTER.bind          st ;;
Talking_PROJET_ENREGISTRER.bind      st ;;
Talking_PROJET_ENREGISTRER_SOUS.bind st ;;
Talking_PROJET_COPIER_SOUS.bind      st ;;

(** Bindings for menu OPTIONS *)
Talking_OPTIONS_CWD.bind    st ;;

(** Bindings for menu AIDE *)
Talking_PROJET_AIDE_APROPOS.bind    st ;;

(* ***************************************** *
       Bindings for MATERIEL's entries 
 * ***************************************** *)

(** Bindings for MACHINE *)

Talking_MATERIEL_MACHINE.bind                               st   ;;
Talking_MATERIEL_DEVICE.bind  Mariokit.Netmodel.Hub         st   ;;
Talking_MATERIEL_DEVICE.bind  Mariokit.Netmodel.Switch      st   ;;
Talking_MATERIEL_DEVICE.bind  Mariokit.Netmodel.Router      st   ;;

Talking_MATERIEL_CABLE_RJ45.bind  Mariokit.Netmodel.Direct  st   ;;
Talking_MATERIEL_CABLE_RJ45.bind  Mariokit.Netmodel.Crossed st   ;;
Talking_MATERIEL_CABLE_NULLMODEM.bind                       st   ;;
Talking_MATERIEL_NUAGE.bind                                 st   ;;
Talking_MATERIEL_GWINTERNET.bind                            st   ;;

Talking_ADJUSTMENT.bind                                     st   ;;

(* Debugging *)
let _ = st#mainwin#imgitem_DEBUG_TESTER_SCRIPTS#connect#activate ~callback:(fun _ ->st#network#show);;
(*let _ = st#mainwin#imgitem_DEBUG_TESTER_SCRIPTS#connect#activate ~callback:(fun _ ->prerr_endline (st#network#dotTrad st#dotoptions));;
*)
(*
let _ = st#mainwin#imgitem_DEBUG_TESTER_SCRIPTS#connect#activate ~callback:(fun _ ->prerr_endline (st#network#xml));;
*)
(* ***************************************** *
            Other 'global' bindings 
 * ***************************************** *)
Talking_BOTTOM_BUTTONS.bind st   ;;
Talking_OTHER_STUFF.bind st   ;;

(* ***************************************** *
            Make the treeview widgets
 * ***************************************** *)
(** Make the states interface: *)
let filesystem_history_interface =
  Filesystem_history.make_states_interface
    ~packing:(st#mainwin#filesystem_history_viewport#add)
    ();;
(** See the comment in states_interface.ml for why we need this ugly kludge: *)
Filesystem_history.set_startup_functions
  (fun name ->
    let node = st#network#getNodeByName name in
    node#can_startup)
  (fun name ->
    let node = st#network#getNodeByName name in
    node#startup);;

let shutdown_or_restart_relevant_device device_name =
  Printf.printf "Shutdown or restart \"%s\".\n" device_name;
  flush_all ();
  try
    (* Is the device a cable? If so we have to restart it (and do nothing if it
       was not connected) *)
    let c = st#network#getCableByName device_name in
    if c#is_connected then begin
      c#suspend; (* disconnect *)
      c#resume;  (* re-connect *)
    end
  with _ -> begin
    (* Ok, the device is not a cable. We have to destroy it, so that its cables
       and hublets are restarted: *)
    let d = st#network#getNodeByName device_name in
    d#destroy;
  end;;

(** Make the network details interface: *)
let network_details_interface =
  make_network_details_interface
    ~packing:(st#mainwin#network_details_viewport#add)
    ~after_user_edit_callback:shutdown_or_restart_relevant_device
    ();;
(** Make the defects interface: *)
let defects_interface =
  make_defects_interface
    ~packing:(st#mainwin#defects_viewport#add)
    ~after_user_edit_callback:shutdown_or_restart_relevant_device
    ();;
(** Make the texts interface: *)
let texts_interface =
  make_texts_interface
    ~packing:(st#mainwin#texts_viewport#add)
    ();;
(* ***************************************** *
                   M A I N
 * ***************************************** *)

(** Ignore some signals (for instance CTRL-C *)
(* List.iter (fun x -> (Sys.set_signal x  Sys.Signal_ignore)) [1;2;3;4;5;6;10;12;15] ;; *)

(** Timeout for refresh the state_coherence *)
(* let id = GMain.Timeout.add ~ms:1000 ~callback:(fun () -> st#state_coherence ();true) ;; *)

print_string "Starting the application\n";;

(* GMain.Main.main ();; *)

(* let () = ignore (GtkMain.Main.init ());; *)
(* let guiThread = GtkThread.start () in (\* start GUI thread *\)  *)
(* Thread.join guiThread;; *)

(try
  Daemon_client.initialize_daemon_client ();
  Daemon_client.start_thread_sending_keepalives ();
with e -> begin
  Daemon_client.disable_daemon_support ();
  Simple_dialogs.warning
    "FRENCH Could not connect to the daemon"
    (Printf.sprintf
       "FRENCH Connecting to the Marionnet daemon failed (%s); Marionnet will work, but some features (graphics on virtual machines and host sockets) won't be available."
       (Printexc.to_string e))
    ();
end);;

(** Show the splash: *)
Splash.show_splash (* ~timeout:15000 *) ();;

(** Choose a reasonable working directory: *)
(if Shell.dir_comfortable "/tmp" then
  st#set_wdir "/tmp"
else if Shell.dir_comfortable "~/tmp" then
  st#set_wdir "~/tmp"
else
  failwith "Please create either /tmp or your home directory on some reasonable modern filesystem supporting sparse files");;

(* Check that we're *not* running as root. Yes, this has been reversed
   since the last version: *)
Printf.printf "Checking whether Marionnet is running as root...\n";;
if (Unix.getuid ()) = 0 then begin
  Printf.printf "\n**********************************************\n";
  Printf.printf "* Marionnet should *not* be run as root, for * \n";
  Printf.printf "* security reasons.                          *\n";
  Printf.printf "* Continuing anyway...                       *\n";
  Printf.printf "**********************************************\n\n";
  Simple_dialogs.warning
    "FRENCH You should not be root!"
    "FRENCH Marionnet is running with UID 0; this is bad from a security point of view.
[To do: write a longer, cleaner message...]
Continuing anyway."
    ();
end;;

(** Enter the GTK+ main loop: *)
GtkThread.main ();;
