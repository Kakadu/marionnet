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

(* Force OCAMLRUNPARAM=-b *)
Printexc.record_backtrace true;

open StdLabels
open Gui
open Gettext
open State
open Talking

let () = Random.self_init ()

(** The global state containing the main window (st#mainwin) and all relevant dynamic
    attributes of the application *)
let st = new globalState ()

(** Add a global thunk allowing to invoke the sketch refresh method, visible from many
    modules: *)
let () = User_level.Refresh_sketch_thunk.set (fun () -> st#refresh_sketch ())
let () = st#gui_coherence ()

module State     = struct let st = st end

(* Complete the main menu *)
module Created_window_MARIONNET   = Gui_window_MARIONNET.   Make (State)
module Created_toolbar_COMPONENTS = Gui_toolbar_COMPONENTS. Make (State)

(* ***************************************** *
            Make the treeview widgets
 * ***************************************** *)
(** Make the states interface: *)
let filesystem_history_interface =
  Treeview_history.make
    ~packing:(st#mainwin#filesystem_history_viewport#add)
    ~after_user_edit_callback:(fun _ -> st#set_project_not_already_saved)
    ()

(** See the comment in states_interface.ml for why we need this ugly kludge: *)
let () = 
 let can_startup =
   (fun name ->
      let node = st#network#get_node_by_name name in
      node#can_startup)
 in
 let startup = 
   (fun name ->
      let node = st#network#get_node_by_name name in
      node#startup)
 in
 Treeview_history.Startup_functions.set (can_startup, startup)

let shutdown_or_restart_relevant_device device_name =
  Log.printf "Shutdown or restart \"%s\".\n" device_name;
  try
    (* Is the device a cable? If so we have to restart it (and do nothing if it
       was not connected) *)
    let c = st#network#get_cable_by_name device_name in
    if c#is_connected then begin
      c#suspend; (* disconnect *)
      c#resume;  (* re-connect *)
    end
  with _ -> begin
    (* Ok, the device is not a cable. We have to destroy it, so that its cables
       and hublets are restarted: *)
    let d = st#network#get_node_by_name device_name in
    d#destroy_my_simulated_device;
  end

let after_user_edit_callback x =
  begin
    st#set_project_not_already_saved;
    shutdown_or_restart_relevant_device x
  end
 
(** Make the ifconfig treeview: *)
let treeview_ifconfig =
  Treeview_ifconfig.make
    ~packing:(st#mainwin#ifconfig_viewport#add)
    ~after_user_edit_callback
    ()

(** Make the defects interface: *)
let treeview_defects =
  Treeview_defects.make
    ~packing:(st#mainwin#defects_viewport#add)
    ~after_user_edit_callback
    ()

(** Make the texts interface: *)
let treeview_documents =
  Treeview_documents.make
    ~packing:(st#mainwin#documents_viewport#add)
    ~after_user_edit_callback:(fun _ -> st#set_project_not_already_saved)
    ()

(* ***************************************** *
                   M A I N
 * ***************************************** *)

(** Ignore some signals (for instance CTRL-C *)
(* List.iter (fun x -> (Sys.set_signal x  Sys.Signal_ignore)) [1;2;3;4;5;6;10;12;15] ;; *)

(** Timeout for refresh the state_coherence *)
(* let id = GMain.Timeout.add ~ms:1000 ~callback:(fun () -> st#state_coherence ();true) ;; *)

let () = Log.printf "Starting the application\n"

(* GMain.Main.main ();; *)

(* let () = ignore (GtkMain.Main.init ());; *)
(* let guiThread = GtkThread.start () in (\* start GUI thread *\)  *)
(* Thread.join guiThread;; *)
let () =
(try
  Daemon_client.initialize_daemon_client ();
  Daemon_client.start_thread_sending_keepalives ();
with e -> begin
  Daemon_client.disable_daemon_support ();
  Simple_dialogs.warning
    (s_ "Could not connect to the daemon")
    (Printf.sprintf
       (f_ "Connecting to the Marionnet daemon failed (%s); Marionnet will work, but some features (graphics on virtual machines and host sockets) won't be available.")
       (Printexc.to_string e))
    ();
end)

(** Show the splash: *)
let () = Splash.show_splash (* ~timeout:15000 *) ()

(** Choose a reasonable temporary working directory: *)
let () = (if Shell.dir_comfortable "/tmp" then
  st#temporary_directory#set "/tmp"
else if Shell.dir_comfortable "~/tmp" then
  st#temporary_directory#set "~/tmp"
else
  failwith "Please create either /tmp or your home directory on some reasonable modern filesystem supporting sparse files")

(* Check that we're *not* running as root. Yes, this has been reversed
   since the last version: *)
let () = begin
Log.printf "Checking whether Marionnet is running as root...\n";
if (Unix.getuid ()) = 0 then begin
  Log.printf "\n**********************************************\n";
  Log.printf "* Marionnet should *not* be run as root, for * \n";
  Log.printf "* security reasons.                          *\n";
  Log.printf "* Continuing anyway...                       *\n";
  Log.printf "**********************************************\n\n";
  Simple_dialogs.warning
    (s_ "You should not be root!")
    (s_ "Marionnet is running with UID 0; this is bad from a security point of view... Continuing anyway.")
    ();
end
end

(** Make sure that the user installed all the needed software: *)
let check_call ~action ~arg ~error_message =
  try
    ignore (action arg)
  with e -> (
    flush_all ();
    Simple_dialogs.error
      (s_ "Unsatisfied dependency")
      (error_message ^ (s_ "\nContinuing anyway, but *some important features will be missing*."))
      ())

let check_dependency command_line error_message =
  check_call ~action:Log.system_or_fail ~arg:command_line ~error_message

let machine_installations = Disk.get_machine_installations ()
let router_installations = Disk.get_router_installations ()

(** Check whether we have UML computer filesystems: *)
let () =
  let error_message = (s_ "You don't have a default filesystem for virtual computers") in
  let action () = Option.extract machine_installations#filesystems#get_default_epithet  in
  check_call ~action ~arg:() ~error_message

(** Check whether we have UML router filesystems: *)
let () =
  let error_message = (s_ "You don't have a default filesystem for virtual routers") in
  let action () = Option.extract router_installations#filesystems#get_default_epithet in
  check_call ~action ~arg:() ~error_message

(** Check whether we have UML kernels: *)
let () =
  let error_message = (s_ "You don't have a default UML kernel for virtual computers") in
  let action () = Option.extract machine_installations#kernels#get_default_epithet  in
  check_call ~action ~arg:() ~error_message

(** Check whether we have (our patched) VDE: *)
let () =
  check_dependency
    ("which `basename " ^ Initialization.Path.vde_prefix ^ "vde_switch`")
    (s_ "You don't have the VDE tool vde_switch")

(** Check whether we have (our patched) VDE: *)
let () =
  check_dependency
    ("which `basename " ^ Initialization.Path.vde_prefix ^ "slirpvde`")
    (s_ "You don't have the VDE tool slirpvde")

(** Check whether we have Graphviz: *)
let () =
  check_dependency
    "which dot"
    (s_ "You don't have Graphviz")


module Motherboard = Created_window_MARIONNET.Motherboard

let () = begin

(** Set the main window icon (which may be the exam icon...), and the window title: *)
st#mainwin#toplevel#set_icon (Some Icon.icon_pixbuf);
st#mainwin#window_MARIONNET#set_title Command_line.window_title;

(* This action must be done when all treeviews are set: *)
Motherboard.set_treeview_filenames_invariant ();

st#sensitive_when_Active#add   st#mainwin#notebook_CENTRAL#coerce;
st#sensitive_when_Runnable#add st#mainwin#hbuttonbox_BASE#coerce ;

(* st#mainwin#notebook_CENTRAL#coerce#misc#set_sensitive false; *)
(** Enter the GTK+ main loop: *)
try
  GtkThread.main ()
with e ->
  begin
   Log.print_backtrace ();
   raise e
  end

end
