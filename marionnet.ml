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

open PreludeExtra.Prelude (* We want synchronous terminal output *)
open StdLabels
open Gui

open State
open Talking
open Filesystem_history
open Network_details_interface
open Defects_interface
open Texts_interface

let () = Random.self_init ()

(** The global state containing the main window (st#mainwin) and all relevant dynamic
    attributes of the application *)
let st = new globalState () 

(** Add a global thunk allowing to invoke the sketch refresh method, visible from many
    modules: *)
let () = Mariokit.set_refresh_sketch_thunk (fun () -> st#refresh_sketch ())
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
  Filesystem_history.make_states_interface
    ~packing:(st#mainwin#filesystem_history_viewport#add)
    ()

(** See the comment in states_interface.ml for why we need this ugly kludge: *)
let () = Filesystem_history.set_startup_functions
  (fun name ->
    let node = st#network#getNodeByName name in
    node#can_startup)
  (fun name ->
    let node = st#network#getNodeByName name in
    node#startup)

let shutdown_or_restart_relevant_device device_name =
  Log.printf "Shutdown or restart \"%s\".\n" device_name;
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
  end

(** Make the network details interface: *)
let network_details_interface =
  make_network_details_interface
    ~packing:(st#mainwin#network_details_viewport#add)
    ~after_user_edit_callback:shutdown_or_restart_relevant_device
    ()

(** Make the defects interface: *)
let defects_interface =
  make_defects_interface
    ~packing:(st#mainwin#defects_viewport#add)
    ~after_user_edit_callback:shutdown_or_restart_relevant_device
    ()

(** Make the texts interface: *)
let texts_interface =
  make_texts_interface
    ~packing:(st#mainwin#texts_viewport#add)
    ()

(* ***************************************** *
                   M A I N
 * ***************************************** *)

(** Ignore some signals (for instance CTRL-C *)
(* List.iter (fun x -> (Sys.set_signal x  Sys.Signal_ignore)) [1;2;3;4;5;6;10;12;15] ;; *)

(** Timeout for refresh the state_coherence *)
(* let id = GMain.Timeout.add ~ms:1000 ~callback:(fun () -> st#state_coherence ();true) ;; *)

let () = Log.print_string "Starting the application\n"

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
    "FRENCH Could not connect to the daemon"
    (Printf.sprintf
       "FRENCH Connecting to the Marionnet daemon failed (%s); Marionnet will work, but some features (graphics on virtual machines and host sockets) won't be available."
       (Printexc.to_string e))
    ();
end)

(** Show the splash: *)
let () = Splash.show_splash (* ~timeout:15000 *) ()

(** Choose a reasonable working directory: *)
let () = (if Shell.dir_comfortable "/tmp" then
  st#set_wdir "/tmp"
else if Shell.dir_comfortable "~/tmp" then
  st#set_wdir "~/tmp"
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
    "FRENCH You should not be root!"
    "FRENCH Marionnet is running with UID 0; this is bad from a security point of view.
[To do: write a longer, cleaner message...]
Continuing anyway."
    ();
end
end

(* To do: move this into UnixExtra or something like that: *)
(** Run system with the given argument, and raise exception in case of failure;
    return unit on success. *)
let system_or_fail command_line =
  Log.printf "Executing \'%s\'...\n" command_line;
  flush_all ();
  match Unix.system command_line with
    Unix.WEXITED 0 ->
      ()
  | Unix.WEXITED n ->
      failwith (Printf.sprintf "Unix.system: the process exited with %i" n)
  | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
      failwith "Unix.system: the process was signaled or stopped"

(** Make sure that the user installed all the needed software: *)
let check_dependency command_line error_message =
  try
    system_or_fail command_line;
  with e ->
    Simple_dialogs.error
      "FRENCH Unsatisfied dependency"
      (error_message ^ "\nFRENCH Continuing anyway, but *some important features will be missing*.")
      ()

(** Check whether we have UML computer filesystems: *)
let () = check_dependency
  (Printf.sprintf
    "ls -l %s/machine-default &> /dev/null"
    Initialization.marionnet_home_filesystems)
  "FRENCH You don't have a default filesystem for virtual computers"

(** Check whether we have UML router filesystems: *)
let () = begin
 let command_line =
  (Printf.sprintf
    "ls -l %s/router-%s &> /dev/null"
    Initialization.marionnet_home_filesystems Strings.router_unprefixed_filesystem) in
  check_dependency
   command_line
   ("FRENCH You don't have a default filesystem for virtual routers (" ^ command_line ^")")
  end

(** Check whether we have UML kernels: *)
let () = check_dependency
  (Printf.sprintf
    "ls -l %s/linux-default &> /dev/null"
    Initialization.marionnet_home_kernels)
  "FRENCH You don't have a default UML kernel"

(** Check whether we have (our patched) VDE: *)
let () = check_dependency
  ("which `basename " ^ Initialization.vde_prefix ^ "vde_switch` &> /dev/null")
  "FRENCH You don't have VDE"

(** Check whether we have Graphviz: *)
let () = check_dependency
  "which dot &> /dev/null"
  "FRENCH You don't have Graphviz"

let () = begin

(** Set the main window icon (which may be the exam icon...), and the window title: *)
st#mainwin#toplevel#set_icon (Some Icon.icon_pixbuf);
st#mainwin#window_MARIONNET#set_title Command_line.window_title;

(*st#mainwin#window_MARIONNET#event#connect#key_press
    ~callback:(fun ev -> (Printf.eprintf "Marionnet: Key pressed: %s\n" (GdkEvent.Key.string ev)); (flush stderr); true);*)

st#add_sensitive_when_Active   st#mainwin#notebook_CENTRAL#coerce;
st#add_sensitive_when_Runnable st#mainwin#hbuttonbox_BASE#coerce ;
st#gui_coherence ();

(** Enter the GTK+ main loop: *)
GtkThread.main ()
end
