(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007, 2008, 2009  Luca Saiu
   Copyright (C) 2007, 2009, 2010  Jean-Vincent Loddo
   Copyright (C) 2007, 2008, 2009, 2010  Université Paris 13

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

module S = Serial
module G = Gui_source_editing

(* Enter the right directory: *)
let _enter_the_right_directory = 
  try Sys.chdir (Initialization.Path.marionnet_home)
  with _ -> failwith ("Could not enter the directory (" ^ Initialization.Path.marionnet_home ^ ")")

(** The global state containing the main window (st#mainwin) and all relevant dynamic
    attributes of the application *)
let st = new globalState ()

(** Add a global thunk allowing to invoke the sketch refresh method, 
    visible from many modules: *)
let () = Sketch.Refresh_sketch_thunk.set (fun () -> st#refresh_sketch)

module State = struct let st = st end

(* Complete the main menu *)
module Created_window_MARIONNET   = Gui_window_MARIONNET.   Make (State)
module Created_toolbar_COMPONENTS = Gui_toolbar_COMPONENTS. Make (State)

(* ***************************************** *
            Make the treeview widgets
 * ***************************************** *)

let window = st#mainwin#window_MARIONNET

(** Make the states interface: *)
let filesystem_history_interface =
  Treeview_history.make
    ~window
    ~hbox:(st#mainwin#filesystem_history_viewport)
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

let dialog_confirm_device_restart ~(devkind:string) ~(device_name:string) =
  let question =
    Printf.sprintf (f_ "Your changes will be applied after the reboot of %s.\nDo you want to restart this %s now?")
      device_name
      devkind
  in
  Gui_bricks.Dialog.yes_or_cancel_question
    ~title:(s_ "Reboot")
    ~markup:question
    ~context:()
    ()

let shutdown_or_restart_relevant_device device_name =
  Log.printf1 "Shutdown or restart \"%s\"?\n" device_name;
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
    let node = st#network#get_node_by_name device_name in
    if not node#can_gracefully_shutdown
    then Log.printf1 "No, \"%s\" doesn't need to be restarted\n" device_name
    else (* continue: *)
    let devkind = node#string_of_devkind in
    match dialog_confirm_device_restart ~devkind ~device_name with
    | None    -> ()
    | Some () -> node#gracefully_restart
  end

let after_user_edit_callback x =
  begin
    st#set_project_not_already_saved;
    shutdown_or_restart_relevant_device x
  end

(** Make the ifconfig treeview: *)
let treeview_ifconfig =
  Treeview_ifconfig.make
    ~window
    ~hbox:(st#mainwin#ifconfig_viewport)
    ~after_user_edit_callback
    ()

(** Make the defects interface: *)
let treeview_defects =
  Treeview_defects.make
    ~window
    ~hbox:(st#mainwin#defects_viewport)
    ~after_user_edit_callback
    ()

(** Make the texts interface: *)
let treeview_documents =
  Treeview_documents.make
    ~window
    ~hbox:(st#mainwin#documents_viewport)
    ~after_user_edit_callback:(fun _ -> st#set_project_not_already_saved)
    ()

module Just_for_testing = struct

  let get_machine_by_name name =
     let m = (st#network#get_node_by_name name) in
     let ul_m = ((Obj.magic m):> Machine.User_level_machine.machine) in
     ul_m

end (* Just_for_testing *)

(* ***************************************** *
                   M A I N
 * ***************************************** *)

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

(** Show the splash (only when there is no project to open): *)
let () =
 if !Initialization.optional_file_to_open = None
   then Splash.show_splash (* ~timeout:15000 *) ()
   else ()

(** Choose a reasonable temporary working directory: *)
let () =
 let suitable_tmp pathname =
   (UnixExtra.dir_rwx_or_link_to pathname) &&
   (Talking.does_directory_support_sparse_files pathname)
 in
 let defined_and_suitable_tmp x =
    (Option.map suitable_tmp x) = Some true
 in
 let warning_tmp_automatically_set_for_you ~dir =
   if not (Initialization.Disable_warnings.temporary_working_directory_automatically_set)
   then
    Simple_dialogs.warning
      (s_ "Temporary working directory automatically set")
      (Printf.sprintf (f_ "We chose %s as the temporary working directory, because the default candidates were not suitable (file rights and sparse files support).") dir)
      ()
   else () (* do nothing *)
 in
 let set_but_warning dir =
   st#temporary_directory#set dir;
   warning_tmp_automatically_set_for_you dir
 in
 let marionnet_tmpdir = Initialization.Path.marionnet_tmpdir in
 let tmpdir = (SysExtra.meaningful_getenv "TMPDIR")#existing_directory in
 let home   = (SysExtra.meaningful_getenv "HOME")#existing_directory
 in
 let d1 = marionnet_tmpdir in                                    (*  ${MARIONNET_TMPDIR}  *)
 let d2 = tmpdir in                                              (*  ${TMPDIR}  *)
 let d3 = "/tmp" in                                              (*  /tmp  *)
 let d4 = "/var/tmp" in                                          (*  /var/tmp *)
 (* The following candidates will raise a warning: *)
 let d5 = Initialization.cwd_at_startup_time in                  (*  $PWD  *)
 let d6 = Option.map (fun h -> Filename.concat h "tmp") home in  (*  ~/tmp *)
 let d7 = home in                                                (*  ~/    *)
 begin
  if defined_and_suitable_tmp d1 then st#temporary_directory#set (Option.extract d1) else
  if defined_and_suitable_tmp d2 then st#temporary_directory#set (Option.extract d2) else
  if suitable_tmp d3             then st#temporary_directory#set d3 else
  if suitable_tmp d4             then st#temporary_directory#set d4 else
  if suitable_tmp d5             then set_but_warning d5 else
  if defined_and_suitable_tmp d6 then set_but_warning (Option.extract d6) else
  if defined_and_suitable_tmp d7 then set_but_warning (Option.extract d7) else
    begin
      Simple_dialogs.warning
	(s_ "Sparse files not supported!")
	(s_ "You should probably create one of /tmp, ~/tmp and ~/ into a modern filesystem supporting sparse files (ext2, ext3, ext4, reiserfs, NTFS, ...), or set another suitable temporary working directory (menu Options). Marionnet will work with the current settings, but performance will be low and disk usage very high.")
	();
      (* Set anyway the value to "/tmp": *)
      st#temporary_directory#set "/tmp"
    end
  end

(* Check that we're *not* running as root. Yes, this has been reversed
   since the last version: *)
let () = begin
Log.printf "Checking whether Marionnet is running as root...\n";
if (Unix.getuid ()) = 0 then begin
  Log.printf "
**********************************************
* Marionnet should *not* be run as root, for *
* security reasons.                          *
* Continuing anyway...                       *
**********************************************\n\n";
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


(** Read and check filesystem's installations. Warning dialogs
    are created when something appears wrong or strange. *)
module VM_installations =
  Disk.Make_and_check_installations(struct end)

module Motherboard = Created_window_MARIONNET.Motherboard

let () = begin

(** Set the main window icon (which may be the exam icon...), and the window title: *)
st#mainwin#toplevel#set_icon (Some Icon.icon_pixbuf);
st#mainwin#window_MARIONNET#set_title Initialization.window_title;

(* This action must be done when all treeviews are set: *)
Motherboard.set_treeview_filenames_invariant ();

StackExtra.push (st#mainwin#notebook_CENTRAL#coerce) (st#sensitive_when_Active);
StackExtra.push (st#mainwin#hbox_BASE#coerce)        (st#sensitive_when_Runnable);

let () = Motherboard.sensitive_widgets_initializer () in

(* Open the project specified at command line, if any: *)
let () =
  match !Initialization.optional_file_to_open with
  | None -> ()
  | Some filename ->
      begin
	let filename =
	  FilenameExtra.to_absolute
	    ~parent:Initialization.cwd_at_startup_time
	    filename
	in
	try
	  st#open_project ~filename
	with
	  _ ->
	  begin
	    Printf.kfprintf flush stderr (f_ "Error: something goes wrong opening the file %s\nExiting.\n") filename;
	    exit 2
	  end
      end
in

(* Ignore some signals: *)
(* List.iter (fun x -> (Sys.set_signal x  Sys.Signal_ignore)) [1;2;3;4;5;6;10;12;15] ;; *)

(* This is very appropriate: a signal 15 (SIGTERM) may be received by Marionnet in some very complicated cases.
   For instance when a graphical program running in background on a virtual machine is showing its
   window on the X server (by the mean of a "socat" process or thread). If the command `halt' is
   launched on the virtual machine, a signal 15 is sent to Marionnet, probably as consequence of
   the broken connection (and the death of the "socat" process or thread). *)
let () =
  let callback _ =
   try
    (* Printf.kfprintf flush stderr "******************* HERE *********************\n"; *)
    let thread_id = Thread.id (Thread.self ()) in
    if thread_id = 0
    then GtkThread.main ()
    else () (* Printf.kfprintf flush stderr "******************* IGNORING *********************\n" *)   (* ignore *)
   with _ -> ()
  in
  Sys.set_signal Sys.sigterm (Sys.Signal_handle callback)
in

(* I we receive a CTRL-C from the terminal (2) we react as if the user click on the window close button: *)
let () =
 let callback _ =
   let () = Created_window_MARIONNET.Created_menubar_MARIONNET.Created_entry_project_quit.callback () in
   if st#quit_async_called then () else GtkThread.main ()
 in
 Sys.set_signal Sys.sigint (Sys.Signal_handle callback)
in

(* (* (* let () = SysExtra.log_signal_reception ~except:[26] () in *) *) *)

(* Try to kill all remaining descendants when exiting: *)
let () =
  let marionnet_pid = Unix.getpid () in
  let kill_orphan_descendants =
    Descendants_monitor.start_monitor_and_get_kill_method ()
  in
  Pervasives.at_exit
    (fun () ->
       begin
         Log.printf "at_exit: killing all current descendants before exiting...\n";
         (* Note here that the parameter `wait_delay' is set to 0. in order to kill the whole hierarchy in the quickest way
            (and directly with the must brutal signal `Sys.sigkill'). We need to be so violent because the UML Linux kernels
            (of the series 3.2.x) react to some signals restarting immediately a port-helper. This process will be attached
            to init (1) and will remain unnecessarily in the system. Furthermore, it may busy inexplicably the port 6000
            instead of Marionnet, when Marionnet exits. Thus, when Marionnet is restarted, it believes that the port is taken
            by a real X server! *)
         Linux.Process.kill_descendants ~signal_sequence:[Sys.sigkill] ~wait_delay:0. ~node_max_retries:2 ~root_max_retries:2 ();
         (* We kill orphans of the main program (not of its forks) *)
         if (Unix.getpid () = marionnet_pid) then begin
           Log.printf "at_exit: killing all orphans before exiting...\n";
           kill_orphan_descendants ();
           end;
       end)
in

(* st#mainwin#notebook_CENTRAL#coerce#misc#set_sensitive false; *)
(** Enter the GTK+ main loop: *)
let rec main_loop () =
  try
    GtkThread.main ()
  with e ->
    begin
    Log.printf "Marionnet's main loop interrupted by the following exception:\n";
    Log.print_backtrace ();
    Thread.delay 1.;
    if st#quit_async_called then (raise e) else main_loop ()
    end

in main_loop ()

end
