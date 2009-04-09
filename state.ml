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

(** Provide the class modelling the global state of the application. *)

open PreludeExtra.Prelude;; (* We want synchronous terminal output *)
open Sugar;;
open ListExtra;;
open StringExtra;;
open UnixExtra;;
open SysExtra;;

open Environment;;
open Mariokit;;

let marionnet_working_directory_prefix =
  "marionnet-";;

let sketch_mutex =
  Mutex.create ();;

let lock_sketch () =
  Mutex.lock sketch_mutex;;

let unlock_sketch () =
  Mutex.unlock sketch_mutex;;

let commit_suicide signal =
  raise Exit;;

(** Model for the global state of the application.
    The sensitive or visible properties of some widgets depend on this value.*)
type application_state =
  | NoActiveProject            (** Working with no project defined. User have to create or open. *)
  | ActiveNotRunnableProject   (** Working with a project with an empty or non runnable network. *)
  | ActiveRunnableProject      (** Working with a runnable project. *)
;;

type filename = string;;

(** The class modelling the global state of the application. *)
class globalState = fun () ->
  let win        = new Gui.window_MARIONNET ()           in
  let net        = new Netmodel.network ()               in

  let guiHandler = new Dotoptions.guiHandler win         in
  let dotoptions = new Dotoptions.network guiHandler net in
  let     _      = net#set_dotoptions dotoptions         in

  let statusbar_ctx = win#statusbar#new_context "global" in

  object (self)

  (** Main window. *)
  method mainwin = win

  (** Virtual network. *)
  method network = net

  (** Access methods for the dot options, used for drawing the virtual network. *)
  method dotoptions = dotoptions

  (** Show something on statusbar. *)
  method flash ?(delay:int=2000) (msg:string) = statusbar_ctx#flash ~delay msg

  (** The state of application.*)
  val app_state = Chip.wref ~name:"globalState#app_state" NoActiveProject
  method app_state = app_state

  (** Are we working with an active project. *)
  method active_project = (app_state#get <> NoActiveProject)

  (** The project filename. *)
  val prj_filename = Chip.wref ~name:"globalState#prj_filename" None
  method prj_filename = prj_filename

  (** The project name. *)
  val mutable prj_name : string option = None

  (** Get the project filename. *)
  method get_prj_filename = match prj_filename#get with | Some x -> x | None -> ""

  (** Get the project name. *)
  method get_prj_name     = match prj_name with | Some x -> x | None -> ""

  (** by default, the name of the project is the basename of filename without extension. *)
  method default_prj_name ?(arg=None) () =
    let arg = (match arg with None -> self#get_prj_filename | Some x -> x) in
    let base = (Filename.basename arg) in
    try
      (Filename.chop_extension base)
    with _ -> base

  (** Working directory. *)
  val mutable wdir = "/tmp"

  (** Setting working directory. *)
  method set_wdir x =
    wdir <- x

  (** Get working directory. *)
  method get_wdir = wdir

  (** Project working directory. *)
  val mutable pwdir_field : string option
      = Global_options.project_working_directory_default

  (** Get project working directory. *)
  method get_pwdir =
    match pwdir_field with
    | Some x ->
        x
    | None ->
        failwith "get_pwdir"

  (** Update the project working directory, also copying the setting to a global which is
      easier to access from other modules: *)
  method set_pwdir value =
    pwdir_field <- value;
    Global_options.set_project_working_directory value;

  (** Handlers for relevant project directories and files. *)
  method getDir dir     =
    Log.printf "getDir: calling get_pwdir\n"; flush_all ();
    let result =
      (self#get_pwdir^"/"^(self#get_prj_name)^"/"^dir^"/")
    in
    Log.printf "getDir: exiting with success\n"; flush_all ();
    result

  method tmpDir         =
    Log.printf "* calling getDir 1\n"; flush_all ();
    self#getDir "tmp"
  method patchesDir     =
    Log.printf "* calling getDir 2\n"; flush_all ();
    self#getDir "states"
  method netmodelDir    =
    Log.printf "* calling getDir 3\n"; flush_all ();
    self#getDir "netmodel"
  method scriptsDir     =
    Log.printf "* calling getDir 4\n"; flush_all ();
    self#getDir "scripts"
  method hostfsDir      =
    Log.printf "* calling getDir 5\n"; flush_all ();
    self#getDir "hostfs"
  method classtestDir   =
    Log.printf "* calling getDir 6\n"; flush_all ();
    self#getDir "classtest"
  method dotSketchFile  =
    Log.printf "* calling getDir 7\n"; flush_all ();
    self#tmpDir^"sketch.dot"
  method pngSketchFile  =
    Log.printf "* calling getDir 8\n"; flush_all ();
    self#tmpDir^"sketch.png"
  method networkFile    =
    Log.printf "* calling getDir 9\n"; flush_all ();
    self#netmodelDir^"network.xml"
  method dotoptionsFile =
    Log.printf "* calling getDir 10\n"; flush_all ();
    self#netmodelDir^"dotoptions.marshal"

  (** New project which will be saved into the given filename. *)
  method new_project (x:filename)  =
    begin
      (* First reset the old network, waiting for all devices to terminate: *)
      self#network#ledgrid_manager#reset;
      self#network#reset () ;

      (* Set the project filename *)
      prj_filename#set (Some x) ;

      (* Set the project name using the basename of filename whitout extension. *)
      prj_name     <-  Some (self#default_prj_name ()) ;

      (* Set the project working directory to a random name in the wdir. *)
      self#set_pwdir
        (Some (Unix.temp_dir ~parent:wdir ~prefix:marionnet_working_directory_prefix ~suffix:".dir" ()));

      (* Create the directory skeleton into the pwdir. *)
      let prefix = (self#get_pwdir^"/"^(self#get_prj_name)^"/") in
      Unix.mkdir (prefix)             0o755 ;
      List.foreach ["states";"netmodel";"scripts";"hostfs";"classtest";"tmp"] (fun x->(Unix.mkdir (prefix^x) 0o755)) ;

      (* Treeview data should be saved within prefix: *)
      Filesystem_history.set_states_directory (prefix ^ "states/");
      Filesystem_history.clear ();
      (Network_details_interface.get_network_details_interface ())#set_file_name (prefix ^ "states/ports");
      (Network_details_interface.get_network_details_interface ())#reset;
      (Defects_interface.get_defects_interface ())#set_file_name (prefix ^ "states/defects");
      (Defects_interface.get_defects_interface ())#clear;
      (Texts_interface.get_texts_interface ())#set_file_name (prefix ^ "states/texts");
      (Texts_interface.get_texts_interface ())#clear;

      (* Reset dotoptions *)
      self#dotoptions#reset_defaults () ;

      (* Set the app_state. *)
      app_state#set ActiveNotRunnableProject ;

      (* Force GUI coherence. *)
      self#gui_coherence () ;

      Log.print_endline ("*** in new_project");
     (* Refresh the network sketch *)
      self#refresh_sketch () ;

     ()
    end


  (** Close the current project. The project is lost if the user hasn't saved it. *)
  method close_project ()  =
    Log.print_string ">>>>>>>>>>CLOSING THE PROJECT: BEGIN<<<<<<<<\n";
    (* Destroy whatever the LEDgrid manager is managing: *)
    self#network#ledgrid_manager#reset;

    (if (app_state#get = NoActiveProject) then
       Log.print_string ">>>>>>>>>>(THERE'S NO PROJECT TO CLOSE)<<<<<<<<\n"
     else begin
      self#network#reset ();

      Log.print_endline ("*** in close_project (calling update_sketch)");
     (* Update the network sketch (now empty) *)
      self#update_sketch () ;

      (* Unset the project filename. *)
      prj_filename#set None ;

      (* Unset the project name. *)
      prj_name     <- None ;

      (* Unlink the project working directory content. *)
      Task_runner.the_task_runner#wait_for_all_currently_scheduled_tasks;
      Log.printf "** DESTROYING THE OLD WORKING DIRECTORY... BEGIN\n"; flush_all ();
      ignore (Unix.system ("rm -rf "^self#get_pwdir));
      Log.printf "** DESTROYING THE OLD WORKING DIRECTORY... DONE\n"; flush_all ();

      (* Unset the project working directory. *)
      self#set_pwdir None;

      (* Set the app_state. *)
      app_state#set NoActiveProject ;

      (* Force GUI coherence. *)
      self#gui_coherence ();
    end);

    (* Clear all treeviews, just in case. *)
    Filesystem_history.clear ();
    Filesystem_history.reset_states_directory ();
    (Network_details_interface.get_network_details_interface ())#reset_file_name;
    (Network_details_interface.get_network_details_interface ())#reset;
    (Defects_interface.get_defects_interface ())#reset_file_name;
    (Defects_interface.get_defects_interface ())#clear;
    (Texts_interface.get_texts_interface ())#reset_file_name;
    (Texts_interface.get_texts_interface ())#clear;
    Log.print_string ">>>>>>>>>>CLOSING THE PROJECT: END<<<<<<<<\n";


 (** Read the pseudo-XML file containing the network definition. *)
 method import_network  ?(emergency:(unit->unit)=(fun x->x)) ?(dotAction:(unit->unit)=fun x->x) (f:filename) =
   begin

   (* Backup the network. *)
   let backup = new Netmodel.network () in
   backup#copyFrom self#network ;

   (* Plan to restore the network if something goes wrong. *)
   let emergency = fun e ->
         Log.printf "import_network: emergency (%s)!!!\n" (Printexc.to_string e); flush_all ();
         self#network#copyFrom backup;
         emergency () in

   (* Read the given file. *)
   (if (Shell.regfile_readable f)
   then try
       Netmodel.Xml.load_network self#network f ;
       Log.print_endline ("import_network: network imported"); flush_all ();
   with e -> (emergency e;  raise e)
   else ( emergency (Failure "file not readable"); raise (Failure "state#import_network: cannot open the xml file") ));

   (* Fix the app_state and update it if necessary *)
   app_state#set ActiveNotRunnableProject ;
   self#update_state (); (* is it runnable? *)

   (* Undump Dotoptions.network *)
   dotAction ();

   (* Force GUI coherence. *)
   self#gui_coherence () ;

   (* Update the network sketch *)
   self#update_sketch ();
   ()
   end


  (** Close the current project and extract the given filename in a fresh project working directory. *)
  method open_project (x:filename) =
    begin
    (* First close the current project, if necessary *)
    self#close_project ();

    (* Set the project filename *)
    prj_filename#set (Some x) ;

    (* Set the project working directory to a random name in the wdir. *)
    let working_directory = Unix.temp_dir ~parent:wdir ~prefix:marionnet_working_directory_prefix ~suffix:".dir" () in
    self#set_pwdir (Some working_directory);

    (* Extract the mar file into the pwdir *)
    Shell.tgz_extract self#get_prj_filename self#get_pwdir;

    (* Look for the name of the root directory of the mar file. Some checks here. *)
    let rootname =
      try
        match (Sys.readdir_into_list self#get_pwdir) with

        | [x] -> let skel = (Sys.readdir_into_list (self#get_pwdir^x)) in
                 if List.subset skel ["states";"netmodel";"scripts";"hostfs";"classtest"]
                 then x
                 else raise (Failure "state#open_project: no expected content in the project working directory root.")

        |  _  -> raise (Failure "state#open_project: no rootname found in the project working directory.")
      with e -> begin
        self#close_project ();
        raise e;
      end;
    in

    (* Set the project name *)
    prj_name <- Some rootname;
    (* Create the tmp subdirectory. *)
    let prefix = (self#get_pwdir^"/"^rootname^"/") in
    Unix.mkdir (prefix^"tmp") 0o755 ;

    (* Set the treeview directories so that we can safely use treeviews, even before filling them
       with real data: *)
    Filesystem_history.set_states_directory (prefix ^ "states/");
    (Network_details_interface.get_network_details_interface ())#set_file_name (prefix ^ "states/ports");
    (Defects_interface.get_defects_interface ())#set_file_name (prefix ^ "states/defects");
    (Texts_interface.get_texts_interface ())#set_file_name (prefix ^ "states/texts");

    (* Dotoptions.network will be undumped after the network, in order to support cable inversions. *)

    let dotAction () = begin
      (try
     	self#dotoptions#load_from_file self#dotoptionsFile;
        Log.print_endline ("open_project: dotoptions recovered");
       with e -> (Log.print_endline ("open_project: cannot read the dotoptions file => resetting defaults");
                   self#dotoptions#reset_defaults ())) ;
       self#dotoptions#write_gui ()
      end in

    Log.print_endline ("open_project: calling import_network");

    (* Second, read the xml file containing the network definition. If something goes wrong, close the project. *)
    (try
    self#import_network ~emergency:self#close_project ~dotAction self#networkFile ;
    with e ->
      Log.printf "Failed with exception %s\n" (Printexc.to_string e);
      flush_all ());

    (* Now undump data and fill all the treeviews: *)
    Filesystem_history.load_states ();
    (Network_details_interface.get_network_details_interface ())#load;
    (Defects_interface.get_defects_interface ())#load;
    (Texts_interface.get_texts_interface ())#load;

    ()
    end



  (** Rewrite the compressed archive prj_filename with the content of the project working directory (pwdir). *)
  method save_project () =

    if self#active_project then begin
    Log.print_string ">>>>>>>>>>SAVING THE PROJECT: BEGIN<<<<<<<<\n";

    (* Progress bar periodic callback. *)
    let fill =
      (* disk usage (in kb) with the unix command *)
      let du x = match Unix.run ~trace:true ("du -sk "^x) with
       | kb, (Unix.WEXITED 0) -> (try Some (float_of_string (List.hd (String.split ~d:'\t' kb))) with _ -> None)
       | _,_                  -> None
      in
      (* disk usage (in kb) with the unix library *)
      let du_file_in_kb x = try Some (float_of_int ((Unix.stat x).Unix.st_size / 1024)) with _ -> None in
      let round x = float_of_string (Printf.sprintf "%.2f" (if x<1. then x else 1.)) (* workaround strange lablgtk behaviour *) in
      match (du self#get_pwdir) with
      | Some kb_flatten ->
         fun () -> (match du_file_in_kb self#get_prj_filename with
                    | Some kb_compressed -> round (0.05 +. (kb_compressed *. 8.) /. kb_flatten)
                    | None -> 0.5)
      | None -> fun () -> 0.5
    in

    let window =
      Progress_bar.make_progress_bar_dialog
       ~modal:true
       ~title:("Marionnet")
       ~kind:(Progress_bar.Fill fill)
       ~text_on_label:("<big><b>Sauvegarde</b></big>")
       ~text_on_sub_label:("<tt><small>"^self#get_prj_filename^"</small></tt>")
       ()
    in

    (* Write the network xml file *)
    Netmodel.Xml.save_network self#network self#networkFile ;

    (* Save also dotoptions for drawing image. *)
    self#dotoptions#save_to_file self#dotoptionsFile;

    (* Save treeviews (just to play it safe, because treeview files should be automatically)
       re-written at every update): *)
    Filesystem_history.save_states ();
    (Network_details_interface.get_network_details_interface ())#save;
    (Defects_interface.get_defects_interface ())#save;
    (Texts_interface.get_texts_interface ())#save;

    (* (Re)write the .mar file *)
    let cmd = ("tar -cSvzf "^(self#get_prj_filename)^" -C "^self#get_pwdir^" --exclude tmp "^(self#get_prj_name)) in
    let _ = Task_runner.the_task_runner#schedule (fun () -> ignore (Unix.system cmd)) in
    let _ = Task_runner.the_task_runner#schedule (fun () -> Progress_bar.destroy_progress_bar_dialog window) in

    Log.print_string ">>>>>>>>>>SAVING THE PROJECT: END<<<<<<<<\n";
  end


  (** Update the project filename to the given string, and save: *)
  method save_project_as (x:filename) =
    if self#active_project then
      try
      begin
        let new_prj_name = (self#default_prj_name ~arg:(Some x) ()) in

        (* Set the project name *)
        self#change_prj_name new_prj_name;

        (* Set the project filename *)
        prj_filename#set (Some x) ;

        (* Save the project *)
        self#save_project ();

        (* Debugging. *)
        self#debugging () ;

        ()
      end
      with e -> (raise e)

  (** Save the project into the given file, but without altering its name in the copy we're
      editing *)
  method copy_project_into (x:filename) =
    (* This is implemented by temporarily updating the name, saving and then switch back to
       the old name *)
    if self#active_project then
      try
      begin
        let original_prj_name = self#get_prj_name in
        let original_filename = prj_filename#get in
        let temporary_prj_name = (self#default_prj_name ~arg:(Some x) ()) in

        (* Temporarily update names...: *)
        self#change_prj_name temporary_prj_name;
        prj_filename#set (Some x) ;

        (* Save the project *)
        self#save_project ();

        (* Reset names ensuring synchronisation *)
        Task_runner.the_task_runner#schedule
          (fun () -> 
           (* Reset names to their old values: *)
           self#change_prj_name original_prj_name;
           prj_filename#set original_filename);

        (* Debugging. *)
        self#debugging () ;

        ()
      end
      with e -> (raise e)

  (** Change the prj_name. This simply means to change the name of the root directory in the pwdir. *)
  method change_prj_name x  =
    let old_name = self#get_prj_name in
    if (not (x=old_name)) then begin
      (* Set the project name. *)
      prj_name <- (Some x) ;

      (* Rename the root directory in the pwdir. *)
      let _ = Unix.system ("mv "^self#get_pwdir^"/"^old_name^" "^self#get_pwdir^"/"^x) in () ;

      (* The states interface must also be informed of the new prefix so that it can find
         its data files, including cows: *)
      Filesystem_history.reset_states_directory ();
      Filesystem_history.set_states_directory (self#get_pwdir^"/"^x^"/states/");
      (Network_details_interface.get_network_details_interface ())#set_file_name
        (self#get_pwdir^"/"^x^"/states/ports");
      (Defects_interface.get_defects_interface ())#set_file_name
        (self#get_pwdir^"/"^x^ "/states/defects");
      (Texts_interface.get_texts_interface ())#set_file_name
        (self#get_pwdir^"/"^x^ "/states/texts");
    end;

  (** Refresh the sketch with the current dotoptions. *)
  method refresh_sketch () =
    lock_sketch ();
    let fs = self#dotSketchFile in
    let ft = self#pngSketchFile in
    let ch = open_out fs in
    output_string ch (self#network#dotTrad ());
    close_out ch;
    let command_line =
      "dot -Efontname=FreeSans -Nfontname=FreeSans -Tpng -o "^ft^" "^fs in
    Log.printf "The dot command line is\n%s\n" command_line; flush_all ();
    let exit_code =
      Sys.command command_line in
    Log.printf "dot exited with exit code %i\n" exit_code; flush_all ();
    self#mainwin#sketch#set_file self#pngSketchFile ;
    (if not (exit_code = 0) then
      Simple_dialogs.error
        "FRENCH dot failed"
        (Printf.sprintf
           "FRENCH Invoking dot failed. Did you install graphviz?
The command line is
%s
and the exit code is %i.
Marionnet will work, but you will not see the network graph picture until you fix the problem.
There is no need to restart the application."
           command_line
           exit_code)
        ());

    unlock_sketch ()


 val mutable sensitive_cables : GObj.widget list = []
 method add_sensitive_cable x = sensitive_cables <- x::sensitive_cables

  (** Forbid cable additions if there are not enough free ports; explicitly enable
      them if free ports are enough: *)
  method update_cable_sensitivity () =
    let node_names = self#network#getNodeNames in
    let free_ethernet_port_names = (* we're interested in their number, not names... *)
      List.flatten
        (List.map
           (fun node_name ->
             self#network#freeReceptaclesNamesOfNode node_name Netmodel.Eth)
           node_names) in
    let free_ethernet_ports_no = List.length free_ethernet_port_names in
    let condition = (free_ethernet_ports_no >= 2) in
    (List.iter (fun x->x#misc#set_sensitive condition) sensitive_cables)


  (** Refresh the sketch resetting the shuffle dotoption.
      When a node is added or removed from the network you have to call the update_sketch,
      else simply refresh_sketch. *)
  method update_sketch () =
    lock_sketch ();
    self#dotoptions#reset_shuffler  ();
    self#dotoptions#reset_extrasize ();
    unlock_sketch ();
    Log.print_endline ("*** in update_sketch calling refresh_sketch");
    self#refresh_sketch (); (* this is already synchronized *)

  (** Update the state and force the gui coherence.
      If a project is active, there are two possibilities:
      1) there is at least a machine => the project is runnable
      2) else => the project is not runnable *)
  method update_state () =
    let old = app_state in
    begin
    match app_state#get with
     | NoActiveProject -> ()
     | _ -> app_state#set (if self#network#nodes=[]
                           then ActiveNotRunnableProject
                           else ActiveRunnableProject)
    end ;
    if app_state <> old then self#gui_coherence ();
    ()


 (** Force coherence between state and sensitive attributes of mainwin's widgets *)
 method gui_coherence () =
    self#update_state  () ;
(*    self#set_sensitive () ;*)
    self#update_cable_sensitivity ();

 (** For debugging *)
 method debugging () =
   Log.print_endline ("st.wdir="^wdir);
   Log.print_endline ("st.pwdir="^(try self#get_pwdir with _ ->""));
   Log.print_endline ("st.prj_filename="^self#get_prj_filename);
   Log.print_endline ("st.prj_name="^self#get_prj_name)

 (* Begin of methods moved from talking.ml *)
 method make_names_and_thunks verb what_to_do_with_a_node =
  List.map
    (fun node -> (verb ^ " " ^ node#get_name,
                  fun () ->
                    let progress_bar =
                      Simple_dialogs.make_progress_bar_dialog
                        ~title:(verb ^ " " ^ node#get_name)
                        ~text_on_bar:"Patientez s'il vous plaît..." () in
                    (try
                      what_to_do_with_a_node node;
                    with e ->
                      Log.printf "Warning (q): \"%s %s\" raised an exception (%s)\n"
                        verb
                        node#name
                        (Printexc.to_string e));
                    flush_all ();
                    Simple_dialogs.destroy_progress_bar_dialog progress_bar))
    self#network#nodes

 method do_something_with_every_node_in_sequence verb what_to_do_with_a_node =
  List.iter
    (fun (name, thunk) -> Task_runner.the_task_runner#schedule ~name thunk)
    (self#make_names_and_thunks verb what_to_do_with_a_node)

 method do_something_with_every_node_in_parallel verb what_to_do_with_a_node =
  Task_runner.the_task_runner#schedule_parallel
    (self#make_names_and_thunks verb what_to_do_with_a_node);;

 method startup_everything () =
  self#do_something_with_every_node_in_sequence
    "Startup" (fun node -> node#startup_right_now)

 method shutdown_everything () =
  self#do_something_with_every_node_in_parallel
    "Shut down" (fun node -> node#gracefully_shutdown_right_now)

 method poweroff_everything () =
  self#do_something_with_every_node_in_sequence
    "Power-off"
    (fun node -> node#poweroff_right_now)

(** Return true iff there is some node on or sleeping *)
 method is_there_something_on_or_sleeping () =
  let result = List.exists
                (fun node -> node#can_gracefully_shutdown or node#can_resume)
                self#network#nodes
  in begin
  Log.print_string ("Is there something running? " ^ (if result then "yes" else "no") ^ "\n");
  result
  end

 (* End of functions moved from talking.ml *)

 method quit () =
   Log.print_string ">>>>>>>>>>QUIT: BEGIN<<<<<<<<\n";
   (* Shutdown all devices and synchronously wait until they actually terminate: *)
   self#shutdown_everything ();
   Task_runner.the_task_runner#wait_for_all_currently_scheduled_tasks;
   self#close_project (); (* destroy the temporary project directory *)
   Log.print_endline ("globalState#quit: .react: Calling mrPropre...");
   self#mrPropre ();
   GMain.Main.quit (); (* Finalize the GUI *)
   Log.print_string "Killing the task runner thread...\n";
   Task_runner.the_task_runner#terminate;
   Log.print_string "Killing the death monitor thread...\n";
   Death_monitor.stop_polling_loop ();
   Log.print_string "Killing the blinker thread...\n";
   self#network#ledgrid_manager#kill_blinker_thread;
   Log.print_string "...ok, the blinker thread was killed (from talking.ml).\n";
   Log.print_string "Sync, then kill our process (To do: this is a very ugly kludge)\n";
   flush_all ();
   Log.print_string "Synced.\n";
   commit_suicide Sys.sigkill; (* this always works :-) *)
   Log.print_string "!!! This should never be shown.\n";;

 method mrPropre () =
    if self#active_project then raise (Failure "A project is still open, I cannot clean the wdir!")
    begin
    let _ = Unix.system ("rmdir "^wdir) in ()
    end

end;; (* class globalState *)
