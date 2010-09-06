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

open Gettext;;

let marionnet_working_directory_prefix =
  "marionnet-";;

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
  let system = new Chip.system ~name:"motherboard" () in
  let win    = new Gui.window_MARIONNET () in
  let net    = new Mariokit.Netmodel.network () in
  object (self)

  (** Main window. *)
  method mainwin = win

  (** Virtual network. *)
  method network = net

  (** Motherboard is set in Gui_motherboard. *)
  val mutable motherboard : State_types.motherboard option = None
  method motherboard = match motherboard with Some x -> x | None -> assert false
  method set_motherboard m = motherboard <- Some m

  method system = system

  (** Access methods for the dot options, used for drawing the virtual network. *)
  method dotoptions = net#dotoptions

  (** Show something on statusbar. *)
  method flash ?(delay:int=2000) (msg:string) =
   let statusbar_ctx = win#statusbar#new_context "global" in
   statusbar_ctx#flash ~delay msg

  (** The state of application.*)
  val app_state = Chip.wref ~name:"app_state" NoActiveProject
  method app_state = app_state

  (** Are we working with an active project. *)
  method active_project = (app_state#get_alone <> NoActiveProject)

  (** The project filename. *)
  val prj_filename = Chip.wref ~name:"prj_filename" None
  method prj_filename = prj_filename

  (** The project name. *)
  val mutable prj_name : string option = None

  (** Get the project filename. *)
  method extract_prj_filename = match prj_filename#get with | Some x -> x | None -> ""

  (** Get the project name. *)
  method get_prj_name     = match prj_name with | Some x -> x | None -> ""

  (** by default, the name of the project is the basename of filename without extension. *)
  method default_prj_name ?(arg=None) () =
    let arg = (match arg with None -> self#extract_prj_filename | Some x -> x) in
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
  method getDir dir = (self#get_pwdir^"/"^(self#get_prj_name)^"/"^dir^"/")

  method tmpDir         = self#getDir "tmp"
  method patchesDir     = self#getDir "states"
  method netmodelDir    = self#getDir "netmodel"
  method scriptsDir     = self#getDir "scripts"
  method hostfsDir      = self#getDir "hostfs"
  method classtestDir   = self#getDir "classtest"
  method dotSketchFile  = self#tmpDir^"sketch.dot"
  method pngSketchFile  = self#tmpDir^"sketch.png"
  method networkFile    = self#netmodelDir^"network.xml"
  method dotoptionsFile = self#netmodelDir^"dotoptions.marshal"

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
        (Some (UnixExtra.temp_dir ~parent:wdir ~prefix:marionnet_working_directory_prefix ~suffix:".dir" ()));

      (* Create the directory skeleton into the pwdir. *)
      let prefix = (self#get_pwdir^"/"^(self#get_prj_name)^"/") in
      Unix.mkdir (prefix)             0o755 ;
      ListExtra.foreach ["states";"netmodel";"scripts";"hostfs";"classtest";"tmp"] (fun x->(Unix.mkdir (prefix^x) 0o755)) ;

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

     (* Refresh the network sketch *)
      self#refresh_sketch () ;

     ()
    end


  (** Close the current project. The project is lost if the user hasn't saved it. *)
  method close_project ()  =

   let close_project_sync () =
    begin
      Log.printf "state#close_project_sync: starting...\n";
      (* Destroy whatever the LEDgrid manager is managing: *)
      self#network#ledgrid_manager#reset;
      (match app_state#get with
       | NoActiveProject -> Log.printf "state#close_project_sync: no project opened.\n"
       | _ -> 
        begin
         self#network#reset ~scheduled:true ();
         (* Unset the project filename. *)
         prj_filename#set None ;
         (* Unset the project name. *)
         prj_name <- None ;
         let cmd = "rm -rf "^self#get_pwdir in
         (* Update the network sketch (now empty) *)
         self#mainwin#sketch#set_file "" ;
         (* Unset the project working directory. *)
         self#set_pwdir None;
         (* Set the app_state. *)
         app_state#set NoActiveProject ;
         (* Force GUI coherence. *)
         self#gui_coherence ();
         Log.printf "Destroying the old working directory (%s)...\n" cmd;
         ignore (Unix.system cmd);
         Log.printf "Done.\n";
        end (* there was an active project *)
       );
      (* Clear all treeviews, just in case. *)
      Filesystem_history.clear ();
      Filesystem_history.reset_states_directory ();
      (Network_details_interface.get_network_details_interface ())#reset_file_name;
      (Network_details_interface.get_network_details_interface ())#reset;
      (Defects_interface.get_defects_interface ())#reset_file_name;
      (Defects_interface.get_defects_interface ())#clear;
      (Texts_interface.get_texts_interface ())#reset_file_name;
      (Texts_interface.get_texts_interface ())#clear;
      Log.printf "state#close_project_sync: done.\n";
    end (* thunk *)
    in
    ignore (Task_runner.the_task_runner#schedule
              ~name:"close_project_sync"
              close_project_sync);

 (** Read the pseudo-XML file containing the network definition. *)
 method import_network  ?(emergency:(unit->unit)=(fun x->x)) ?(dotAction:(unit->unit)=fun x->x) (f:filename) =
   begin

   (* Backup the network. *)
   self#network#save_to_buffers;

   (* Plan to restore the network if something goes wrong. *)
   let emergency = fun e ->
         Log.printf "import_network: emergency (%s)!!!\n" (Printexc.to_string e);
	 self#network#restore_from_buffers;
         emergency () in

   (* Read the given file. *)
   (if (Shell.regfile_readable f)
   then try
       Mariokit.Netmodel.Xml.load_network self#network f ;
       Log.printf ("import_network: network imported\n");
   with e -> (emergency e;  raise e)
   else ( emergency (Failure "file not readable"); raise (Failure "state#import_network: cannot open the xml file") ));

   (* Fix the app_state and update it if necessary *)
   app_state#set ActiveNotRunnableProject ;
   self#update_state (); (* is it runnable? *)

   (* Undump Dotoptions.network *)
   dotAction ();

   (* Force GUI coherence. *)
   self#gui_coherence ();

   (* Update the network sketch *)
   self#refresh_sketch ();
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
    let working_directory = UnixExtra.temp_dir ~parent:wdir ~prefix:marionnet_working_directory_prefix ~suffix:".dir" () in
    self#set_pwdir (Some working_directory);

    (* Extract the mar file into the pwdir *)
    Shell.tgz_extract self#extract_prj_filename self#get_pwdir;

    (* Look for the name of the root directory of the mar file. Some checks here. *)
    let rootname =
      try
        match (SysExtra.readdir_into_list self#get_pwdir) with

        | [x] -> let skel = (SysExtra.readdir_into_list (self#get_pwdir^x)) in
                 if ListExtra.subset skel ["states";"netmodel";"scripts";"hostfs";"classtest"]
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
        Log.printf ("state#open_project: dotoptions recovered\n");
       with e -> (Log.printf ("open_project: cannot read the dotoptions file => resetting defaults\n");
                   self#dotoptions#reset_defaults ())) ;
       self#dotoptions#set_toolbar_widgets ()
      end in

    Log.printf ("state#open_project: calling import_network\n");

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
    Log.printf "state#save_project starting...\n";

    (* The motherboard is read by the father *)
    let filename = self#extract_prj_filename in

    (* Progress bar periodic callback. *)
    let fill =
      (* disk usage (in kb) with the unix command *)
      let du x = match UnixExtra.run ~trace:true ("du -sk "^x) with
       | kb, (Unix.WEXITED 0) -> (try Some (float_of_string (List.hd (StringExtra.split ~d:'\t' kb))) with _ -> None)
       | _,_                  -> None
      in
      (* disk usage (in kb) with the unix library *)
      let du_file_in_kb x = try Some (float_of_int ((Unix.stat x).Unix.st_size / 1024)) with _ -> None in
      let round x = float_of_string (Printf.sprintf "%.2f" (if x<1. then x else 1.)) (* workaround strange lablgtk behaviour *) in
      match (du self#get_pwdir) with
      | Some kb_flatten ->
         fun () -> (match du_file_in_kb filename with
                    | Some kb_compressed -> round (0.05 +. (kb_compressed *. 8.) /. kb_flatten)
                    | None -> 0.5)
      | None -> fun () -> 0.5
    in

    let window =
      Progress_bar.make_progress_bar_dialog
       ~modal:true
       ~title:("Work in progress")
       ~kind:(Progress_bar.Fill fill)
       ~text_on_label:("<big><b>Sauvegarde</b></big>")
       ~text_on_sub_label:("<tt><small>"^filename^"</small></tt>")
       ()
    in
    (* Write the network xml file *)
    Mariokit.Netmodel.Xml.save_network self#network self#networkFile ;

    (* Save also dotoptions for drawing image. *)
    self#dotoptions#save_to_file self#dotoptionsFile;

    (* Save treeviews (just to play it safe, because treeview files should be automatically)
       re-written at every update): *)
    Filesystem_history.save_states ();
    (Network_details_interface.get_network_details_interface ())#save;
    (Defects_interface.get_defects_interface ())#save;
    (Texts_interface.get_texts_interface ())#save;

    (* (Re)write the .mar file *)
    let cmd = ("tar -cSvzf "^filename^" -C "^self#get_pwdir^" --exclude tmp "^(self#get_prj_name)) in
    let _ = Task_runner.the_task_runner#schedule ~name:"tar" (fun () -> ignore (Unix.system cmd)) in
    let _ = Task_runner.the_task_runner#schedule ~name:"destroy saving progress bar" (fun () -> Progress_bar.destroy_progress_bar_dialog window) in

    Log.printf "state#save_project (main thread) finished.\n";
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
        Task_runner.the_task_runner#schedule ~name:"copy_project_into" 
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


 val mutable sensitive_cables : GObj.widget list = []
 method add_sensitive_cable x = sensitive_cables <- x::sensitive_cables

  (** Forbid cable additions if there are not enough free ports; explicitly enable
      them if free ports are enough: *)
  method update_cable_sensitivity () =
    let node_names = self#network#get_node_names in
    let free_ethernet_port_names = (* we're interested in their number, not names... *)
      List.flatten
        (List.map
           (fun node_name ->
             self#network#free_receptacles_names_of_node node_name Mariokit.Netmodel.Eth)
           node_names) in
    let free_ethernet_port_no = List.length free_ethernet_port_names in
    let condition = (free_ethernet_port_no >= 2) in
    (List.iter (fun x->x#misc#set_sensitive condition) sensitive_cables)

  val refresh_sketch_counter = Chip.wcounter ~name:"refresh_sketch_counter" ()
  method refresh_sketch_counter = refresh_sketch_counter

  method refresh_sketch () =
   refresh_sketch_counter#set ();

  method network_change : 'a 'b. ('a -> 'b) -> 'a -> unit =
  fun action obj ->
   begin
    action obj;
    self#update_state ();
    self#update_cable_sensitivity ();
    self#dotoptions#reset_shuffler ();
    self#dotoptions#reset_extrasize ();
    refresh_sketch_counter#set ();
   end

  (** Update the state and force the gui coherence.
      If a project is active, there are two possibilities:
      1) there is at least a machine => the project is runnable
      2) else => the project is not runnable *)
  method update_state () =
    let old = app_state in
    begin
    match app_state#get_alone with
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
   Log.printf "st.wdir=%s" wdir;
   Log.printf "st.pwdir=%s" (try self#get_pwdir with _ ->"");
   Log.printf "st.prj_filename=%s" self#extract_prj_filename;
   Log.printf "st.prj_name=%s" self#get_prj_name

 (* Begin of methods moved from talking.ml *)
 method make_names_and_thunks verb what_to_do_with_a_node =
  List.map
    (fun node -> (verb ^ " " ^ node#get_name,
                  fun () ->
                    let progress_bar =
                      Simple_dialogs.make_progress_bar_dialog
                        ~title:(verb ^ " " ^ node#get_name)
                        ~text_on_bar:(s_ "Wait please...") () in
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
  Log.printf "is_there_something_on_or_sleeping: %s\n" (if result then "yes" else "no");
  result
  end

 (* End of functions moved from talking.ml *)

 method quit_async () =
   let quit () =
     begin
      Log.printf "Starting the last job...\n";
      self#network#destroy_process_before_quitting ();
(*    Log.printf "Killing the task runner thread...\n";
      Task_runner.the_task_runner#terminate; *)
      Log.printf "Killing the blinker thread...\n";
      self#network#ledgrid_manager#kill_blinker_thread;
      Log.printf "Ok, the blinker thread was killed.\n";
      flush_all ();
      Log.printf "Synced.\n";
      Log.printf "state#quit: done.\n";
      GtkMain.Main.quit ();
     end
   in
   begin
   Task_runner.the_task_runner#schedule ~name:"quit" quit;
   Log.printf "Main thread: quit has been scheduled.\n";
   end

end;; (* class globalState *)
