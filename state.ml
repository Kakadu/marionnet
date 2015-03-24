(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007, 2008  Luca Saiu
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

(** Provide the class modelling the global state of the application. *)

open Gettext;;

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

(* Example of hierarchy:

/tmp/marionnet-588078453.dir/
/tmp/marionnet-588078453.dir/sparse-swap-410647109
/tmp/marionnet-588078453.dir/sparse-swap-922455527
/tmp/marionnet-588078453.dir/test_machine
/tmp/marionnet-588078453.dir/test_machine/classtest
/tmp/marionnet-588078453.dir/test_machine/tmp
/tmp/marionnet-588078453.dir/test_machine/tmp/sketch.dot
/tmp/marionnet-588078453.dir/test_machine/tmp/sketch.png
/tmp/marionnet-588078453.dir/test_machine/states
/tmp/marionnet-588078453.dir/test_machine/states/states-forest
/tmp/marionnet-588078453.dir/test_machine/states/ports-counters    # now states/ifconfig-counters
/tmp/marionnet-588078453.dir/test_machine/states/ports             # now states/ifconfig
/tmp/marionnet-588078453.dir/test_machine/states/defects
/tmp/marionnet-588078453.dir/test_machine/states/texts
/tmp/marionnet-588078453.dir/test_machine/scripts
/tmp/marionnet-588078453.dir/test_machine/netmodel
/tmp/marionnet-588078453.dir/test_machine/netmodel/dotoptions.marshal
/tmp/marionnet-588078453.dir/test_machine/netmodel/network.xml
/tmp/marionnet-588078453.dir/test_machine/hostfs
/tmp/marionnet-588078453.dir/test_machine/hostfs/2
/tmp/marionnet-588078453.dir/test_machine/hostfs/2/boot_parameters
/tmp/marionnet-588078453.dir/test_machine/hostfs/1
/tmp/marionnet-588078453.dir/test_machine/hostfs/1/boot_parameters

Here:
temporary_directory = "/tmp"
project_working_directory = "/tmp/marionnet-588078453.dir"
get_project_subdirs_prefix = "/tmp/marionnet-588078453.dir/test_machine/"
complete_dir "foo" = "/tmp/marionnet-588078453.dir/test_machine/foo"
*)

(** The class modelling the global state of the application. *)
class globalState = fun () ->
  let system = new Chip.system ~name:"motherboard" () in
  let win    = new Gui.window_MARIONNET () in
  let net    = new User_level.network () in
  object (self)

  (** Main window. *)
  method mainwin = win

  (** Virtual network. *)
  method network = net

  (** Motherboard is set in Motherboard_builder. *)
  method motherboard = Option.extract !Motherboard.content

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
  method app_state_as_string = match app_state#get with
  | NoActiveProject            -> "NoActiveProject"
  | ActiveNotRunnableProject   -> "ActiveNotRunnableProject"
  | ActiveRunnableProject      -> "ActiveRunnableProject"

  (** Are we working with an active project. *)
  method active_project = (app_state#get_alone <> NoActiveProject)

  val sensitive_when_Active   = Chip.wlist ~name:"sensitive_when_Active" []
  val sensitive_when_Runnable = Chip.wlist ~name:"sensitive_when_Runnable" []
  val sensitive_when_NoActive = Chip.wlist ~name:"sensitive_when_NoActive" []
  method sensitive_when_Active   : GObj.widget Chip.wlist = sensitive_when_Active
  method sensitive_when_Runnable : GObj.widget Chip.wlist = sensitive_when_Runnable
  method sensitive_when_NoActive : GObj.widget Chip.wlist = sensitive_when_NoActive

  (** The project filename. *)
  val project_filename = Chip.wref ~name:"project_filename" None
  method project_filename = project_filename

  (** The project name. *)
  val mutable project_name = Chip.wref ~name:"project_name" None
  method project_name = project_name

  (** by default, the name of the project is the basename of filename without the extension. *)
  method private default_project_name_of ~filename =
    let base = (Filename.basename filename) in
    try
      (Filename.chop_extension base)
    with _ -> base

  (** The parent of the project working directory: *)
  val temporary_directory = Chip.wref ~name:"temporary_directory" "/tmp"
  method temporary_directory = temporary_directory

  (** The project working directory: *)
  val project_working_directory = Chip.wref ~name:"project_working_directory" None
  method project_working_directory = project_working_directory

  method private make_the_project_working_directory =
    let pwd =
      UnixExtra.temp_dir
        ~parent:(temporary_directory#get)
        ~prefix:"marionnet-"
        ~suffix:".dir"
        ()
    in
    project_working_directory#set (Some pwd);
    pwd

  (** Supposing all optional value defined: *)
  method private complete_dir dir =
    let pwd = Option.extract project_working_directory#get in
    let prn = Option.extract project_name#get in
    FilenameExtra.concat_list [pwd; prn; dir]

  method private get_project_subdirs_prefix =  self#complete_dir ""

  method tmpDir         = self#complete_dir "tmp"
  method patchesDir     = self#complete_dir "states"
  method netmodelDir    = self#complete_dir "netmodel"
  method scriptsDir     = self#complete_dir "scripts"
  method hostfsDir      = self#complete_dir "hostfs"
  method classtestDir   = self#complete_dir "classtest"
  method dotSketchFile  = Filename.concat self#tmpDir "sketch.dot"
  method pngSketchFile  = Filename.concat self#tmpDir "sketch.png"
  method networkFile    = Filename.concat self#netmodelDir "network.xml"
  method dotoptionsFile = Filename.concat self#netmodelDir "dotoptions.marshal"
  
  method project_version_file = self#complete_dir "version"

 (* The treeview `ifconfig' may be used to derive the informations about the project version
    if necessary. This may be done inspecting the existence and the content of its related files:
    - `v0 is the version of marionnet 0.90.x series
    - `v1 is the version of trunk revno >= 445 with ocamlbricks revno >= 387 (2013/11/17) to trunk revno 460 (included);
          the treeview `ifconfig' is saved in an incompatible (forest) format in states/ports (as in `v0)
    - `v2 is the version of trunk revno >= 461 and marionnet 1.0;
          the treeview `ifconfig' is saved in an incompatible (forest) format in states/ifconfig, in order to prevent 
          seg-faults of old binaries reading a new project *)
  method opening_project_version : [ `v0 | `v1 | `v2 ] option = (* None stands for undefined, i.e. failed to identify *)
    try 
      let version = PervasivesExtra.get_first_line_of_file (self#project_version_file) in
      match version with
      | Some "v0" -> Some `v0   (* marionnet 0.90.x *)
      | Some "v1" -> Some `v1   (* trunk revno >= 445 with ocamlbricks revno >= 387 (2013/11/17) to trunk revno 460 (included) *)
      | Some "v2" -> Some `v2   (* trunk revno >= 461 and marionnet 1.0 *)
      | _         -> self#treeview#ifconfig#try_to_understand_in_which_project_version_we_are
    with _ -> None

  (* Project are saved anymway in the newest version: *)  
  method closing_project_version : [ `v0 | `v1 | `v2 ] = `v2
    
  method private string_of_project_version : [ `v0 | `v1 | `v2 ] -> string = 
    function `v0 -> "v0" | `v1 -> "v1" | `v2 -> "v2"

  method private project_version_of_string : string -> [ `v0 | `v1 | `v2 ] = 
    function "v0" -> `v0 | "v1" -> `v1 | "v2" -> `v2 | _ -> assert false
    
  (** New project which will be saved into the given filename. *)
  method new_project ~filename  =
    begin
      (* First reset the old network, waiting for all devices to terminate: *)
      self#network#ledgrid_manager#reset;
      self#network#reset () ;
      project_filename#set (Some filename) ;
      project_name#set (Some (self#default_project_name_of ~filename));
      ignore (self#make_the_project_working_directory);
      (* Create the directory skeleton: *)
      let prefix = self#get_project_subdirs_prefix in
      Unix.mkdir prefix 0o755 ;
      List.iter
        (fun x-> Unix.mkdir (Filename.concat prefix x) 0o755)
        ["states"; "netmodel"; "scripts"; "hostfs"; "classtest"; "tmp"];
      (* Treeview data should be saved within prefix: *)
      self#clear_treeviews;
      (* Reset dotoptions *)
      self#dotoptions#reset_defaults () ;
      (* Set the app_state. *)
      app_state#set ActiveNotRunnableProject ;
      (* Force GUI coherence. *)
      self#gui_coherence () ;
     (* Refresh the network sketch *)
      self#refresh_sketch () ;
    end


  (** Close the current project. The project is lost if the user hasn't saved it. *)
  method close_project =
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
         project_filename#set None ;
         (* Unset the project name. *)
         project_name#set None ;
         let cmd =
           Printf.sprintf
             "rm -rf %s"
             (Option.extract project_working_directory#get)
         in
         (* Update the network sketch (now empty) *)
         self#mainwin#sketch#set_file "" ;
         (* Unset the project working directory. *)
         project_working_directory#set None;
         (* Set the app_state. *)
         app_state#set NoActiveProject;
         (* Force GUI coherence. *)
         self#gui_coherence ();
         Log.printf1 "Destroying the old working directory (%s)...\n" cmd;
         Log.system_or_ignore cmd;
         Log.printf "Done.\n";
        end (* there was an active project *)
       );
      (* Clear all treeviews, just in case. *)
      self#clear_treeviews;
      Log.printf "state#close_project_sync: done.\n";
    end (* thunk *)
    in
    ignore (Task_runner.the_task_runner#schedule
              ~name:"close_project_sync"
              close_project_sync);

 (** Read the pseudo-XML file containing the network definition. *)
 method import_network
   ?(emergency:(unit->unit)=(fun x->x))
   ?(dotAction:(unit->unit)=fun x->x)
   ~project_version
   (f:filename) =
   begin
    (* Backup the network. *)
    self#network#save_to_buffers;
    (* Plan to restore the network if something goes wrong. *)
    let emergency = fun e ->
          Log.printf1 "state#import_network: emergency (%s)!!!\n" (Printexc.to_string e);
          self#network#restore_from_buffers;
          emergency ()
    in
    (* Read the given file. *)
    (if (Shell.regfile_readable f)
    then try
        let result = User_level.Xml.load_network ~project_version self#network f in
        Log.printf ("state#import_network: network imported\n");
        result
    with e -> (emergency e;  raise e)
    else begin
      emergency (Failure "file not readable");
      raise (Failure "state#import_network: cannot open the xml file")
      end
      );
    (* Fix the app_state and update it if necessary *)
    app_state#set ActiveNotRunnableProject ;
    self#update_state (); (* is it runnable? *)
    (* Undump Dot_tuning.network *)
    dotAction ();
    (* Force GUI coherence. *)
    self#gui_coherence ();
    (* Update the network sketch *)
    self#refresh_sketch ();
   end


  (** Close the current project and extract the given filename in a fresh project working directory. *)
  method open_project ~filename =
    begin
    (* First close the current project, if necessary: *)
    self#close_project;
    (* Set the project filename *)
    project_filename#set (Some filename);
    let pwd = self#make_the_project_working_directory in
    (* --- *)
    let opening_project_progress_bar =
      Progress_bar.make_progress_bar_dialog
       ~modal:true
       ~title:(s_ "Work in progress")
       ~text_on_label:(Printf.sprintf "<big><b>%s</b></big>" (s_ "Opening"))
       ~text_on_sub_label:(Printf.sprintf (f_ "<tt><small>%s</small></tt>") filename)
       ()
    in
    opening_project_progress_bar#show ();
    (* --- *)
    (* Extract the mar file into the pwdir *)
    let command_line =
      Printf.sprintf "tar -xSvzf %s -C %s"
        (Option.extract project_filename#get)
        pwd
    in
    (* --- *)
    let synchronous_loading () = begin
      (* --- *)
      Log.system_or_fail command_line;
      (* --- *)
      (* Look for the name of the root directory of the mar file. Some checks here. *)
      let mar_inner_dir =
	try
	  (match (SysExtra.readdir_as_list pwd) with
	  | [x] ->
	    let skel = (SysExtra.readdir_as_list (Filename.concat pwd x)) in
	    if ListExtra.subset skel ["states"; "netmodel"; "scripts"; "hostfs"; "classtest"; "version"] then x else (* continue: *)
	    failwith "state#open_project: no expected content in the project root directory."
	  |  _  ->
	    failwith "state#open_project: no rootname found in the project directory."
	  )
	with e -> begin
	  self#close_project;
	  raise e;
	end;
      in
      (* --- *)
      (* Set the project name *)
      project_name#set (Some mar_inner_dir);
      let prefix = self#get_project_subdirs_prefix in
      (* --- *)
      (* Create the tmp subdirectory. *)
      Unix.mkdir (prefix^"tmp") 0o755 ;
      (* --- *)
      (* Determine the version of the project we are opening: *)
      let project_version : [ `v0 | `v1 | `v2 ] = 
        match self#opening_project_version with
        | Some v -> v
        | None   -> failwith "state#open_project: project version cannot be identified"
      in
      let project_version_as_string = self#string_of_project_version project_version in
      Log.printf1 "state#open_project: project version is %s\n" (project_version_as_string);
      let () = 
        if project_version <> self#closing_project_version then 
        Simple_dialogs.warning
          (s_ "Project in old file format")
          (s_ "This project will be automatically converted in a format not compatible with previous versions of this software. If you want to preserve compatibility, don't save it or save it with another name.")
         ()
      in
      (* --- *)
      (* Dot_tuning.network will be undumped after the network,
	in order to support cable inversions. *)
      let dotAction () = begin
	(try
	  self#dotoptions#load_from_file self#dotoptionsFile;
	  Log.printf ("state#open_project: dotoptions recovered\n");
	with e ->
	  begin
	    Log.printf ("state#open_project: cannot read the dotoptions file => resetting defaults\n");
	    self#dotoptions#reset_defaults ()
	  end);
	self#dotoptions#set_toolbar_widgets ()
	end
      in
      (* --- *)
      Log.printf ("state#open_project: calling load_treeviews\n");
      (* Undump treeview's data. Doing this action now we allow components
	 to modify the treeviews according to the marionnet version: *)
      self#load_treeviews ~project_version ();
      (* --- *)
      Log.printf ("state#open_project: calling import_network\n");
      (* Second, read the xml file containing the network definition.
	If something goes wrong, close the project. *)
      (try
	self#import_network
	  ~emergency:(fun () -> self#close_project)
	  ~dotAction
	  ~project_version
	  self#networkFile
      with e ->
	self#clear_treeviews;
	Log.printf1 "Failed with exception %s\n" (Printexc.to_string e);
      );
      self#register_state_after_save_or_open;
      ()
    end (* function synchronous_loading *)
    (* --- *)
    in
    let _ =
      Task_runner.the_task_runner#schedule
        ~name:"state#open_project.synchronous_loading"
        (fun () ->
	    try
	      synchronous_loading ()
	    with e ->
	      begin
		Log.printf1 "Failed loading the project `%s'. The next reported exception is harmless.\n" filename;
		let error_msg =
		  Printf.sprintf "<tt><small>%s</small></tt>\n\n%s"
		    filename
		    (s_ "Please ensure that the file be well-formed.")
		in
		Simple_dialogs.error (s_ "Failed loading the project") error_msg ();
		raise e;
	      end)
    in
    (* Remove now the progress_bar: *)
    let _ =
      Task_runner.the_task_runner#schedule
        ~name:"destroy opening project progress bar"
        (fun () -> Progress_bar.destroy_progress_bar_dialog (opening_project_progress_bar))
    in
    ()
    end

  (*** BEGIN: this part of code tries to understand if the project must be really saved before exiting. *)

  val mutable refresh_sketch_counter_value_after_last_save = None
  method set_project_not_already_saved =
   refresh_sketch_counter_value_after_last_save <- None

  method treeview =
   object
     method ifconfig  = Treeview_ifconfig.extract ()
     method history   = Treeview_history.extract ()
     method defects   = Treeview_defects.extract ()
     method documents = Treeview_documents.extract ()
   end

  method private get_treeview_list : Treeview.t list =
   begin
    let t1 = Treeview_ifconfig.extract () in
    let t2 = Treeview_history.extract () in
    let t3 = Treeview_defects.extract () in
    let t4 = Treeview_documents.extract () in
    [ (t1 :> Treeview.t);
      (t2 :> Treeview.t);
      (t3 :> Treeview.t);
      (t4 :> Treeview.t);
      ]
   end

  method private load_treeviews ~project_version () =
    List.iter 
      (fun (treeview : Treeview.t) -> treeview#load ~project_version ())
      self#get_treeview_list

  method private save_treeviews =
    List.iter (fun (treeview : Treeview.t) -> treeview#save ()) self#get_treeview_list

  method private clear_treeviews =
    List.iter (fun (treeview : Treeview.t) -> treeview#clear) self#get_treeview_list

  method private get_treeview_complete_forest_list =
    List.map
      (fun (treeview : Treeview.t) -> treeview#get_complete_forest)
       self#get_treeview_list

  val mutable treeview_forest_list_after_save = None
  method private register_state_after_save_or_open =
   begin
     refresh_sketch_counter_value_after_last_save <- Some self#refresh_sketch_counter#get;
     treeview_forest_list_after_save <- Some (self#get_treeview_complete_forest_list);
   end

  method project_already_saved =
    (match refresh_sketch_counter_value_after_last_save, self#refresh_sketch_counter#get with
     (* Efficient test: *)
    | Some x, y when x=y ->
        Log.printf "The project *seems* already saved.\n";
        (* Potentially expensive test: *)
        if (treeview_forest_list_after_save = (Some self#get_treeview_complete_forest_list))
        then begin
          Log.printf "The project *is* already saved.\n";
          true
        end
        else begin
          Log.printf "Something has changed in treeviews: the project must be re-saved.\n";
          false
        end
    | Some x, y -> (Log.printf2 "The project seems not already saved (x=%d, y=%d).\n" x y; false)
    | None, y   -> (Log.printf1 "The project seems not already saved (x=None, y=%d).\n" y; false)
    )

  (*** END: this part of code try to understand if the project must be really saved before exiting. *)

  (** Rewrite the compressed archive prj_filename with the content of the project working directory (pwdir). *)
  method save_project =

    if self#active_project then begin
    Log.printf "state#save_project starting...\n";

    let filename = Option.extract project_filename#get in
    let project_working_directory = Option.extract project_working_directory#get in
    let project_name = Option.extract project_name#get in

    (* Progress bar periodic callback. *)
    let fill =
      (* disk usage (in kb) with the unix command *)
      let du x = match UnixExtra.run ("du -sk "^x) with
       | kb, (Unix.WEXITED 0) -> (try Some (float_of_string (List.hd (StringExtra.split ~d:'\t' kb))) with _ -> None)
       | _,_                  -> None
      in
      (* disk usage (in kb) with the unix library *)
      let du_file_in_kb x = try Some (float_of_int ((Unix.stat x).Unix.st_size / 1024)) with _ -> None in
      let round x = float_of_string (Printf.sprintf "%.2f" (if x<1. then x else 1.)) (* workaround strange lablgtk behaviour *) in
      match (du project_working_directory) with
      | Some kb_flatten ->
         fun () -> (match du_file_in_kb filename with
                    | Some kb_compressed -> round (0.05 +. (kb_compressed *. 8.) /. kb_flatten)
                    | None -> 0.5)
      | None -> fun () -> 0.5
    in

    let window =
      let text_about_saved_snapshots =
        match Global_options.Keep_all_snapshots_when_saving.extract () with
        | true  -> s_ "Project with all snapshots"
        | false -> s_ "Project with the most recent snapshots"
      in
      let saving_word = (s_ "Saving") in
      let text_on_label =
        Printf.sprintf "<big><b>%s</b></big>\n<small>%s</small>"
          saving_word
          text_about_saved_snapshots
      in
      Progress_bar.make_progress_bar_dialog
       ~modal:true
       ~title:(s_ "Work in progress")
       ~kind:(Progress_bar.Fill fill)
       ~text_on_label
       ~text_on_sub_label:(Printf.sprintf (f_ "<tt><small>%s</small></tt>") filename)
       ()
    in

    (* Write the network xml file *)
    User_level.Xml.save_network self#network self#networkFile ;

    (* Save also dotoptions for drawing image. *)
    self#dotoptions#save_to_file self#dotoptionsFile;

    (* Save treeviews (just to play it safe, because treeview files should be automatically)
       re-written at every update): *)
    self#save_treeviews;

    (* Save the project's version: *)
    let project_version_as_string = self#string_of_project_version (self#closing_project_version) in
    UnixExtra.put (self#project_version_file) (project_version_as_string);

    (* (Re)write the .mar file *)
    let cmd =
      let exclude_command_section =
        let excluded_cows = self#treeview#history#get_files_may_not_be_saved in
        let excluded_items = List.map (Printf.sprintf "--exclude states/%s") excluded_cows in
        String.concat " " ("--exclude tmp"::excluded_items)
      in
      Printf.sprintf "tar -cSvzf %s -C %s %s %s"
        filename
        project_working_directory
        exclude_command_section
        project_name
    in
    let _ =
      Task_runner.the_task_runner#schedule
        ~name:"tar"
        (fun () -> Log.system_or_ignore cmd)
    in
    let _ =
      Task_runner.the_task_runner#schedule
        ~name:"destroy saving progress bar"
        (fun () -> Progress_bar.destroy_progress_bar_dialog window)
    in
    self#register_state_after_save_or_open;
    Log.printf "state#save_project (main thread) finished.\n";
  end


  (** Update the project filename to the given string, and save: *)
  method save_project_as ~filename =
    if self#active_project then
      try
        let new_project_name = (self#default_project_name_of ~filename)
        in
        (* Set the project name *)
        self#change_project_name new_project_name;
        (* Set the project filename *)
        project_filename#set (Some filename) ;
        (* Save the project *)
        self#save_project;
      with e -> (raise e)

  (** Save the project into the given file, but without changing its name in the
      copy we're editing. Implemented by temporarily updating the name, saving
      then switch back to the old name. *)
  method copy_project_into ~filename =
    if self#active_project then
      try
        let original_project_name = Option.extract project_name#get in
        let original_filename = project_filename#get in
        let temporary_project_name = self#default_project_name_of ~filename in
        (* Temporarily update names...: *)
        self#change_project_name temporary_project_name;
        project_filename#set (Some filename) ;
        (* Save the project *)
        self#save_project;
        (* Reset names ensuring synchronisation *)
        Task_runner.the_task_runner#schedule ~name:"copy_project_into"
          (fun () ->
           (* Reset names to their old values: *)
           self#change_project_name original_project_name;
           project_filename#set original_filename);
      with e -> (raise e)

  (** Change the prj_name. This simply means to change the name
      of the mar root directory in the project working directory. *)
  method change_project_name new_name  =
    let old_name = Option.extract project_name#get in
    if new_name = old_name then () else
    begin
      (* Set the project name. *)
      project_name#set (Some new_name) ;

      (* Rename the root directory in the pwdir. *)
      let cmd =
        let pwd = Option.extract project_working_directory#get in
        let old_pathname = Filename.concat pwd old_name in
        let new_pathname = Filename.concat pwd new_name in
        Printf.sprintf "mv %s %s" old_pathname new_pathname
      in
      Log.system_or_ignore cmd;
    end

  val mutable sensitive_cables : GObj.widget list = []
  method add_sensitive_cable x = sensitive_cables <- x::sensitive_cables

  (** Forbid cable additions if there are not enough free ports; explicitly enable
      them if free ports are enough: *)
  method update_cable_sensitivity () =
    let condition = self#network#are_there_almost_2_free_endpoints in
    (List.iter (fun x->x#misc#set_sensitive condition) sensitive_cables)

  val refresh_sketch_counter = Chip.wcounter ~name:"refresh_sketch_counter" ()
  method refresh_sketch_counter = refresh_sketch_counter

  method refresh_sketch () =
   refresh_sketch_counter#set ();

  method network_change : 'a. ('a -> unit) -> 'a -> unit =
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
   begin
    let old = app_state in
    begin
    match app_state#get_alone with
     | NoActiveProject -> ()
     | _ -> app_state#set  (* TODO: this is a rule (chip) in a reactive system! *)
              (if self#network#nodes#get = []
                 then ActiveNotRunnableProject
                 else ActiveRunnableProject)
    end;
    if app_state <> old then self#gui_coherence () else ()
   end

 (** Force coherence between state and sensitive attributes of mainwin's widgets *)
 method gui_coherence () =
    self#update_state  () ;
    self#update_cable_sensitivity ();

 (* Begin of methods moved from talking.ml *)
 method make_names_and_thunks ?(node_list=self#network#nodes#get) (verb) (what_to_do_with_a_node) =
  List.map
    (fun node -> (
      (verb ^ " " ^ node#get_name),
      (fun () ->
	let progress_bar =
	  Simple_dialogs.make_progress_bar_dialog
	    ~title:(verb ^ " " ^ node#get_name)
	    ~text_on_bar:(s_ "Wait please...") ()
	in
	begin try
	  what_to_do_with_a_node node;
	  with e ->
	  Log.printf3 "Warning (q): \"%s %s\" raised an exception (%s)\n"
	    verb
	    node#name
	    (Printexc.to_string e);
	  Log.print_backtrace ();
	end;
	Simple_dialogs.destroy_progress_bar_dialog progress_bar))
    )
    node_list

 method do_something_with_every_node_in_sequence ?node_list (verb) (what_to_do_with_a_node) =
  List.iter
    (fun (name, thunk) -> Task_runner.the_task_runner#schedule ~name thunk)
    (self#make_names_and_thunks ?node_list verb what_to_do_with_a_node)

 method do_something_with_every_node_in_parallel ?node_list (verb) (what_to_do_with_a_node) =
  Task_runner.the_task_runner#schedule_parallel
    (self#make_names_and_thunks ?node_list verb what_to_do_with_a_node);;

 method startup_everything () =
  self#do_something_with_every_node_in_sequence
    ~node_list:(self#network#get_nodes_that_can_startup ())
    "Startup" (fun node -> node#startup_right_now)

 method shutdown_everything () =
  self#do_something_with_every_node_in_parallel
    ~node_list:(self#network#get_nodes_that_can_gracefully_shutdown ())
    "Shut down"
    (fun node -> node#gracefully_shutdown_right_now)

 method poweroff_everything () =
 (* self#do_something_with_every_node_in_sequence *)
  self#do_something_with_every_node_in_parallel
    ~node_list:(self#network#get_nodes_that_can_gracefully_shutdown ())
    "Power-off"
    (fun node -> node#poweroff_right_now)

(** Return true iff there is some node on or sleeping *)
 method is_there_something_on_or_sleeping () =
  let result =
    List.exists
      (fun node -> node#can_gracefully_shutdown || node#can_resume)
      (self#network#nodes#get)
  in
  begin
   Log.printf1 "is_there_something_on_or_sleeping: %s\n" (if result then "yes" else "no");
   result
  end

 (* End of functions moved from talking.ml *)

 (* When quit_async is called we have to quit really (for instance in a signal callback): *)
 val mutable quit_async_called = false
 method quit_async_called = quit_async_called

 method quit_async () =
   let quit () =
     begin
      Log.printf "Starting the last job...\n";
      self#network#destroy_process_before_quitting ();
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
   quit_async_called <- true;
   Task_runner.the_task_runner#schedule ~name:"quit" quit;
   Log.printf "Main thread: quit has been scheduled.\n";
   end

end;; (* class globalState *)
