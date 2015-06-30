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

type filename = string
 and pathname = string
;;

(* Example of hierarchy:

/tmp/marionnet-588078453.dir/
/tmp/marionnet-588078453.dir/sparse-swap-410647109
/tmp/marionnet-588078453.dir/sparse-swap-922455527
/tmp/marionnet-588078453.dir/foo/classtest
/tmp/marionnet-588078453.dir/foo/tmp
/tmp/marionnet-588078453.dir/foo/tmp/sketch.dot
/tmp/marionnet-588078453.dir/foo/tmp/sketch.png
/tmp/marionnet-588078453.dir/foo/states
/tmp/marionnet-588078453.dir/foo/states/states-forest
/tmp/marionnet-588078453.dir/foo/states/ports-counters    # now states/ifconfig-counters
/tmp/marionnet-588078453.dir/foo/states/ports             # now states/ifconfig
/tmp/marionnet-588078453.dir/foo/states/defects
/tmp/marionnet-588078453.dir/foo/states/texts
/tmp/marionnet-588078453.dir/foo/scripts
/tmp/marionnet-588078453.dir/foo/netmodel
/tmp/marionnet-588078453.dir/foo/netmodel/dotoptions.marshal
/tmp/marionnet-588078453.dir/foo/netmodel/network.xml
/tmp/marionnet-588078453.dir/foo/hostfs
/tmp/marionnet-588078453.dir/foo/hostfs/2
/tmp/marionnet-588078453.dir/foo/hostfs/2/boot_parameters
/tmp/marionnet-588078453.dir/foo/hostfs/1
/tmp/marionnet-588078453.dir/foo/hostfs/1/boot_parameters

Here:
temporary_directory                 = "/tmp"
project_working_directory           = "/tmp/marionnet-588078453.dir"
project_root_basename               = "foo"
project_root_pathname               = "/tmp/marionnet-588078453.dir/foo"
project_root_pathname_concat "bar"  = "/tmp/marionnet-588078453.dir/foo/bar"
*)

(** The class modelling the global state of the application. 
    All method with the suffix "_sync" are synchronous and they don't call the task manager.
    In other words, the caller of these methods should ensure the correct order of tasks. *)
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
  method motherboard = Motherboard.extract ()

  method system = system

  (** Access methods for the dot options, used for drawing the virtual network. *)
  method dotoptions = net#dotoptions

  (** Show something on statusbar. *)
  method flash ?(delay:int=2000) (msg:string) =
   let statusbar_ctx = win#statusbar#new_context "global" in
   statusbar_ctx#flash ~delay msg

  (** The project filename. *)
  val project_filename : string option Cortex.t = Cortex.return None
  method project_filename = project_filename

  (** Are we working with an active project? *)
  method active_project = Cortex.apply (project_filename) ((<>) None)

  (** Are we working with an active project with some node defined? *)
  method runnable_project = 
    self#active_project && (not self#network#is_node_list_empty)

  (* Containers for widgets that must be sensitive when a project is active, runnable or not active: *)
  val sensitive_when_Active   : GObj.widget StackExtra.t = StackExtra.create ()
  val sensitive_when_Runnable : GObj.widget StackExtra.t = StackExtra.create ()
  val sensitive_when_NoActive : GObj.widget StackExtra.t = StackExtra.create ()
  (* --- *)
  method sensitive_when_Active   = sensitive_when_Active
  method sensitive_when_Runnable = sensitive_when_Runnable
  method sensitive_when_NoActive = sensitive_when_NoActive

  val sensitive_cable_menu_entries : GObj.widget StackExtra.t = StackExtra.create ()
  method sensitive_cable_menu_entries = sensitive_cable_menu_entries
  
  (** The project root base name, i.e. the name of the root directory in the tarball containing the project. *)
  val mutable project_root_basename = Chip.wref ~name:"project_root_basename" None
  method project_root_basename = project_root_basename
  
  (** The project root base name is the basename of the filename without the extension 
      and without funny (UTF-8) chars (replaced by '_'). In this way we prevent some 
      troubles with UML and VDE tools. *)   
  method private project_root_basename_of_filename (filename) =
    let base = (Filename.basename filename) in
    let name = try (Filename.chop_extension base) with _ -> base in
    let result = StrExtra.Global.replace (Str.regexp "[^a-zA-Z0-9_-.]+") (fun _ -> "_") name in
    result
    
  (** The parent of the project working directory: *)
  val temporary_directory = Chip.wref ~name:"temporary_directory" "/tmp"
  method temporary_directory = temporary_directory

  (** The project working directory: *)
  val project_working_directory = Chip.wref ~name:"project_working_directory" None
  method project_working_directory = project_working_directory

  method private create_a_project_working_directory : pathname =
    let pwd =
      UnixExtra.temp_dir
        ~parent:(temporary_directory#get)
        ~prefix:"marionnet-"
        ~suffix:".dir"
        ()
    in
    let () = project_working_directory#set (Some pwd) in
    pwd

  (* Should be reactive! *)  
  method private set_filename_and_root_basename ?root_basename ~filename () = begin 
    let root = Option.extract_or_force (root_basename) (lazy (self#project_root_basename_of_filename filename)) in
    (* --- *)
    Cortex.set project_filename (Some filename);
    project_root_basename#set (Some root);
    end

  (* The optional argument ~root_basename is currently not provided but derived from ~filename: *)  
  method private change_filename_and_root_basename ?root_basename ~filename () = begin 
    let old_root = self#project_root_pathname in
    let       () = self#set_filename_and_root_basename ?root_basename ~filename () in
    let new_root = self#project_root_pathname in
    Unix.rename (old_root) (new_root);
    end

  method private unset_filename_and_root_basename = begin 
    Cortex.set project_filename (None);
    project_root_basename#set None;    (* Unset the project root basename *)
    end

  method private get_filename_and_root_basename =
    (Option.extract (Cortex.get project_filename),     
     Option.extract project_root_basename#get)
    
  (** Supposing all optional value defined (not equal to None): *)
  method private project_root_pathname =
    let pwd  = Option.extract project_working_directory#get in
    let root = Option.extract project_root_basename#get in
    Filename.concat (pwd) (root)
    
  method private project_root_pathname_concat x =
    let prefix = self#project_root_pathname in
    Filename.concat (prefix) (x)
  
  method tmpDir         = self#project_root_pathname_concat "tmp"
  method patchesDir     = self#project_root_pathname_concat "states"
  method netmodelDir    = self#project_root_pathname_concat "netmodel"
  method scriptsDir     = self#project_root_pathname_concat "scripts"
  method hostfsDir      = self#project_root_pathname_concat "hostfs"
  method classtestDir   = self#project_root_pathname_concat "classtest"
  method dotSketchFile  = Filename.concat self#tmpDir "sketch.dot"
  method pngSketchFile  = Filename.concat self#tmpDir "sketch.png"
  method networkFile    = Filename.concat self#netmodelDir "network.xml"
  method dotoptionsFile = Filename.concat self#netmodelDir "dotoptions.marshal"
  
  method project_version_file = self#project_root_pathname_concat "version"

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
    
  (** New project which will be saved into the given filename. 
      This method is synchronous: the caller should ensure the correct order of tasks. *)
  method new_project_sync ~filename  =
    (* First reset the old network, waiting for all devices to terminate: *)
    let () = self#network#ledgrid_manager#reset in
    let () = self#network#reset () in
    let () = self#set_filename_and_root_basename ~filename () in
    let _pwd = self#create_a_project_working_directory in
    (* --- *)
    (* Create the skeleton of project's directories.
       The `root_pathname' is the catenation of the project-working-directory and the root_basename 
       choosen according to the provided filename. *)
    let root_pathname = self#project_root_pathname in
    let () = Unix.mkdir (root_pathname) 0o755 in
    let () = 
      List.iter
        (fun x-> Unix.mkdir (Filename.concat root_pathname x) 0o755)
        ["states"; "netmodel"; "scripts"; "hostfs"; "classtest"; "tmp"]
    in
    (* Treeview data should be saved within prefix: *)
    let () = self#clear_treeviews in
    (* Reset dotoptions *)
    let () = self#dotoptions#reset_defaults () in
    (* Refresh the network sketch *)
    let () = self#refresh_sketch in
    ()

  (** Close the current project. The project is lost if the user hasn't saved it. 
      This method is synchronous: the caller should ensure the correct order of tasks. *)
  method close_project_sync = begin
    Log.printf "state#close_project_sync: starting...\n";
    (* Destroy whatever the LEDgrid manager is managing: *)
    self#network#ledgrid_manager#reset;
    (match self#active_project with
      | false -> Log.printf "state#close_project_sync: no project opened.\n"
      | true ->
      begin
        let () = self#network#reset ~scheduled:true () in
        let () = self#unset_filename_and_root_basename in
        (* --- *)
        let cmd = 
          Printf.sprintf "rm -rf '%s'"
            (Option.extract project_working_directory#get)
        in
        (* Update the network sketch (now empty) *)
        self#mainwin#sketch#set_file "" ;
        (* Unset the project working directory. *)
        project_working_directory#set None;
        (* --- *)
        Log.printf1 "state#close_project_sync: destroying the old project working directory (%s)...\n" cmd;
        Log.system_or_ignore cmd;
      end (* there was an active project *)
      );
    (* Clear all treeviews, just in case. *)
    self#clear_treeviews;
    Log.printf "state#close_project_sync: done.\n";
    end (* close_project_sync *)

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
    (if (UnixExtra.regfile_r_or_link_to f)
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
    (* Undump Dot_tuning.network *)
    dotAction ();
    (* Update the network sketch *)
    self#refresh_sketch;
   end


  (** Close the current project and extract the given filename in a fresh project working directory. *)
  method open_project ~filename =
    begin
    (* First close the current project, if necessary: *)
    let () = if self#active_project then self#close_project_sync else () in
    (* Set the project filename and the (expected) project root basename: *)
    let () = self#set_filename_and_root_basename ~filename () in
    let pwd = self#create_a_project_working_directory in
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
      Printf.sprintf "tar -xSvzf '%s' -C '%s'"
        (Option.extract (Cortex.get project_filename))
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
	  self#close_project_sync;
	  raise e;
	end;
      in
      (* --- *)
      (* Set the project root basename (again, but this time with the basename 
         read in the tarball): *)
      let () = project_root_basename#set (Some mar_inner_dir) in
      (* --- *)
      let prefix = self#project_root_pathname in
      (* --- *)
      (* Create the tmp subdirectory. *)
      let () = Unix.mkdir (Filename.concat prefix "tmp") 0o755 in
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
      let dotAction () =
        let () = 
	  try
	    let () = self#dotoptions#load_from_file ~project_version (self#dotoptionsFile) in
	    Log.printf ("state#open_project: dotoptions recovered\n")
	  with e ->
	    begin
	      Log.printf ("state#open_project: cannot read the dotoptions file => resetting defaults\n");
	      self#dotoptions#reset_defaults ()
	    end
	in
	self#dotoptions#set_toolbar_widgets ()
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
	  ~emergency:(fun () -> self#close_project_sync)
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
     refresh_sketch_counter_value_after_last_save <- Some (Cortex.get self#refresh_sketch_counter);
     treeview_forest_list_after_save <- Some (self#get_treeview_complete_forest_list);
   end

  method project_already_saved =
    (match refresh_sketch_counter_value_after_last_save, (Cortex.get self#refresh_sketch_counter) with
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
    (* --- *)
    let filename = Option.extract (Cortex.get project_filename) in
    let project_working_directory = Option.extract project_working_directory#get in
    let project_root_basename = Option.extract project_root_basename#get in
    (* --- *)
    (* Progress bar periodic callback. *)
    let fill =
      (* disk usage (in kb) with the unix command *)
      let du x = match UnixExtra.run (Printf.sprintf "du -sk '%s'" x) with
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
    (* --- *)
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
    (* --- *)
    (* Write the network xml file *)
    User_level.Xml.save_network self#network self#networkFile ;
    (* --- *)
    (* Save also dotoptions for drawing image. *)
    self#dotoptions#save_to_file self#dotoptionsFile;
    (* --- *)
    (* Save treeviews (just to play it safe, because treeview files should be automatically)
       re-written at every update): *)
    self#save_treeviews;
    (* --- *)
    (* Save the project's version: *)
    let project_version_as_string = self#string_of_project_version (self#closing_project_version) in
    UnixExtra.put (self#project_version_file) (project_version_as_string);
    (* --- *)
    (* (Re)write the .mar file *)
    let cmd =
      let exclude_command_section =
        let excluded_cows = self#treeview#history#get_files_may_not_be_saved in
        let excluded_items = List.map (Printf.sprintf "--exclude states/%s") excluded_cows in
        String.concat " " ("--exclude tmp"::excluded_items)
      in
      Printf.sprintf "tar -cSvzf '%s' -C '%s' %s '%s'"
        filename
        project_working_directory
        exclude_command_section
        project_root_basename
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
  method save_project_as ?root_basename ~filename () =
    if self#active_project then
      try
        (* Set the project filename, name and root_basename: *)
        self#change_filename_and_root_basename ?root_basename ~filename ();
        (* Save the project *)
        self#save_project;
      with e -> (raise e)

  (** Save the project into the given file, but without changing its name in the
      copy we're editing. Implemented by temporarily updating the name, saving
      then switch back to the old name. *)
  method copy_project_into ?root_basename ~filename () =
    if self#active_project then
      try
        let (filename0, root_basename0) = self#get_filename_and_root_basename in
        (* Set the project filename, name and root_basename: *)
        let () = self#change_filename_and_root_basename ?root_basename ~filename () in
        (* Save the project *)
        let () = self#save_project in
        (* Reset names ensuring synchronisation *)
        Task_runner.the_task_runner#schedule ~name:"copy_project_into"
          (fun () ->
             (* Revert to the previous names: *)
             let () = self#change_filename_and_root_basename ~root_basename:(root_basename0) ~filename:(filename0) () in
             ()
          );
      with e -> (raise e)

  method private really_refresh_sketch =
    let fs = self#dotSketchFile in
    let ft = self#pngSketchFile in
    try begin
      let ch = open_out fs in
      output_string ch (self#network#dotTrad ());
      close_out ch;
      let cmdline =
        let splines = string_of_bool (Cortex.get self#network#dotoptions#curved_lines) in (* Appel de methode Cortex !!!!!!!!!!!!!!! *)
        Printf.sprintf "dot -Gsplines=%s -Efontname=FreeSans -Nfontname=FreeSans -Tpng -o '%s' '%s'" splines ft fs
      in
      let exit_code = Sys.command cmdline in
      (* --- *)
      self#mainwin#sketch#set_file (self#pngSketchFile);
      (* --- *)
      (if not (exit_code = 0) then
        Simple_dialogs.error
          (s_ "dot failed")
          (Printf.sprintf
              (f_ "Invoking dot failed. Did you install graphviz?\n\
    The command line is\n%s\nand the exit code is %i.\n\
    Marionnet will work, but you will not see the network graph picture until you fix the problem.\n\
    There is no need to restart the application.")
              cmdline
              exit_code)
          ());
        end
      with e ->
        (Log.printf1
           "Warning: exception raised in really_refresh_sketch:\n%s\nIgnoring.\n"
           (Printexc.to_string e))
      
  (* The structure (counter) for the reactive sketch refreshing: *)
  val refresh_sketch_counter = Cortex.return 0
  method refresh_sketch_counter = refresh_sketch_counter

  (* Provoke the refreshing simply incrementing the counter (the on_commit reaction is defined elsewhere) *)
  method refresh_sketch =
   let _ = Cortex.move (refresh_sketch_counter) (fun x -> x+1) in ()

  (* --- *) 
  method network_change : 'a. ('a -> unit) -> 'a -> unit =
  fun action obj ->
   begin
    action obj;
    self#dotoptions#shuffler_reset;
    self#dotoptions#extrasize_reset;
    self#refresh_sketch;
   end

 (* Begin of methods moved from talking.ml *)
 method make_names_and_thunks ?(node_list=self#network#get_node_list) (verb) (what_to_do_with_a_node) =
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
      (self#network#get_node_list)
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

 initializer 
    let _ = Cortex.on_commit_append (refresh_sketch_counter) (fun _ _ -> self#really_refresh_sketch) in
    ()
    
end;; (* class globalState *)
