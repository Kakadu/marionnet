(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007  Jean-Vincent Loddo
   Copyright (C) 2007  Luca Saiu
   Updated by Luca Saiu in 2008.

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
open UnixExtra;;
open SysExtra;;

open Environment;;
open Mariokit;;

let sketch_mutex =
  Mutex.create ();;

let lock_sketch () =
  Mutex.lock sketch_mutex;;

let unlock_sketch () =
  Mutex.unlock sketch_mutex;;

(** Model for the global state of the application. 
    The sensitive or visible properties of some widgets depend on this value.*)
type application_state = 
  | NoActiveProject            (** Working with no project defined. User have to create or open. *)
  | ActiveNotRunnableProject   (** Working with a project with an empty or non runnable network. *) 
  | ActiveRunnableProject      (** Working with a runnable project. *) 
;;  

(** Set sensitive attributes of mainwin's widgets. *)
let set_sensitive (win:Gui.window_MARIONNET) app_state = 

  let l1 = [ win#notebook_CENTRAL#coerce#misc;
             win#imgitem_PROJET_ENREGISTRER#coerce#misc ;
             win#imgitem_PROJET_ENREGISTRER_SOUS#coerce#misc ;
             win#imgitem_PROJET_COPIER_SOUS#coerce#misc ;
             win#imgitem_PROJET_ENREGISTRER#coerce#misc ;
             win#imgitem_PROJET_FERMER#coerce#misc ;
             win#imgitem_PROJET_IMPORTER#coerce#misc ;
             win#imagemenuitem_PROJET_EXPORTER#coerce#misc;
             win#menuitem_DEBUG#coerce#misc ] in
 
  let l2 = [ win#hbuttonbox_BAS#coerce#misc ]     in
  let l3 = [ win#imgitem_OPTION_CWD#coerce#misc ] in

  match app_state with

  | NoActiveProject            
    ->  List.iter (fun x->x#set_sensitive false) (l1@l2) ;
        List.iter (fun x->x#set_sensitive true)  l3 

  | ActiveNotRunnableProject   
    ->  List.iter (fun x->x#set_sensitive true)  l1 ;
        List.iter (fun x->x#set_sensitive false) (l2@l3) 

  | ActiveRunnableProject 
    ->  List.iter (fun x->x#set_sensitive true)  (l1@l2) ;
        List.iter (fun x->x#set_sensitive true)  l2 ;
        List.iter (fun x->x#set_sensitive false) l3 

;;


(** Again *)
type filename = string;;


(** The class modelling the global state of the application. *)
class globalState = fun () -> 
  let win        = new Gui.window_MARIONNET ()           in
  let net        = new Netmodel.network ()               in

  let guiHandler = new Dotoptions.guiHandler win         in
  let dotoptions = new Dotoptions.network guiHandler net in
  let     _      = net#set_dotoptions dotoptions         in

  let statusbar_ctx = win#statusbar#new_context "global" in
  let dets = new Widget.textview ~view:win#details ()    in 

  object (self)
  
  (** Main window. *)
  method mainwin = win

  (** Virtual network. *)
  method network = net

  (** Access methods for the dot options, used for drawing the virtual network. *)
  method dotoptions = dotoptions

  method details = dets
 
  (** Show something on statusbar. *)
  method flash ?(delay:int=2000) (msg:string) = statusbar_ctx#flash ~delay msg

  (** The state of application.*)
  val mutable app_state : application_state = NoActiveProject    

  (** Are we working with an active project. *)
  method active_project = not (app_state=NoActiveProject)
 
  (** The project filename. *)
  val mutable prj_filename : string option = None    

  (** The project name. *)
  val mutable prj_name : string option = None   

  (** Get the project filename. *)
  method get_prj_filename = match prj_filename with | Some x -> x | None -> ""

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

  (** Project working directory. *)
  val mutable pwdir : string option = None    

  (** Get working directory. *)
  method get_wdir = wdir

  (** Get project working directory. *)
  method get_pwdir =
    match pwdir with
    | Some x ->
        x
    | None ->
        failwith "get_pwdir"

  (** Handlers for relevant project directories and files. *)
  method getDir dir     = 
    Printf.printf "getDir: calling get_pwdir\n"; flush_all ();
    let result = 
      (self#get_pwdir^"/"^(self#get_prj_name)^"/"^dir^"/")
    in
    Printf.printf "getDir: exiting with success\n"; flush_all ();
    result

  method tmpDir         = 
    Printf.printf "* calling getDir 1\n"; flush_all ();
    self#getDir "tmp" 
  method patchesDir     =
    Printf.printf "* calling getDir 2\n"; flush_all ();
    self#getDir "states" 
  method netmodelDir    =
    Printf.printf "* calling getDir 3\n"; flush_all ();
    self#getDir "netmodel" 
  method scriptsDir     =
    Printf.printf "* calling getDir 4\n"; flush_all ();
    self#getDir "scripts" 
  method hostfsDir      =
    Printf.printf "* calling getDir 5\n"; flush_all ();
    self#getDir "hostfs" 
  method classtestDir   =
    Printf.printf "* calling getDir 6\n"; flush_all ();
    self#getDir "classtest" 
  method dotSketchFile  =
    Printf.printf "* calling getDir 7\n"; flush_all ();
    self#tmpDir^"sketch.dot"
  method pngSketchFile  =
    Printf.printf "* calling getDir 8\n"; flush_all ();
    self#tmpDir^"sketch.png"
  method networkFile    =
    Printf.printf "* calling getDir 9\n"; flush_all ();
    self#netmodelDir^"network.xml"
  method dotoptionsFile =
    Printf.printf "* calling getDir 10\n"; flush_all ();
    self#netmodelDir^"dotoptions.marshal"

  (** New project which will be saved into the given filename. *)
  method new_project (x:filename)  = 
    begin
      (* First reset the old network, waiting for all devices to terminate: *)
      self#network#ledgrid_manager#reset;
      self#network#reset () ;

      (* Set the project filename *)
      prj_filename <- (Some x) ;
      
      (* Set the project name using the basename of filename whitout extension. *)
      prj_name     <-  Some (self#default_prj_name ()) ;

      (* Set the project working directory to a random name in the wdir. *)
      pwdir        <-  Some (Unix.temp_dir ~parent:wdir ~prefix:"MARIONNET-WORKING-" ~suffix:".dir" ());

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
      app_state <- ActiveNotRunnableProject ;

      (* Force GUI coherence. *)
      self#gui_coherence () ;

      prerr_endline ("*** in new_project");
     (* Refresh the network sketch *)
      self#refresh_sketch () ;
    
     ()
    end


  (** Close the current project. The project is lost if the user hasn't saved it. *)
  method close_project ()  = 
    print_string ">>>>>>>>>>CLOSING THE PROJECT: BEGIN<<<<<<<<\n";
    (* Destroy whatever the LEDgrid manager is managing: *)
    self#network#ledgrid_manager#reset;

    (if (app_state = NoActiveProject) then 
       print_string ">>>>>>>>>>(THERE'S NO PROJECT TO CLOSE)<<<<<<<<\n"
     else begin
      self#network#reset ();

      prerr_endline ("*** in close_project (calling update_sketch)");
     (* Update the network sketch (now empty) *)
      self#update_sketch () ; 

      (* Unset the project filename. *)
      prj_filename <- None ;
      
      (* Unset the project name. *)
      prj_name     <- None ;

      (* Unlink the project working directory content. *)
      Task_runner.the_task_runner#wait_for_all_currently_scheduled_tasks;
      Printf.printf "** DESTROYING THE OLD WORKING DIRECTORY... BEGIN\n"; flush_all ();
      ignore (Unix.system ("rm -rf "^self#get_pwdir));
      Printf.printf "** DESTROYING THE OLD WORKING DIRECTORY... DONE\n"; flush_all ();

      (* Unset the project working directory. *)
      pwdir        <-  None ;

      (* Set the app_state. *)
      app_state <- NoActiveProject ;

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
    print_string ">>>>>>>>>>CLOSING THE PROJECT: END<<<<<<<<\n";


 (** Read the pseudo-XML file containing the network definition. *)
 method import_network  ?(emergency:(unit->unit)=(fun x->x)) ?(dotAction:(unit->unit)=fun x->x) (f:filename) = 
   begin
   
   (* Backup the network. *)
   let backup = new Netmodel.network () in
   backup#copyFrom self#network ;

   (* Plan to restore the network if something goes wrong. *)
   let emergency = fun e -> 
         Printf.printf "import_network: emergency (%s)!!!\n" (Printexc.to_string e); flush_all ();
         self#network#copyFrom backup; 
         emergency () in

   (* Read the given file. *)
   (if (Shell.regfile_readable f) 
   then try 
       Netmodel.Xml.load_network self#network f ;
       prerr_endline ("import_network: network imported"); flush_all ();
   with e -> (emergency e;  raise e) 
   else ( emergency (Failure "file not readable"); raise (Failure "state#import_network: cannot open the xml file") ));
  
   (* Fix the app_state and update it if necessary *)
   app_state <- ActiveNotRunnableProject ; 
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
    prj_filename <- (Some x) ;
      
    (* Set the project working directory to a random name in the wdir. *)
    let working_directory = Unix.temp_dir ~parent:wdir ~prefix:"MARIONNET-WORKING-" ~suffix:".dir" () in
    pwdir <- Some working_directory;

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
        prerr_endline ("open_project: dotoptions recovered");
       with e -> (prerr_endline ("open_project: cannot read the dotoptions file => resetting defaults"); 
                   self#dotoptions#reset_defaults ())) ;
       self#dotoptions#write_gui ()
      end in

    prerr_endline ("open_project: calling import_network");

    (* Second, read the xml file containing the network definition. If something goes wrong, close the project. *)
    (try
    self#import_network ~emergency:self#close_project ~dotAction self#networkFile ;
    with e -> 
      Printf.printf "Failed with exception %s\n" (Printexc.to_string e);
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
    if not (app_state=NoActiveProject) then begin
      print_string ">>>>>>>>>>SAVING THE PROJECT: BEGIN<<<<<<<<\n";
(*        let progress_bar =
          Simple_dialogs.make_progress_bar_dialog
            ~title:("Saving the project into " ^ self#get_prj_filename) () in *)
(*         To do: Jean, che faccio con questo marshalling che era
           gia` commentato via?
           Quasi sicuramente nulla, visto che il salvataggio e` qua sotto,
           e l'ho gia` aggiustato... *)
        (* Marshal the network * )
         let s = Marshal.to_string st#network [Marshal.Closures] in 
         let _ = Unix.put self#networkFile s                    in
         *)

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
        let cmd = ("tar cSvzf "^(self#get_prj_filename)^" -C "^self#get_pwdir^" --exclude tmp "^(self#get_prj_name)) in
        let _ = (Unix.system cmd) in 
        prerr_endline ("cmd=<"^cmd^">");
(*        Simple_dialogs.destroy_progress_bar_dialog progress_bar; *)
        print_string ">>>>>>>>>>SAVING THE PROJECT: END<<<<<<<<\n";
      end


  (** Update the project filename to the given string, and save: *)
  method save_project_as (x:filename) =
    if not (app_state=NoActiveProject) then
      try
      begin
        let old_prj_name = self#get_prj_name in (* To do: Jean, a che serve questo? *)
        let new_prj_name = (self#default_prj_name ~arg:(Some x) ()) in

        (* Set the project name *)
        self#change_prj_name new_prj_name;

        (* Set the project filename *)
        prj_filename <- (Some x) ;

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
    if not (app_state=NoActiveProject) then
      try
      begin
        let original_prj_name = self#get_prj_name in
        let original_filename = prj_filename in
        let temporary_prj_name = (self#default_prj_name ~arg:(Some x) ()) in

        (* Temporarily update names...: *)
        self#change_prj_name temporary_prj_name;
        prj_filename <- (Some x) ;

        (* Save the project *)
        self#save_project ();

        (* Reset names to their old values: *)
        self#change_prj_name original_prj_name;
        prj_filename <- original_filename;

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
    Sys.command("dot -Efontname=FreeSans -Nfontname=FreeSans -Tpng -o "^ft^" "^fs) => ignore ;
    self#mainwin#sketch#set_file self#pngSketchFile ; 
    
    (* self#mainwin#sketch#set_pixbuf (Widget.Image.zoom 1.2 self#mainwin#sketch#pixbuf) ; *)
    (* Debugging *)
    prerr_endline ("============= DEBUGGING ===============");
    prerr_endline ("*** IDENTIFY :");
    Sys.command("identify "^ft) => ignore ;
    prerr_endline ("*** RATIO :");
    prerr_endline self#dotoptions#ratio ;
    prerr_endline ("===================================");

    self#details#rewrite ~tags:["center";"bold";"x-large"] "DOT FILE CONTENT\n\n"     ;
    self#details#append  ~tags:["monospace"] (Unix.cat fs) ;
    unlock_sketch ();

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
(*     Printf.printf "Free Ethernet ports are now %i\n" free_ethernet_ports_no; flush_all (); *)
    (if free_ethernet_ports_no < 2 then begin
      self#mainwin#imagemenuitem_CROISE_AJOUT#misc#set_sensitive false;
      self#mainwin#imagemenuitem_DROIT_AJOUT#misc#set_sensitive false;
    end
    else begin
      self#mainwin#imagemenuitem_CROISE_AJOUT#misc#set_sensitive true;
      self#mainwin#imagemenuitem_DROIT_AJOUT#misc#set_sensitive true;
    end);

  (** Refresh the sketch resetting the shuffle dotoption. 
      When a node is added or removed from the network you have to call the update_sketch, 
      else simply refresh_sketch. *)
  method update_sketch () =
    lock_sketch ();
    self#dotoptions#reset_shuffler  ();
    self#dotoptions#reset_extrasize ();
    unlock_sketch ();
    prerr_endline ("*** in update_sketch calling refresh_sketch");
    self#refresh_sketch (); (* this is already synchronized *)

  (** Update the state and force the gui coherence. 
      If a project is active, there are two possibilities: 
      1) there is at least a machine => the project is runnable 
      2) else => the project is not runnable *)  
  method update_state () = 
    let old = app_state in
    begin
    match app_state with 
     | NoActiveProject -> () 
     | _ -> app_state <- if self#network#nodes=[] 
                         then ActiveNotRunnableProject 
                         else ActiveRunnableProject 
    end ; 
    if app_state <> old then self#gui_coherence ();
    ()
     

 (** Force coherence between state and sensitive attributes of mainwin's widgets *)
 method gui_coherence () = 
    self#update_state () ;
    set_sensitive self#mainwin app_state ;
    self#update_cable_sensitivity ();

 (** For debugging *)
 method debugging () = 
   prerr_endline ("st.wdir="^wdir); 
   prerr_endline ("st.pwdir="^(try self#get_pwdir with _ ->"")); 
   prerr_endline ("st.prj_filename="^self#get_prj_filename); 
   prerr_endline ("st.prj_name="^self#get_prj_name)

 method mrPropre () = 
    if not (app_state=NoActiveProject) then raise (Failure "A project is still open, I cannot clean the wdir!")
    begin
    let _ = Unix.system ("rmdir "^wdir) in ()
    end

end;; (* class globalState *)
