(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2009  Jean-Vincent Loddo

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


open Gettext;;

(** Gui completion for the menubar_MARIONNET widget defined with glade. *)

(* Shortcuts *)
module EDialog = Talking.EDialog
module Msg = Talking.Msg
let mkenv = Environment.make_string_env
let check_path_name_validity_and_add_extension_if_needed =
  Talking.check_path_name_validity_and_add_extension_if_needed

open GdkKeysyms
open GtkStock

module Make (State:sig val st:State.globalState end) = struct

open State

(* Create the factory linked to the menubar. *)
module F = Menu_factory.Make (struct
  let parent = Menu_factory.Menubar st#mainwin#menubar_MARIONNET
  let window = st#mainwin#window_MARIONNET
end)
include F

(* **************************************** *
                Menu "Project"
 * **************************************** *)

let project         = add_menu (s_ "_Project" )

module Common_dialogs = struct

 (* Dialog used both for "New" and "Open" *)
 let save_current () =
   if st#active_project
    then EDialog.ask_question ~help:None ~cancel:true
          ~gen_id:"save_current"
          ~title:(s_ "Close" )
          ~question:(s_ "Do you want to save the current project?") ()
    else (Some (mkenv [("save_current","no")]))

end

type env  = string Environment.string_env
let env_to_string (t:env) = t#to_string (fun s->s)

module Created_entry_project_new = Menu_factory.Make_entry
 (struct
   type t = env
   let to_string = env_to_string
   let text  = (s_ "New" )
   let stock = `NEW
   let key   = (Some _N)

   let dialog =
     let filename () =
       EDialog.ask_for_fresh_writable_filename
         ~title:(s_ "Name of the new project" )
         ~filter_names:[`MAR;`ALL]
         ~help:(Some Msg.help_nom_pour_le_projet) ()
     in
     (EDialog.sequence [Common_dialogs.save_current; filename])

   let reaction r = begin
     st#shutdown_everything ();
     let filename = check_path_name_validity_and_add_extension_if_needed (r#get "filename") in
     let actions () =
       begin
       st#close_project;
       st#new_project filename ;
       end in
     if (st#active_project) && ((r#get "save_current") = "yes")
      then
       (st#save_project;
        Task_runner.the_task_runner#schedule ~name:"new project" actions)
      else
       (actions ())
     end

  end) (F)
let project_new = Created_entry_project_new.item


module Created_entry_project_open = Menu_factory.Make_entry
 (struct
   type t = env
   let to_string = env_to_string
   let text  = (s_ "Open" )
   let stock = `OPEN
   let key   = (Some _O)

   let dialog =
     let filename_dialog () =
       EDialog.ask_for_existing_filename
         ~title:(s_ "Open an existing Marionnet project" )
         ~filter_names:[`MAR; `ALL]
         ~help:(Some Msg.help_nom_pour_le_projet) ()
     in
     (EDialog.sequence [Common_dialogs.save_current; filename_dialog])

   let reaction r =
     begin
      st#shutdown_everything ();
      let filename = (r#get "filename") in
      let actions () = begin
         st#close_project;
         try
          st#open_project filename;
         with e -> ((Simple_dialogs.error (s_ "Open a project") ((s_ "Failed to open the file ")^filename) ()); raise e)
        end in
      if (st#active_project) && ((r#get "save_current")="yes")
      then
       (st#save_project;
        Task_runner.the_task_runner#schedule ~name:"open_project" actions)
      else
       (actions ())
     end

  end) (F)
let project_open = Created_entry_project_open.item


let project_save =
  add_stock_item (s_ "Save" )
    ~stock:`SAVE
    ~callback:(fun () ->
      if st#is_there_something_on_or_sleeping ()
	then Msg.error_saving_while_something_up ()
        else st#save_project)
    ()

module Created_entry_project_save_as = Menu_factory.Make_entry
 (struct
   type t = env
   let to_string = env_to_string
   let text  = (s_ "Save as" )
   let stock = `SAVE_AS
   let key   = None

   let dialog () =
     EDialog.ask_for_fresh_writable_filename
       ~title:(s_ "Save as" )
       ~filter_names:[`MAR; `ALL]
       ~help:(Some Msg.help_nom_pour_le_projet) ()

   let reaction r =
     if st#is_there_something_on_or_sleeping () then Msg.error_saving_while_something_up () else
     let filename = check_path_name_validity_and_add_extension_if_needed ~extension:"mar" (r#get "filename") in
     try
      st#save_project_as filename;
     with _ -> (Simple_dialogs.error (s_ "Save project as") ((s_ "Failed to save the project into the file ")^filename) ())

  end) (F)
let project_save_as = Created_entry_project_save_as.item


module Created_entry_project_copy_to = Menu_factory.Make_entry
 (struct
   type t = env
   let to_string = env_to_string
   let text  = (s_ "Copy to" )
   let stock = `SAVE_AS
   let key   = None

   let dialog () =
     EDialog.ask_for_fresh_writable_filename
       ~title:(s_ "Copy to" )
       ~filter_names:[`MAR; `ALL]
       ~help:(Some Msg.help_nom_pour_le_projet) ()

   let reaction r =
     if st#is_there_something_on_or_sleeping () then Msg.error_saving_while_something_up () else
     let filename = check_path_name_validity_and_add_extension_if_needed ~extension:"mar" (r#get "filename") in
     try
       st#copy_project_into filename
     with _ -> (Simple_dialogs.error (s_ "Project copy to" ) ((s_ "Failed to copy the project into the file ")^filename) ())

  end) (F)
let project_copy_to = Created_entry_project_copy_to.item


module Created_entry_project_close = Menu_factory.Make_entry
 (struct
   type t = env
   let to_string = env_to_string
   let text  = (s_ "Close" )
   let stock = `CLOSE
   let key   = (Some _W)

   let dialog () = 
     EDialog.ask_question ~help:None ~cancel:true
       ~title:(s_ "Close" )
       ~question:(s_ "Do you want to save the current project?") ()

   let reaction r = begin
    st#shutdown_everything ();
    let () = if (st#active_project) && ((r#get "answer") = "yes")
              then st#save_project
              else () in
    st#close_project;
    end

  end) (F)
let project_close = Created_entry_project_close.item


let separator       = project#add_separator ()

module Created_entry_project_export = Menu_factory.Make_entry
 (struct
   type t = env
   let to_string = env_to_string
   let text  = (s_ "Export image" )
   let stock = `CONVERT
   let key   = None

   let dialog () =
     let extra_widget = 
       let (combo_box, get_selected) = Dot_widget.combo_of_working_output_formats ~active:`png () in
       let widget_reader () =
	 let frm = get_selected () in
	 Dot.string_of_output_format frm
       in
       let table = GPack.table ~rows:2 ~columns:1 ~row_spacings:10 ~homogeneous:false () in
       let _ = GMisc.label
         ~xalign:0.5
         ~markup:("<b>"^(s_ "Output format")^"</b>")
         ~packing:(table#attach ~left:0 ~top:0) ()
       in
       (table#attach ~left:0 ~top:1 combo_box#coerce);
       (table#coerce, widget_reader)
     in
     EDialog.ask_for_fresh_writable_filename
       ~title:(s_ "Export network image" )
       ~filters:(Dot_widget.make_all_working_filters ())
       ~filter_names:[`ALL]
       ~extra_widget
       ~help:None ()

   let reaction r =
     let output_format = (r#get "extra_widget") in
     let filename = check_path_name_validity_and_add_extension_if_needed ~extension:output_format (r#get "filename") in
     let command = Printf.sprintf "dot -T%s -o %s %s" output_format filename st#dotSketchFile in
     let on_error () =
	Simple_dialogs.error
	  "Export network image"
	  ((s_ "Failed to export network image to the file ")^filename^" (format "^output_format^")")
	  ()
     in
     try
       Log.system_or_fail command;
       st#flash ~delay:8000 ((s_ "Network image correctly exported to the file ")^filename)
     with _ -> on_error ()

  end) (F)
let project_export = Created_entry_project_export.item


module Created_entry_project_quit = Menu_factory.Make_entry
 (struct
   type t = env
   let to_string = env_to_string
   let text  = (s_ "Quit")
   let stock = `QUIT
   let key   = (Some _Q)

   let dialog () =
    if ((not st#active_project) || st#project_already_saved)
     then (Some (mkenv [("answer","no")]))
     else Talking.EDialog.ask_question ~help:None ~cancel:true
           ~title:(s_ "Quit")
           ~question:(s_ "Do you want to save\nthe current project before quitting?")
           ()

   let reaction r =
    (* At this point the user really wants to quit the application. *)
    let save = (st#active_project) && ((r#get "answer") = "yes") in
    (match st#is_there_something_on_or_sleeping (), save with
     | true, true  ->
         st#shutdown_everything ();
         st#save_project;
     | true, false ->
         st#poweroff_everything ();
     | false, true ->
         st#save_project;
     | false, false -> ()
     );
    Log.printf "Killing the death monitor thread...\n";
    Death_monitor.stop_polling_loop ();
    st#network#destroy_process_before_quitting ();
    st#close_project;
    st#quit_async ()

  end) (F)
let project_quit = Created_entry_project_quit.item


(* **************************************** *
                Menu "Options"
 * **************************************** *)

let options         = add_menu (s_ "_Options")

module Created_entry_options_cwd = Menu_factory.Make_entry
 (struct
   type t = env
   let to_string = env_to_string
   let text  = (s_ "Change working directory")
   let stock = `DIRECTORY
   let key   = None
   let dialog () =
    Talking.EDialog.ask_for_existing_writable_folder_pathname_supporting_sparse_files
       ~title:(s_ "Choose working directory")
       ~help:(Some Msg.help_repertoire_de_travail) ()
   let reaction r = st#temporary_directory#set (r#get "foldername")
  end) (F)
let options_cwd = Created_entry_options_cwd.item

let options_autogenerate_ip_addresses =
 add_check_item (s_ "Auto-generation of IP address" )
  ~active:Global_options.autogenerate_ip_addresses_default
  ~callback:(fun active ->
         Log.printf "You toggled the option (IP)\n";
         Global_options.set_autogenerate_ip_addresses active)
   ()

let options_debug_mode                =
 add_check_item (s_ "Debug mode")
  ~active:(Global_options.Debug_level.are_we_debugging ())
  ~callback:(fun active ->
         Log.printf "You toggled the option (debug)\n";
         Global_options.Debug_level.set 1)

 ()

(** Hidden to user in this version. *)
let workaround_wirefilter_problem      =
 add_check_item "Workaround wirefilter problem"
  ~active:Global_options.workaround_wirefilter_problem_default
  ~callback:(fun active ->
         Log.printf "You toggled the option (wirefilter)\n"; flush_all ();
         Global_options.set_workaround_wirefilter_problem active)

 ()

let () = workaround_wirefilter_problem#coerce#misc#hide ()

(* **************************************** *
                Menu "Help"
 * **************************************** *)

let help         = add_menu (s_ "_Help")
let help_apropos =
 let module D = Gui_dialog_A_PROPOS.Make (State) in
 let callback () =
   let dialog = D.dialog () in
   let _ = dialog#closebutton_A_PROPOS#connect#clicked ~callback:(dialog#toplevel#destroy) in ()
 in add_stock_item (s_ "Help") ~stock:`ABOUT ~callback ()


(* **************************************** *
                Sensitiveness
 * **************************************** *)

let () = List.iter (* when Active *)
          (fun w -> st#sensitive_when_Active#add w#coerce)
          [project_save; project_save_as; project_copy_to; project_close; project_export]

let () = List.iter (* when NoActive *)
          (fun w -> st#sensitive_when_NoActive#add w#coerce)
          [options_cwd]

end
