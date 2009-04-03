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


(** Gui completion for the menubar_MARIONNET widget defined with glade. *)

open Talking (* to be deleted *)

(* Shortcuts *)
module Unix = UnixExtra.Unix
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

let project         = add_menu "_Project"

module Common_dialogs = struct

 (* Dialog used both for "New" and "Open" *)
 let save_current () =
   if st#active_project
    then EDialog.ask_question ~help:None ~cancel:true
          ~gen_id:"save_current"
          ~title:"Fermer"
          ~question:"Voulez-vous enregistrer le projet courant ?" ()
    else (Some (mkenv [("save_current","no")]))

end

module Created_entry_project_new = Menu_factory.Make_entry
 (struct
   let text  = "Nouveau"
   let stock = `NEW
   let key   = (Some _N)

   let dialog =
     let filename () =
       EDialog.ask_for_fresh_writable_filename
         ~title:"Nom du nouveau projet"
         ~filters:[EDialog.MAR;EDialog.ALL]
         ~help:(Some Msg.help_nom_pour_le_projet) ()
     in
     (EDialog.sequence [Common_dialogs.save_current; filename])

   let reaction r = begin
     st#shutdown_everything ();
     let filename = check_path_name_validity_and_add_extension_if_needed (r#get "filename") in
     let actions () =
       begin
       st#close_project () ;
       st#new_project filename ;
       st#mainwin#window_MARIONNET#set_title (Command_line.window_title ^ " - " ^ filename);
       end in
     if (st#active_project) && ((r#get "save_current") = "yes")
      then
       (st#save_project ();
        Task_runner.the_task_runner#schedule actions)
      else
       (actions ())
     end

  end) (F)
let project_new = Created_entry_project_new.item


module Created_entry_project_open = Menu_factory.Make_entry
 (struct
   let text  = "Ouvrir"
   let stock = `OPEN
   let key   = (Some _O)

   let dialog =
     let filename_dialog () =
       EDialog.ask_for_existing_filename
         ~title:"Ouvrir un projet marionnet existant"
         ~filters:[EDialog.MAR;EDialog.ALL]
         ~help:(Some Msg.help_nom_pour_le_projet) ()
     in
     (EDialog.sequence [Common_dialogs.save_current; filename_dialog])

   let reaction r =
     begin
      st#shutdown_everything ();
      let filename = (r#get "filename") in
      let actions () = begin
         st#close_project () ;
         try
          st#open_project filename;
          st#mainwin#window_MARIONNET#set_title (Command_line.window_title ^ " - " ^ filename);
         with e -> ((Simple_dialogs.error "Ouvrir un projet" ("Échéc d'ouverture du fichier "^filename) ()); raise e)
        end in
      if (st#active_project) && ((r#get "save_current")="yes")
      then
       (st#save_project ();
        Task_runner.the_task_runner#schedule actions)
      else
       (actions ())
     end

  end) (F)
let project_open = Created_entry_project_open.item


let project_save =
  add_stock_item "Enregistrer"
    ~stock:`SAVE
    ~callback:(fun () ->
      if st#is_there_something_on_or_sleeping ()
	then Msg.error_saving_while_something_up ()
        else st#save_project ())
    ()

module Created_entry_project_save_as = Menu_factory.Make_entry
 (struct
   let text  = "Enregistrer sous"
   let stock = `SAVE_AS
   let key   = None

   let dialog () =
     EDialog.ask_for_fresh_writable_filename
       ~title:"Enregistrer sous"
       ~filters:[EDialog.MAR;EDialog.ALL]
       ~help:(Some Msg.help_nom_pour_le_projet) ()

   let reaction r =
     if st#is_there_something_on_or_sleeping () then Msg.error_saving_while_something_up () else
     let filename = check_path_name_validity_and_add_extension_if_needed ~extension:"mar" (r#get "filename") in
     try
      st#save_project_as filename;
      st#mainwin#window_MARIONNET#set_title (Command_line.window_title^" - "^filename);
     with _ -> (Simple_dialogs.error "Projet enregistrer sous" ("Échéc de la sauvegarde du project vers "^filename) ())

  end) (F)
let project_save_as = Created_entry_project_save_as.item


module Created_entry_project_copy_to = Menu_factory.Make_entry
 (struct
   let text  = "Copier sous"
   let stock = `SAVE_AS
   let key   = None

   let dialog () =
     EDialog.ask_for_fresh_writable_filename
       ~title:"Copier sous"
       ~filters:[EDialog.MAR;EDialog.ALL]
       ~help:(Some Msg.help_nom_pour_le_projet) ()

   let reaction r =
     if st#is_there_something_on_or_sleeping () then Msg.error_saving_while_something_up () else
     let filename = check_path_name_validity_and_add_extension_if_needed ~extension:"mar" (r#get "filename") in
     try
       st#copy_project_into filename
     with _ -> (Simple_dialogs.error "Projet copier sous" ("Échéc de la copie vers "^filename) ())

  end) (F)
let project_copy_to = Created_entry_project_copy_to.item


module Created_entry_project_close = Menu_factory.Make_entry
 (struct
   let text  = "Fermer"
   let stock = `CLOSE
   let key   = (Some _W)

   let dialog () = 
     EDialog.ask_question ~help:None ~cancel:true
       ~title:"Fermer"
       ~question:"Voulez-vous enregistrer le projet courant ?" ()

   let reaction r = begin
    st#shutdown_everything ();
    let () = if (st#active_project) && ((r#get "answer") = "yes")
              then st#save_project ()
              else () in
    st#close_project ();
    st#mainwin#window_MARIONNET#set_title Command_line.window_title; (* no project name *)
    end

  end) (F)
let project_close = Created_entry_project_close.item


let separator       = project#add_separator ()


module Created_entry_project_export = Menu_factory.Make_entry
 (struct
   let text  = "Exporter image"
   let stock = `CONVERT
   let key   = None

   let dialog () = 
     EDialog.ask_for_fresh_writable_filename
       ~title:"Exporter l'image du reseau"
       ~filters:[EDialog.PNG;EDialog.ALL]
       ~help:None ()

   let reaction r =
     let filename = check_path_name_validity_and_add_extension_if_needed ~extension:"png" (r#get "filename") in
     let command  = ("cp "^st#pngSketchFile^" "^filename) in
     let () =  Log.print_string "About to call Unix.run...\n"; flush_all () in
     try
      (match Unix.run command with
      |  (_ , Unix.WEXITED 0) -> st#flash ~delay:6000 ("Image du réseau exportée avec succès dans le fichier "^filename)
      |  _                    -> raise (Failure ("Echec durant l'exportation de l'image du réseau vers le fichier "^filename))
      )
     with e -> ((Simple_dialogs.error "Exporter l'image du reseau" ("Échec durant l'exportation vers le fichier "^filename) ()); raise e)

  end) (F)
let project_export = Created_entry_project_export.item


module Created_entry_project_quit = Menu_factory.Make_entry
 (struct
   let text  = "Quitter"
   let stock = `QUIT
   let key   = (Some _Q)
   let dialog () =
    if (st#active_project = false)
     then (Some (mkenv [("answer","no")]))
     else Talking.EDialog.ask_question ~help:None ~cancel:true
           ~title:"QUITTER"
           ~question:"Voulez-vous enregistrer\nle projet courant avant de quitter ?"
           ()
   let reaction r =
    let () = if (st#active_project) && ((r#get "answer") = "yes")
      then st#save_project ()
      else ()
    in st#quit ()

  end) (F)
let project_quit = Created_entry_project_quit.item


(* **************************************** *
                Menu "Options"
 * **************************************** *)

let options         = add_menu "_Options"

module Created_entry_options_cwd = Menu_factory.Make_entry
 (struct
   let text  = "Changer le répertoire de travail"
   let stock = `DIRECTORY
   let key   = None
   let dialog () =
    Talking.EDialog.ask_for_existing_writable_folder_pathname_supporting_sparse_files
       ~title:"Choisir un répertoire de travail"
       ~help:(Some Msg.help_repertoire_de_travail) ()
   let reaction r = st#set_wdir (r#get "foldername")
  end) (F)
let options_cwd = Created_entry_options_cwd.item

let options_autogenerate_ip_addresses =
 add_check_item "Auto-génération des adresses IP"
  ~active:Global_options.autogenerate_ip_addresses_default
  ~callback:(fun active ->
         Log.printf "You toggled the option (IP)\n"; flush_all ();
         Global_options.set_autogenerate_ip_addresses active)
   ()

let options_debug_mode                =
 add_check_item "Modalité debug"
  ~active:Global_options.debug_mode_default
  ~callback:(fun active ->
         Log.printf "You toggled the option (debug)\n"; flush_all ();
         Global_options.set_debug_mode active)

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

let help         = add_menu "_Aide"
let help_apropos =
 let module D = Gui_dialog_A_PROPOS.Make (State) in
 let callback () =
   let dialog = D.dialog () in
   let _ = dialog#closebutton_A_PROPOS#connect#clicked ~callback:(dialog#toplevel#destroy) in ()
 in add_stock_item "À propos" ~stock:`ABOUT ~callback ()


(* **************************************** *
                Sensitiveness
 * **************************************** *)

let () = List.iter (* when Active *)
          (fun w -> st#add_sensitive_when_Active w#coerce)
          [project_save; project_save_as; project_copy_to; project_close; project_export]

let () = List.iter (* when NoActive *)
          (fun w -> st#add_sensitive_when_NoActive w#coerce)
          [options_cwd]

end
