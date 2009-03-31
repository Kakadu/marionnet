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
module Unix = UnixExtra.Unix

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

(* Menu "Project" *)

let project         = add_menu "_Project"
let project_new     = add_stock_item "Nouveau"          ~stock:`NEW     ~key:_N ~callback:(Talking_PROJECT_NEW.callback st)       ()
let project_open    = add_stock_item "Ouvrir"           ~stock:`OPEN    ~key:_O ~callback:(Talking_PROJECT_OPEN.callback st)      ()
let project_save    = add_stock_item "Enregistrer"      ~stock:`SAVE            ~callback:(Talking_PROJECT_SAVE.callback st)      ()
let project_save_as = add_stock_item "Enregistrer sous" ~stock:`SAVE_AS         ~callback:(Talking_PROJECT_SAVE_AS.callback st)   ()
let project_copy_to = add_stock_item "Copier sous"      ~stock:`SAVE_AS         ~callback:(Talking_PROJECT_COPY_INTO.callback st) ()
let project_close   = add_stock_item "Fermer"           ~stock:`CLOSE   ~key:_W ~callback:(Talking_PROJECT_CLOSE.callback st)     ()

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

(* Menu "Options" *)

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

(* Menu "Help" *)

let help         = add_menu "_Aide"
let help_apropos =
 let module D = Gui_dialog_A_PROPOS.Make (State) in
 let callback () =
   let dialog = D.dialog () in
   let _ = dialog#closebutton_A_PROPOS#connect#clicked ~callback:(dialog#toplevel#destroy) in ()
 in add_stock_item "À propos" ~stock:`ABOUT ~callback ()

(* Sensitiveness *)

let () = List.iter (* when Active *)
          (fun w -> st#add_sensitive_when_Active w#coerce)
          [project_save; project_save_as; project_copy_to; project_close; project_export]

let () = List.iter (* when NoActive *)
          (fun w -> st#add_sensitive_when_NoActive w#coerce)
          [options_cwd]

end
