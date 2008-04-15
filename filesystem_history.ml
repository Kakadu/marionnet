(* This file is part of Marionnet, a virtual network laboratory
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

(* To do: rename 'state' into 'row' *)

open PreludeExtra.Prelude;; (* We want synchronous terminal output *)
open Treeview;;
open Pathnames;;
open Sugar;;
open Row_item;;
open StrExtra;;
open Strings;;

(** A function to be called for starting up a given device in a given state. This very ugly
    kludge is needed to avoid a cyclic depencency between mariokit and states_interface *)
let the_startup_functions : ((string -> bool) * (string -> unit)) option ref =
  ref None;;
let reset_startup_functions () =
  the_startup_functions := None;;
let set_startup_functions can_startup startup =
  match !the_startup_functions with
    None ->
      the_startup_functions := Some(can_startup, startup)
  | (Some _) ->
      failwith "the startup functions were initialized twice";;
let get_startup_functions () =
  match !the_startup_functions with
    None ->
      failwith "the startup functions are not initialized yet"
  | (Some(can_startup, startup)) ->
      can_startup, startup;;

(** Create a fresh filename, without making the file (empty cow files are not allowed) *)
let make_temporary_file_name () =
  Printf.sprintf
    "%i-%i-%i.cow"
    (Random.int 65536)
    (Random.int 65536)
    (Random.int 65536);;

let make_fresh_state_file_name () =
  make_temporary_file_name ();;

let the_states_directory : string option ref =
  ref None;;
let get_states_directory () =
  match ! the_states_directory with
    None ->
      failwith "the states directory was not initialized yet"
  | (Some the_states_directory) ->
      the_states_directory;;

let get_variants_path prefixed_filesystem =
   marionnet_home_filesystems ^ "/" ^ prefixed_filesystem ^ "_variants/";;

class states_interface =
fun ~packing
    () -> 
object(self)
  inherit
    treeview
      ~packing
      ~hide_reserved_fields:true
      ()

  method startup_in_state row_id =
    let correct_date = item_to_string (self#get_row_item row_id "Timestamp") in
    let cow_file_name = item_to_string (self#get_row_item row_id "File name") in
    let name = item_to_string (self#get_row_item row_id "Name") in
    (* Ugly kludge: temporarily change the date of the state we want to duplicate,
       so that it becomes the most recent. Then startup the machine, and eventually
       reset the date fields to its correct value: *)
(*     print_string ("\n\n+++ Starting up "^name^" with "^cow_file_name^"\n\n"); flush_all (); *)
(*     self#set_row_item row_id "Timestamp" (String "9999-99-99 99:99"); *)
    self#set_row_item row_id "Timestamp" (String (Timestamp.current_timestamp_as_string ()));
    let _, startup_function = get_startup_functions () in
    let () = startup_function name in
    Task_runner.the_task_runner#schedule
      (fun () ->
        self#set_row_item row_id "Timestamp" (String correct_date));
    self#save
  
  method delete_state row_id =
    let name = item_to_string (self#get_row_item row_id "Name") in
    let file_name = item_to_string (self#get_row_item row_id "File name") in
    (* Remove the full row: *)
    self#remove_row row_id;
    (* Remove the cow file: *)
    (try Unix.unlink ((get_states_directory ()) ^ file_name) with _ -> ());
    let most_recent_row_for_name = self#get_the_most_recent_state_with_name name in
    let id_of_the_most_recent_row_for_name =
      lookup_alist "_id" most_recent_row_for_name in
    Forest.iter
      (fun a_row _ ->
        let an_id = lookup_alist "_id" a_row in
        let a_name = lookup_alist "Name" a_row in
        if a_name = String name then
          (if an_id = id_of_the_most_recent_row_for_name then
            self#highlight_row (item_to_string an_id)
          else
            self#unhighlight_row (item_to_string an_id)))
      (self#get_complete_forest);
    self#save

  method get_the_most_recent_state_with_name name =
    let forest = self#get_complete_forest in
    let relevant_forest =
      Forest.filter
        (fun row -> ((lookup_alist "Name" row) = String name))
      forest in
    let relevant_states =
      Forest.linearize relevant_forest in (* the forest should be a tree *)
(*
    List.iter
      (fun row ->
        Printf.printf "** Candidate most recent: %s\n"
          (item_to_string (lookup_alist "Timestamp" row));
        flush_all ()); *)
    Printf.printf "Relevant states for %s are %i\n" name (List.length relevant_states);
    assert ((List.length relevant_states) > 0);
    let result =
    List.fold_left
      (fun maximum row ->
        let timestamp_maximum =
          item_to_string (lookup_alist "Timestamp" maximum) in
        let timestamp_row =
          item_to_string (lookup_alist "Timestamp" row) in
        if timestamp_maximum > timestamp_row then
          maximum
        else
          row)
      (List.hd relevant_states)
      ((*List.tl*) relevant_states) in
(*    Printf.printf "The most recent state has timestamp \'%s\'\n"
      (item_to_string (lookup_alist "Timestamp" result)); *)
    flush_all ();
    result

  method number_of_states_such_that predicate =
    let linearized_complete_forest = Forest.linearize self#get_complete_forest in
    List.length (List.filter predicate linearized_complete_forest)

  method number_of_states_with_name name =
    self#number_of_states_such_that
      (fun complete_row ->
        (lookup_alist "Name" complete_row) = String name)

  method export_as_machine_variant row_id =
    self#export_as_variant ~router:false row_id

  method export_as_router_variant row_id =
    self#export_as_variant ~router:true row_id
    
  method private export_as_variant ~router row_id =
    let device_name = item_to_string (self#get_row_item row_id "Name") in
    let can_startup, _ = get_startup_functions () in
    (* We can only export the cow file if we are not running the device: *)
    if not (can_startup device_name) then
      Simple_dialogs.error
        ("La device "^device_name^" est in exécution")
        "Vous devez l'arrêter au préalable."
        ()
    else if router then begin (* the given row is about a router *)
      (* We don't need to choose a name because there can be only one variant for
         routers; don't ask anything: just export. *)
      let cow_name = item_to_string (self#get_row_item row_id "File name") in
      let prefixed_filesystem = item_to_string (self#get_row_item row_id "Prefixed filesystem") in
      self#actually_export_as_variant
        prefixed_filesystem
        cow_name
        "default";
    end else begin (* the given row is about a machine *)
      (* Just show the dialog window, and bind a method which does all the real work to the
         'Ok' button. This continuation-based logic is the best we can do here, because we
         can't loop waiting for the user without giving control back to Gtk+: *)
      let cow_name = item_to_string (self#get_row_item row_id "File name") in
      let prefixed_filesystem = item_to_string (self#get_row_item row_id "Prefixed filesystem") in
      Simple_dialogs.ask_text_dialog
        ~title:"Choix du nom de la variante"
        ~label:"Entrez le nom de la nouvelle variante; ce nom doit commencer par une lettre et peut contenir des lettres, chiffres, tirets et tirets bas."
        ~constraint_predicate:(fun s ->
                                 (String.length s > 0) &&
                                 (Str.wellFormedName ~allow_dash:true s))
        ~invalid_text_message:"Le nom doit commencer par une lettre et peut contenir des lettres, chiffres, tirets et tirets bas."
        ~enable_cancel:true
        ~ok_callback:(fun variant_name ->
          self#actually_export_as_variant
            prefixed_filesystem
            cow_name
            variant_name)
        ();
    end

  method private actually_export_as_variant prefixed_filesystem cow_name variant_name =
    (* Perform the actual copy: *)
    let variants_path = get_variants_path prefixed_filesystem in
    let cow_path = get_states_directory () in
    let new_variant_pathname = variants_path ^ "/" ^ variant_name in
    let command_line =
      Printf.sprintf
        "(mkdir '%s' &> /dev/null || true); (test ! -f '%s/%s' || cp --sparse=always '%s/%s' '%s')"
        variants_path
        cow_path cow_name
        cow_path cow_name
        new_variant_pathname in
    (match Unix.system command_line with
      (Unix.WEXITED 0) ->
        (* Ok, everything went smooth. *)
        Simple_dialogs.info
          "Succès"
          ("La variante a été exportée dans le fichier \"" ^ new_variant_pathname ^ "\".")
          ()
    | _ -> begin
      (* Remove any partial copy: *)
      (try
        Unix.unlink new_variant_pathname
      with _ ->
        ());
      Simple_dialogs.error
        "Échec"
        ("La variante n'a pu être exportée dans le fichier \"" ^ new_variant_pathname ^ "\".") 
        ()
    end)

  initializer
    (* Make columns: *)
    let _ =
      self#add_string_column
        ~header:"Name"
        ~shown_header:"Nom"
        () in
    let _ =
      self#add_icon_column
        ~header:"Type"
        ~shown_header:"Type"
        ~strings_and_pixbufs:[ "router", marionnet_home_images^"treeview-icons/router.xpm";
                               "machine", marionnet_home_images^"treeview-icons/machine.xpm"; ]
        () in
    let _ =
      self#add_string_column
        ~header:"Activation scenario"
        ~default:(fun () -> String "[No scenario]")
        ~hidden:true
        ~italic:true
        () in
    let _ =
      self#add_string_column
        ~header:"Timestamp"
        ~shown_header:"Date"
        ~default:(fun () -> String (Timestamp.current_timestamp_as_string ()))
        () in
    (*
    let _ =
      self#add_checkbox_column
        ~header:"Checked"
        ~default:(fun () -> CheckBox false)
        () in *)
    let _ =
      self#add_editable_string_column
        ~header:"Comment"
        ~shown_header:"Commentaire"
        ~italic:true
        ~default:(fun () -> String "[no comment]")
        () in
    let _ =
      self#add_string_column
        ~header:"File name"
        ~hidden:true
        () in
    let _ =
      self#add_string_column
        ~header:"Prefixed filesystem"
        ~hidden:true
        () in

    (* Make internal data structures: no more columns can be added now: *)
    self#create_store_and_view;
    
    (* Make the contextual menu: *)
    let get = raise_when_none in (* just a convenient alias *)
    self#set_contextual_menu_title "Filesystem history operations";
    self#add_menu_item
      "Exporter comme variante de machine"
      (fun selected_rowid_if_any ->
        (is_some selected_rowid_if_any) &&
        (let row_id = get selected_rowid_if_any in
        let type_ = item_to_string (self#get_row_item row_id "Type") in
        type_ = "machine"))
      (fun selected_rowid_if_any ->
        let row_id = get selected_rowid_if_any in
        self#export_as_machine_variant row_id);
    self#add_menu_item
      "Exporter comme la (seule) variante des routeurs"
      (fun selected_rowid_if_any ->
        (is_some selected_rowid_if_any) &&
        (let row_id = get selected_rowid_if_any in
        let type_ = item_to_string (self#get_row_item row_id "Type") in
        type_ = "router"))
      (fun selected_rowid_if_any ->
        let row_id = get selected_rowid_if_any in
        self#export_as_router_variant row_id);
(*     self#add_separator_menu_item; *)
    self#add_menu_item
      "Démarrer dans cet état"
      (fun selected_rowid_if_any ->
        (is_some selected_rowid_if_any) &&
        (let row_id = get selected_rowid_if_any in
        let name = item_to_string (self#get_row_item row_id "Name") in
        let can_startup, _ = get_startup_functions () in
        can_startup name))
      (fun selected_rowid_if_any ->
        let row_id = get selected_rowid_if_any in
        self#startup_in_state row_id);
(*     self#add_separator_menu_item; *)
    self#add_menu_item
      "Éliminer cet état"
      (fun selected_rowid_if_any ->
        (is_some selected_rowid_if_any) &&
        (let row_id = get selected_rowid_if_any in
        let name = item_to_string (self#get_row_item row_id "Name") in
        (self#number_of_states_with_name name) > 1))
      (fun selected_rowid_if_any ->
        let row_id = get selected_rowid_if_any in
        self#delete_state row_id);
end;;
  
(** The one and only states interface object, with my usual OCaml kludge
    enabling me to set it at runtime: *)
let the_states_interface =
  ref None;;
let get_states_interface () =
  match !the_states_interface with
    None ->
      failwith "the_state_interface has not been defined yet"
  | Some the_states_interface ->
      the_states_interface;;
let make_states_interface ~packing () =
  match !the_states_interface with
    None ->
      the_states_interface :=
        Some(new states_interface ~packing ())
  | Some the_states_interface ->
      failwith "the_state_interface has already been defined"

(** Note that the states directory {e must} be an absolute pathname and {e must} include
    a trailing slash *)
let set_states_directory states_directory =
  match ! the_states_directory with
    None -> begin
      print_string ("\n\n[[[[[Setting the states directory to "^states_directory^"]]]]]\n\n");
      the_states_directory := Some states_directory;
      (get_states_interface ())#set_file_name (states_directory^"/states-forest");
    end
  | _ ->
      failwith "the states directory was initialized twice!";;
let reset_states_directory () =
  match ! the_states_directory with
    None ->
      () (* do nothing *)
  | _ -> begin
      the_states_directory := None;
      (get_states_interface ())#reset_file_name;
    end

exception Cp_failed;;
(** Make a sparse copy of the given file, and return the name of the copy. Note that this
    also succeeds when the source file does not exist, as it's the case with 'fresh' states *)
let duplicate_file ?(is_path_full=false) path_name =
  let name_of_the_copy = make_temporary_file_name () in
  let states_directory = get_states_directory () in
  let command_line =
    if is_path_full then
      (Printf.sprintf "if [ -e %s ]; then cp --sparse=always %s %s%s; else true; fi"
         path_name
         path_name
         states_directory
         name_of_the_copy)
    else
      (Printf.sprintf "if [ -e %s%s ]; then cp --sparse=always %s%s %s%s; else true; fi"
         states_directory
         path_name
         states_directory
         path_name
         states_directory
         name_of_the_copy)  in
  print_string ("\n\nMaking a copy of a cow file: the command line is\n"^command_line^"\n\n");
  try
    (match Unix.system command_line with
      (Unix.WEXITED 0) ->
        name_of_the_copy
    | _ -> begin
        raise Cp_failed;
    end)
  with Cp_failed -> begin
    Simple_dialogs.error
      "This is a serious problem"
      "The disc is probably full. A disaster is about to happen..."
      ();
    print_string "To do: react in some reasonable way\n";
    flush_all ();
    failwith "cp failed";
  end
  | _ -> begin
      print_string "To do: this look harmless.\n";
      flush_all ();
      name_of_the_copy;
  end;;

(* ------------------------------------------------------------------------------- *)
let clear () =
  let states_interface = get_states_interface () in
  states_interface#clear

let load_states () =
  let states_interface = get_states_interface () in
  states_interface#load

let save_states () =
  let states_interface = get_states_interface () in
  states_interface#save

let get_forest () =
  let states_interface = get_states_interface () in
  states_interface#get_forest   

let add_row
    ~name
    ?parent (* this is an id, not an iter! *)
    ~comment
    ~icon
    ~toggle
    ~date
    ~scenario
    ~prefixed_filesystem
    ~file_name
    () =
  print_string "Adding a row to the filesystem history model... begin\n";
  let states_interface = get_states_interface () in
  let row =
    [ "Name", String name;
      "Comment", String comment;
      "Type", Icon icon;
      "Activation scenario", String scenario;
      "Timestamp", String date;
      "Prefixed filesystem", String prefixed_filesystem;
      "File name", String file_name ] in
  let result = 
    states_interface#add_row ?parent_row_id:parent row
  in
  result

let number_of_states_such_that
    ?forest:(forest=get_forest ())
    f =
  let linearized_forest = Forest.linearize forest in
  List.length (List.filter f linearized_forest);;

let number_of_states_with_name
    ?forest:(forest=get_forest ())
    name =
  number_of_states_such_that
   ~forest
   (fun row -> (lookup_alist "Name" row) = String name);;

let add_device name prefixed_filesystem variant icon =
  Printf.printf "FILESYSTEM HISTORY: adding the device %s which has variant %s\n" name variant; flush_all ();
  let states_interface = get_states_interface () in
  (* If we're using a non-clean variant then copy it so that it becomes the first cow
     file: *)
  let file_name =
    if variant = no_variant_text then
      make_fresh_state_file_name ()
    else
      duplicate_file
        ~is_path_full:true
        ((get_variants_path prefixed_filesystem) ^ "/" ^ variant) in
  let row_id =
    add_row
      ~name
      ~icon
(*       ~comment:(if variant = no_variant_text then "état initiale" else ("clean [variante \""^variant^"\"]")) *)
      ~comment:(prefixed_filesystem ^ (if variant = no_variant_text then "" else " : variante \""^variant^"\""))
      ~file_name
      ~prefixed_filesystem
      ~date:"-"
      ~scenario:"[no scenario]"
      ~toggle:false
      () in
  states_interface#highlight_row row_id;

let rename_device old_name new_name =
  let states_interface = get_states_interface () in
  let altered_forest =
    Forest.map
      (fun row -> if (lookup_alist "Name" row) = String old_name then
                    bind_or_replace_in_alist "Name" (String new_name) row
                  else
                    row)
      (get_forest ()) in
  states_interface#set_forest altered_forest;;

let remove_device_tree name =
  let states_interface = get_states_interface () in
  Printf.printf "Removing the device tree for %s\n" name;
  (* Remove cow's: *)
  let rows_to_remove = 
    Forest.nodes_such_that
      (fun complete_row ->
        (lookup_alist "Name" complete_row) = String name)
      (get_forest ()) in
  List.iter
    (fun row ->
      let cow_file_name = item_to_string (lookup_alist "File name" row) in
      (try Unix.unlink ((get_states_directory ()) ^ "/" ^ cow_file_name) with _ -> ()))
    rows_to_remove;
  let filtered_forest =
    Forest.filter
      (fun row -> not ((lookup_alist "Name" row) = String name))
      (get_forest ()) in
  states_interface#set_forest filtered_forest;;

let get_the_most_recent_state_with_name name =
  let states_interface = get_states_interface () in
  states_interface#get_the_most_recent_state_with_name name;;

let add_substate_of parent_file_name =
  let states_interface = get_states_interface () in
  let copied_file_name =
    duplicate_file parent_file_name in
  let complete_forest = states_interface#get_complete_forest in
  let row_to_copy_in_a_singleton_list =
    Forest.linearize
      (Forest.filter
         (fun row -> (lookup_alist "File name" row) = String parent_file_name)
         complete_forest) in
  assert((List.length row_to_copy_in_a_singleton_list) = 1);
  let complete_row_to_copy =
    List.hd row_to_copy_in_a_singleton_list in
  let parent_row_id_as_item =
    lookup_alist "_id" complete_row_to_copy in
  let parent_row_id =
    match parent_row_id_as_item with String id -> id | _ -> assert false in
  let siblings_no =
    List.length (states_interface#children_of parent_row_id) in
  let parent_row_name =
    item_to_string (lookup_alist "Name" complete_row_to_copy) in
  let row_to_copy =
    states_interface#remove_reserved_fields complete_row_to_copy in
  Forest.iter
    (fun row _ ->
      let a_row_id = lookup_alist "_id" row in
      let a_row_name = item_to_string (lookup_alist "Name" row) in
      (if parent_row_name = a_row_name then
        states_interface#unhighlight_row (item_to_string a_row_id));
      if a_row_id = parent_row_id_as_item then
        let new_row_id =
          add_row
            ~name:(item_to_string (lookup_alist "Name" row_to_copy))
            ~icon:(item_to_string (lookup_alist "Type" row_to_copy))
            ~comment:"[no comment]"
            ~file_name:copied_file_name
            ~toggle:false
            ~parent:parent_row_id
            ~date:(Timestamp.current_timestamp_as_string ())
            ~scenario:"[no scenario]"
            ~prefixed_filesystem:(item_to_string (lookup_alist "Prefixed filesystem" row_to_copy))
            () in
        states_interface#highlight_row new_row_id)
    complete_forest;
  (* Collapse the new row's parent iff the new row is its first child. This behavior
     gives the impression that trees 'are born' collapsed (collapsing a leaf has no
     effect on the children it doesn't yet have), and on the other hand it does not
     bother the user undoing his/her expansions: *)
  (if siblings_no = 0 then
    states_interface#collapse_row parent_row_id);
  copied_file_name;;

let get_the_most_recent_file_name_with_name name =
  match lookup_alist "File name" (get_the_most_recent_state_with_name name) with
    String file_name -> file_name
  | _ -> assert false;;

let add_state_for_device device_name =
  let most_recent_file_name =
    get_the_most_recent_file_name_with_name device_name in
  add_substate_of most_recent_file_name;;
