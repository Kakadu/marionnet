(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007, 2008  Luca Saiu
   Updated in 2010 by Jean-Vincent Loddo
   
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

(*open Treeview;;*)
open Row_item;;
open Gettext;;
module Assoc = ListExtra.Assoc;;

(** A function to be called for starting up a given device in a given state. This very ugly
    kludge is needed to avoid a cyclic depencency between mariokit and states_interface *)
module Startup_functions = Stateful_modules.Variable (struct
  type t = (string -> bool) * (string -> unit)
  let name = Some "startup_functions"
end)

(** Create a fresh filename, without making the file (empty cow files are not allowed) *)
let make_temporary_file_name () =
  Printf.sprintf
    "%i-%i-%i.cow"
    (Random.int 65536)
    (Random.int 65536)
    (Random.int 65536);;

let make_fresh_state_file_name () =
  make_temporary_file_name ();;

exception Cp_failed;;

(** Make a sparse copy of the given file, and return the name of the copy. Note that this
    also succeeds when the source file does not exist, as it's the case with 'fresh' states *)
let duplicate_file ?(is_path_full=false) ~states_directory path_name  =
  let name_of_the_copy = make_temporary_file_name () in
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
  (Log.printf "Making a copy of a cow file: the command line is: %s\n" command_line);
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
    Log.printf "To do: react in some reasonable way\n";
    failwith "cp failed";
  end
  | _ -> begin
      Log.printf "To do: this look harmless.\n";
      name_of_the_copy;
  end;;


(** Principal exported treeview type: *)
class t =
fun ~packing
    ~after_user_edit_callback
    () ->
object(self)
  inherit
    Treeview.treeview_with_a_Name_column
      ~packing
      ~hide_reserved_fields:true
      () as self_as_treeview

  method add_row_with
    ~name
    ?parent (* this is an id, not an iter! *)
    ~comment
    ~icon
    ~date
    ~scenario
    ~prefixed_filesystem
    ~file_name
    () =
    let row =
      [ "Name", String name;
	"Comment", String comment;
	"Type", Icon icon;
	"Activation scenario", String scenario;
	"Timestamp", String date;
	"Prefixed filesystem", String prefixed_filesystem;
	"File name", String file_name ]
    in
    self#add_row ?parent_row_id:parent row

  method add_device ~name ~prefixed_filesystem ?variant ?variant_realpath ~icon () =
  (* If we're using a non-clean variant then copy it so that it becomes
     the first cow file: *)
  let (file_name, variant_name, comment_suffix) =
    (match variant, variant_realpath with
     | None, _ -> (make_fresh_state_file_name (), "", "")
     | Some variant_name, Some variant_realpath ->
         let file_name =
           duplicate_file
             ~is_path_full:true
             ~states_directory:(Option.extract directory#get)
             variant_realpath
         in (file_name, variant_name, (Printf.sprintf " : variant \"%s\"" variant_name))
     | Some _, None -> assert false
     )
  in
  Log.printf "Filesystem history.add_device: adding the device %s with variant name=\"%s\"\n" name variant_name;
  let row_id =
    self#add_row_with ~name ~icon
      ~comment:(prefixed_filesystem ^ comment_suffix)
      ~file_name
      ~prefixed_filesystem
      ~date:"-"
      ~scenario:"[no scenario]"
      () in
  self#highlight_row row_id

  method remove_device_tree device_name =
  let root_id = self#unique_root_row_id_of_name device_name in
  let rows_to_remove = self#rows_of_name device_name in
  (* Remove cow files: *)
  (List.iter
    (fun row ->
      let cow_filename = item_to_string (Assoc.find "File name" row) in
      let cow_pathname = Filename.concat (Option.extract directory#get) cow_filename in
      (try Unix.unlink cow_pathname with _ -> ()))
    rows_to_remove
   );
   self#remove_subtree root_id;

  method add_substate_of parent_file_name =
  let copied_file_name =
    duplicate_file
      ~states_directory:(Option.extract directory#get)
      parent_file_name
  in
  let complete_row_to_copy =
    self#unique_complete_row_such_that
      (fun row -> (Assoc.find "File name" row) = String parent_file_name)
  in
  let parent_id   = self#id_of_complete_row complete_row_to_copy in
  let parent_name = self#name_of_row complete_row_to_copy in
  let sibling_no  = self#children_no parent_id in
  let row_to_copy = self#remove_reserved_fields complete_row_to_copy
  in
  Forest.iter
    (fun row _ ->
       let a_row_id = self#id_of_complete_row row in
       let a_row_name = self#name_of_row row in
       (if parent_name = a_row_name then self#unhighlight_row a_row_id);
       if a_row_id = parent_id then
         let new_row_id =
           self#add_row_with
             ~name:parent_name
             ~icon:(item_to_string (Assoc.find "Type" row_to_copy))
             ~comment:"[no comment]"
             ~file_name:copied_file_name
             ~parent:parent_id
             ~date:(Timestamp.current_timestamp_as_string ())
             ~scenario:"[no scenario]"
             ~prefixed_filesystem:(item_to_string (Assoc.find "Prefixed filesystem" row_to_copy))
             ()
          in
          self#highlight_row new_row_id)
    (self#get_complete_forest);
  (* Collapse the new row's parent iff the new row is its first child. This behavior
     gives the impression that trees 'are born' collapsed (collapsing a leaf has no
     effect on the children it doesn't yet have), and on the other hand it does not
     bother the user undoing his/her expansions: *)
  (if sibling_no = 0 then self#collapse_row parent_id);
  copied_file_name

  method add_state_for_device device_name =
    let most_recent_file_name =
      item_to_string
        (Assoc.find "File name" (self#get_the_most_recent_state_with_name device_name))
    in
    self#add_substate_of most_recent_file_name

  method startup_in_state row_id =
    let correct_date = item_to_string (self#get_row_item row_id "Timestamp") in
    let _cow_file_name = item_to_string (self#get_row_item row_id "File name") in
    let name = item_to_string (self#get_row_item row_id "Name") in
    self#set_row_item row_id "Timestamp" (String (Timestamp.current_timestamp_as_string ()));
    let _, startup_function = Startup_functions.extract () in
    let () = startup_function name in
    Task_runner.the_task_runner#schedule
      (fun () ->
        self#set_row_item row_id "Timestamp" (String correct_date));
    

  method delete_state row_id =
    let name = item_to_string (self#get_row_item row_id "Name") in
    let file_name = item_to_string (self#get_row_item row_id "File name") in
    (* Remove the full row: *)
    self#remove_row row_id;
    (* Remove the cow file: *)
    let path_name = Filename.concat (Option.extract directory#get) file_name in
    (try Unix.unlink path_name with _ -> ());
    let most_recent_row_for_name = self#get_the_most_recent_state_with_name name in
    let id_of_the_most_recent_row_for_name =
      Assoc.find "_id" most_recent_row_for_name
    in
    Forest.iter
      (fun a_row _ ->
        let an_id = Assoc.find "_id" a_row in
        let a_name = Assoc.find "Name" a_row in
        if a_name = String name then
          (if an_id = id_of_the_most_recent_row_for_name then
            self#highlight_row (item_to_string an_id)
          else
            self#unhighlight_row (item_to_string an_id)))
      (self#get_complete_forest);
    

  method get_the_most_recent_state_with_name name =
    let forest = self#get_complete_forest in
    let relevant_forest =
      Forest.filter
        (fun row -> ((Assoc.find "Name" row) = String name))
      forest in
    let relevant_states =
      Forest.linearize relevant_forest in (* the forest should be a tree *)
    Log.printf "Relevant states for %s are %i\n" name (List.length relevant_states);
    assert ((List.length relevant_states) > 0);
    let result =
    List.fold_left
      (fun maximum row ->
        let timestamp_maximum =
          item_to_string (Assoc.find "Timestamp" maximum) in
        let timestamp_row =
          item_to_string (Assoc.find "Timestamp" row) in
        if timestamp_maximum > timestamp_row then
          maximum
        else
          row)
      (List.hd relevant_states)
      ((*List.tl*) relevant_states) in
    result

  method number_of_states_such_that predicate =
    let linearized_complete_forest = Forest.linearize self#get_complete_forest in
    List.length (List.filter predicate linearized_complete_forest)

  method number_of_states_with_name name =
    self#number_of_states_such_that
      (fun complete_row ->
        (Assoc.find "Name" complete_row) = String name)

  method export_as_machine_variant row_id =
    self#export_as_variant ~router:false row_id

  method export_as_router_variant row_id =
    self#export_as_variant ~router:true row_id

  method private export_as_variant ~router row_id =
    let device_name = item_to_string (self#get_row_item row_id "Name") in
    let can_startup, _ = Startup_functions.extract () in
    (* We can only export the cow file if we are not running the device: *)
    if not (can_startup device_name) then
      Simple_dialogs.error
        (Printf.sprintf (f_ "The device %s is running") device_name)
        (s_ "You have to shut it down first.") (* TODO *)
        ()
    else
    let cow_name = item_to_string (self#get_row_item row_id "File name") in
    let variant_dir =
      (* For backward compatibility I can't change the treeview structure
         to store these informations once. On the contrary, I re-calculate
	 them at each export; *)
      let prefixed_filesystem = item_to_string (self#get_row_item row_id "Prefixed filesystem") in
      Disk.user_export_dirname_of_prefixed_filesystem prefixed_filesystem
    in
    (* Just show the dialog window, and bind a method which does all the real work to the
       'Ok' button. This continuation-based logic is the best we can do here, because we
       can't loop waiting for the user without giving control back to Gtk+: *)
    Simple_dialogs.ask_text_dialog
      ~title:(s_ "Choose the variant name")
      ~label:(s_ "Enter the new variant name; this name must begin with a letter and can contain letters, numbers, dashes and underscores.")
      ~constraint_predicate:
	  (fun s ->
	    (String.length s > 0) &&
	    (StrExtra.wellFormedName ~allow_dash:true s))
      ~invalid_text_message:(s_ "The name must begin with a letter and can contain letters, numbers, dashes and underscores.")
      ~enable_cancel:true
      ~ok_callback:(fun variant_name ->
	self#actually_export_as_variant
	  ~cow_name
	  ~variant_dir
	  ~variant_name ())
      ()


  method private actually_export_as_variant ~variant_dir ~cow_name ~variant_name () =
    (* Perform the actual copy: *)
    let cow_path = (Option.extract directory#get) in
    let new_variant_pathname = Filename.concat variant_dir variant_name in
    let command_line =
      Printf.sprintf
        "(mkdir -p '%s' && test -f '%s/%s' && cp --sparse=always '%s/%s' '%s')"
        variant_dir
        cow_path cow_name
        cow_path cow_name
        new_variant_pathname in
    try
      Log.system_or_fail command_line;
      Simple_dialogs.info
        (s_ "Success")
        ((s_ "The variant has been exported to the file") ^ " \"" ^ new_variant_pathname ^ "\".")
        ()
    with _ -> begin
      (* Remove any partial copy: *)
      UnixExtra.apply_ignoring_Unix_error Unix.unlink new_variant_pathname;
      Simple_dialogs.error
        (s_ "Error")
        (Printf.sprintf (f_ "\
The variant couldn't be exported to the file \"%s\".\n\n\
Many reasons are possible:\n - you don't have write access to this directory\n\
 - the machine was never started\n - you didn't select the machine disk but \n\
the machine itself (you should expand the tree).") new_variant_pathname)
        ()
      end

  initializer
    (* Make columns: *)
    let _ =
      self#add_icon_column
        ~header:"Type"
        ~shown_header:(s_ "Type")
        ~strings_and_pixbufs:[ "router", Initialization.Path.images^"treeview-icons/router.xpm";
                               "machine", Initialization.Path.images^"treeview-icons/machine.xpm"; ]
        () in
    let _ =
      self#add_string_column
        ~header:"Activation scenario"
        ~shown_header:(s_ "Activation scenario")
        ~default:(fun () -> String "[No scenario]")
        ~hidden:true
        ~italic:true
        () in
    let _ =
      self#add_string_column
        ~header:"Timestamp"
        ~shown_header:(s_ "Timestamp")
        ~default:(fun () -> String (Timestamp.current_timestamp_as_string ()))
        () in
    let _ =
      self#add_editable_string_column
        ~header:"Comment"
        ~shown_header:(s_ "Comment")
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
    self#set_contextual_menu_title "Filesystem history operations";
    self#add_menu_item
     (s_  "Export as machine variant")
      (fun selected_rowid_if_any ->
        (Option.to_bool selected_rowid_if_any) &&
        (let row_id = Option.extract selected_rowid_if_any in
        let type_ = item_to_string (self#get_row_item row_id "Type") in
        type_ = "machine"))
      (fun selected_rowid_if_any ->
        let row_id = Option.extract selected_rowid_if_any in
        self#export_as_machine_variant row_id);
    self#add_menu_item
      (s_ "Export as router variant")
      (fun selected_rowid_if_any ->
        (Option.to_bool selected_rowid_if_any) &&
        (let row_id = Option.extract selected_rowid_if_any in
        let type_ = item_to_string (self#get_row_item row_id "Type") in
        type_ = "router"))
      (fun selected_rowid_if_any ->
        let row_id = Option.extract selected_rowid_if_any in
        self#export_as_router_variant row_id);
(*     self#add_separator_menu_item; *)
    self#add_menu_item
      (s_ "Start in this state")
      (fun selected_rowid_if_any ->
        (Option.to_bool selected_rowid_if_any) &&
        (let row_id = Option.extract selected_rowid_if_any in
        let name = item_to_string (self#get_row_item row_id "Name") in
        let can_startup, _ = Startup_functions.extract () in
        can_startup name))
      (fun selected_rowid_if_any ->
        let row_id = Option.extract selected_rowid_if_any in
        self#startup_in_state row_id);
(*     self#add_separator_menu_item; *)
    self#add_menu_item
      (s_ "Delete this state")
      (fun selected_rowid_if_any ->
        (Option.to_bool selected_rowid_if_any) &&
        (let row_id = Option.extract selected_rowid_if_any in
        let name = item_to_string (self#get_row_item row_id "Name") in
        (self#number_of_states_with_name name) > 1))
      (fun selected_rowid_if_any ->
        let row_id = Option.extract selected_rowid_if_any in
        self#delete_state row_id);

     (* J.V. *)
      self#set_after_update_callback after_user_edit_callback;

end;;

class treeview = t
module The_unique_treeview = Stateful_modules.Variable (struct
  type t = treeview
  let name = Some "treeview_history"
  end)
let extract = The_unique_treeview.extract

let make ~packing ~after_user_edit_callback () =
  let result = new t ~packing ~after_user_edit_callback () in
  The_unique_treeview.set result;
  result

