(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007, 2008, 2009  Luca Saiu
   Copyright (C) 2010, 2011, 2012  Jean-Vincent Loddo
   Copyright (C) 2007, 2008, 2009, 2010, 2011, 2012  Université Paris 13

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

open Gettext;;
module Row_item = Treeview.Row_item ;;
module Row = Treeview.Row ;;

(** A function to be called for starting up a given device in a given state. This very ugly
    kludge is needed to avoid a cyclic depencency between mariokit and states_interface *)
module Startup_functions = Stateful_modules.Variable (struct
  type t = (string -> bool) * (string -> unit)
  let name = Some "startup_functions"
end)

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

  val comment_header = "Comment"
  method get_row_comment = self#get_String_field (comment_header)
  method set_row_comment = self#set_String_field (comment_header)

  val type_header = "Type"
  method get_row_type = self#get_Icon_field (type_header)
  method set_row_type = self#set_Icon_field (type_header)

  val activation_scenario_header = "Activation scenario"
  method get_row_activation_scenario = self#get_String_field (activation_scenario_header)
  method set_row_activation_scenario = self#set_String_field (activation_scenario_header)

  val timestamp_header = "Timestamp"
  method get_row_timestamp = self#get_String_field (timestamp_header)
  method set_row_timestamp = self#set_String_field (timestamp_header)

  val prefixed_filesystem_header = "Prefixed filesystem"
  method get_row_prefixed_filesystem = self#get_String_field (prefixed_filesystem_header)
  method set_row_prefixed_filesystem = self#set_String_field (prefixed_filesystem_header)

  val filename_header = "File name"
  method get_row_filename = self#get_String_field (filename_header)
  method set_row_filename = self#set_String_field (filename_header)

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
      [ name_header,    Row_item.String name;
	comment_header, Row_item.String comment;
	type_header,    Row_item.Icon icon;
	activation_scenario_header, Row_item.String scenario;
	timestamp_header, Row_item.String date;
	prefixed_filesystem_header, Row_item.String prefixed_filesystem;
	filename_header, Row_item.String file_name ]
    in
    self#add_row ?parent_row_id:parent row

  method add_device ~name ~prefixed_filesystem ?variant ~icon () =
    let states_directory = (Option.extract directory#get) in
    let file_name = Cow_files.make_temporary_cow_file_name ~states_directory () in
    let (variant_name, comment_suffix) =
      (match variant with
      | None -> ("", "")
      | Some variant_name -> (variant_name, (Printf.sprintf " : variant \"%s\"" variant_name))
      )
    in
    Log.printf2 "Treeview_history.t#add_device: adding the device %s with variant name=\"%s\"\n" name variant_name;
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
      let cow_filename = Row.String_field.get ~field:filename_header row in
      let cow_pathname = Filename.concat (Option.extract directory#get) cow_filename in
      (try Unix.unlink cow_pathname with _ -> ()))
    rows_to_remove
   );
   self#remove_subtree root_id;

  (* This method is useful to understand which source file has
     to be copied into the cow_file_name assigned to an UML device. *)
  method get_parent_cow_file_name ~(cow_file_name:string) () : string option =
    let cow_file_name = Filename.basename cow_file_name in
    let complete_row =
      self#unique_complete_row_such_that
        (fun row -> Row.String_field.eq ~field:filename_header ~value:cow_file_name row)
    in
    let row_id = self#id_of_complete_row complete_row in
    Option.map (self#get_row_filename) (self#parent_of row_id)

  method add_substate_of parent_file_name =
  let cow_file_name =
    Cow_files.make_temporary_cow_file_name
      ~states_directory:(Option.extract directory#get)
      ()
  in
  let complete_row_to_copy =
    self#unique_complete_row_such_that
      (fun row -> Row.String_field.eq ~field:filename_header ~value:parent_file_name row)
  in
  let parent_id   = self#id_of_complete_row complete_row_to_copy in
  let parent_name = Row.get_name (complete_row_to_copy) in
  let sibling_no  = self#children_no parent_id in
  let row_to_copy = self#remove_reserved_fields complete_row_to_copy
  in
  Forest.iter
    (fun row _ ->
       let row_id = self#id_of_complete_row row in
       let row_name = Row.get_name row in
       (if parent_name = row_name then self#unhighlight_row row_id);
       if row_id = parent_id then
         let new_row_id =
           self#add_row_with
             ~name:parent_name
             ~icon:(Row.Icon_field.get ~field:type_header row_to_copy)
             ~comment:"[no comment]"
             ~file_name:cow_file_name
             ~parent:parent_id
             ~date:(UnixExtra.date ~dot:" " ())
             ~scenario:"[no scenario]"
             ~prefixed_filesystem:(Row.String_field.get ~field:prefixed_filesystem_header row_to_copy)
             ()
          in
          self#highlight_row new_row_id)
    (self#get_complete_forest);
  (* Collapse the new row's parent iff the new row is its first child. This behavior
     gives the impression that trees 'are born' collapsed (collapsing a leaf has no
     effect on the children it doesn't yet have), and on the other hand it does not
     bother the user undoing his/her expansions: *)
  (if sibling_no = 0 then self#collapse_row parent_id);
  cow_file_name

  method add_state_for_device (device_name) (* "m1" *) =
    let most_recent_file_name =
      let row = self#get_the_most_recent_state_with_name (device_name) in
      Row.String_field.get ~field:filename_header row
    in
    self#add_substate_of most_recent_file_name

  method startup_in_state row_id =
    let correct_date   = self#get_row_timestamp row_id in
    let _cow_file_name = self#get_row_filename  row_id in
    let name           = self#get_row_name row_id in
    self#set_row_timestamp row_id (UnixExtra.date ~dot:" " ());
    let _, startup_function = Startup_functions.extract () in
    let () = startup_function name in
    Task_runner.the_task_runner#schedule
      (fun () -> self#set_row_timestamp row_id correct_date);


  method delete_state row_id =
    let name      = self#get_row_name row_id in
    let file_name = self#get_row_filename row_id in
    (* Remove the full row: *)
    self#remove_row row_id;
    (* Remove the cow file: *)
    let path_name = Filename.concat (Option.extract directory#get) file_name in
    (try Unix.unlink path_name with _ -> ());
    let most_recent_row_for_name = self#get_the_most_recent_state_with_name name in
    let id_of_the_most_recent_row_for_name =
      Row.get_id most_recent_row_for_name
    in
    Forest.iter
      (fun a_row _ ->
        let an_id = Row.get_id a_row in
        let a_name = Row.get_name a_row in
        if a_name = name then
          (if an_id = id_of_the_most_recent_row_for_name then
            self#highlight_row (an_id)
          else
            self#unhighlight_row (an_id)))
      (self#get_complete_forest);


  method delete_states_except_this row_id =
    let name    = self#get_row_name row_id in
    let row_ids = self#row_ids_of_name name in
    let root_id = self#unique_root_row_id_of_name name in
    let row_ids_to_remove = ListExtra.substract row_ids [root_id; row_id] in
    List.iter self#delete_state row_ids_to_remove


  method get_the_most_recent_state_with_name name =
    let forest = self#get_complete_forest in
    let relevant_forest = Forest.filter (Row.eq_name name) forest in
    let relevant_states =
      Forest.to_list relevant_forest
    in (* the forest should be a tree *)
    Log.printf2 "Relevant states for %s are %i\n" name (List.length relevant_states);
    assert ((List.length relevant_states) > 0);
    let result =
    List.fold_left
      (fun maximum row ->
         let timestamp_maximum = Row.String_field.get ~field:timestamp_header maximum in
         let timestamp_row = Row.String_field.get ~field:timestamp_header row in
         if timestamp_maximum > timestamp_row then maximum else row)
      (List.hd relevant_states)
      ((*List.tl*) relevant_states) in
    result

  method get_all_row_ids_except_root_and_the_most_recent_of_name name =
    let row_ids = self#row_ids_of_name name in
    let root_id = self#unique_root_row_id_of_name name in
    let most_recent_id =
      self#id_of_complete_row (self#get_the_most_recent_state_with_name name)
    in
    ListExtra.substract row_ids [root_id; most_recent_id]

  method get_all_row_ids_except_root_and_the_most_recent_ones =
    let names = self#get_name_list in
    List.concat (List.map (self#get_all_row_ids_except_root_and_the_most_recent_of_name) names)

  (* Returns the list of existing cow files except the most recent ones: *)
  method get_files_may_not_be_saved =
    if Global_options.Keep_all_snapshots_when_saving.extract () = true then [] else
    let row_ids = self#get_all_row_ids_except_root_and_the_most_recent_ones in
    let file_names = List.map (self#get_row_filename) row_ids in
    let file_exists cow_file_name =
      Cow_files.cow_file_exists
        ~states_directory:(Option.extract self#directory#get)
        ~cow_file_name ()
    in
    List.filter (file_exists) file_names

  (* Method redefinition. In this class we need to define a specific forest treatment
     that consists in saving only the most recent states: *)
  method save ?with_forest_treatment () =
    let relevant_forest_of forest =
      if Global_options.Keep_all_snapshots_when_saving.extract () = true then forest else
      let excluded_row_ids = self#get_all_row_ids_except_root_and_the_most_recent_ones in
      Forest.filter
        (fun row -> not (List.mem (self#id_of_complete_row row) excluded_row_ids))
      forest
    in
    let with_forest_treatment =
      match with_forest_treatment with
      | None   -> relevant_forest_of
      | Some f -> (fun x -> relevant_forest_of (f x)) (* compose: relevant_forest_of°f *)
    in
    self_as_treeview#save ~with_forest_treatment ()

  method remove_all_states_except_the_most_recent_of_name name =
    List.iter self#delete_state (self#get_all_row_ids_except_root_and_the_most_recent_of_name name)

  method remove_all_states_except_the_most_recent_ones =
    let names = self#get_name_list in
    List.iter (self#remove_all_states_except_the_most_recent_of_name) names

  method remove_all_states_of_name name =
    let row_ids = self#row_ids_of_name name in
    let root_id = self#unique_root_row_id_of_name name in
    let row_ids_to_remove = ListExtra.substract row_ids [root_id] in
    List.iter self#delete_state row_ids_to_remove

  method remove_all_states =
    let names = self#get_name_list in
    List.iter (self#remove_all_states_of_name) names

  method number_of_states_such_that predicate =
    let linearized_complete_forest = Forest.to_list self#get_complete_forest in
    List.length (List.filter predicate linearized_complete_forest)

  method number_of_states_with_name name =
    self#number_of_states_such_that (Row.eq_name name)

  method number_of_states =
    let linearized_complete_forest = Forest.to_list self#get_complete_forest in
    List.length linearized_complete_forest

  method export_as_machine_variant row_id =
    self#export_as_variant ~router:false row_id

  method export_as_router_variant row_id =
    self#export_as_variant ~router:true row_id

  method private export_as_variant ~router row_id =
    let device_name = self#get_row_name row_id in
    let can_startup, _ = Startup_functions.extract () in
    (* We can only export the cow file if we are not running the device: *)
    if not (can_startup device_name) then
      Simple_dialogs.error
        (Printf.sprintf (f_ "The device %s is running") device_name)
        (s_ "You have to shut it down first.") (* TODO *)
        ()
    else
    let cow_name = self#get_row_filename row_id in
    let variant_dir =
      (* For backward compatibility I can't change the treeview structure
         to store these informations once. On the contrary, I re-calculate
	 them at each export; *)
      let prefixed_filesystem = self#get_row_prefixed_filesystem (row_id) in
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
	    (StrExtra.Class.identifierp ~allow_dash:() s))
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
        ~header:type_header
        ~shown_header:(s_ "Type")
        ~strings_and_pixbufs:[ "router", Initialization.Path.images^"treeview-icons/router.xpm";
                               "machine", Initialization.Path.images^"treeview-icons/machine.xpm"; ]
        () in
    let _ =
      self#add_string_column
        ~header:activation_scenario_header
        ~shown_header:(s_ "Activation scenario")
        ~default:(fun () -> Row_item.String "[No scenario]")
        ~hidden:true
        ~italic:true
        () in
    let _ =
      self#add_string_column
        ~header:timestamp_header
        ~shown_header:(s_ "Timestamp")
        ~default:(fun () -> Row_item.String (UnixExtra.date ~dot:" " ()))
        () in
    let _ =
      self#add_editable_string_column
        ~header:comment_header
        ~shown_header:(s_ "Comment")
        ~italic:true
        ~default:(fun () -> Row_item.String "[no comment]")
        () in
    let _ =
      self#add_string_column
        ~header:filename_header
        ~hidden:true
        () in
    let _ =
      self#add_string_column
        ~header:prefixed_filesystem_header
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
        let type_ = self#get_row_type row_id in
        type_ = "machine"))
      (fun selected_rowid_if_any ->
        let row_id = Option.extract selected_rowid_if_any in
        self#export_as_machine_variant row_id);

    self#add_menu_item
      (s_ "Export as router variant")
      (fun selected_rowid_if_any ->
        (Option.to_bool selected_rowid_if_any) &&
        (let row_id = Option.extract selected_rowid_if_any in
        let type_ = self#get_row_type row_id in
        type_ = "router"))
      (fun selected_rowid_if_any ->
        let row_id = Option.extract selected_rowid_if_any in
        self#export_as_router_variant row_id);

 (* self#add_separator_menu_item; *)

    self#add_menu_item
      (s_ "Start in this state")
      (fun selected_rowid_if_any ->
        (Option.to_bool selected_rowid_if_any) &&
        (let row_id = Option.extract selected_rowid_if_any in
        let name = self#get_row_name row_id in
        let can_startup, _ = Startup_functions.extract () in
        can_startup name))
      (fun selected_rowid_if_any ->
        let row_id = Option.extract selected_rowid_if_any in
        self#startup_in_state row_id);

    self#add_separator_menu_item;

    let number_of_states_gt_1 =
      fun selected_rowid_if_any ->
	(self#number_of_states > 1)
    in
    let number_of_states_with_name_gt_1 =
      fun selected_rowid_if_any ->
	(Option.to_bool selected_rowid_if_any) &&
	(let row_id = Option.extract selected_rowid_if_any in
        let name = self#get_row_name row_id in
	(self#number_of_states_with_name name) > 1)
    in

    self#add_menu_item
      (s_ "Delete this state")
      number_of_states_with_name_gt_1
      (fun selected_rowid_if_any ->
        let row_id = Option.extract selected_rowid_if_any in
        self#delete_state row_id);

    self#add_menu_item
      (s_ "Delete all states except this")
      number_of_states_with_name_gt_1
      (fun selected_rowid_if_any ->
        let row_id = Option.extract selected_rowid_if_any in
        self#delete_states_except_this row_id);

    self#add_separator_menu_item;

    self#add_menu_item
      (s_ "Delete all states of this machine except the most recent")
      number_of_states_with_name_gt_1
      (fun selected_rowid_if_any ->
        let row_id = Option.extract selected_rowid_if_any in
        let name = self#get_row_name row_id in
        self#remove_all_states_except_the_most_recent_of_name name);

    self#add_menu_item
      (s_ "Delete all states of this machine")
      number_of_states_with_name_gt_1
      (fun selected_rowid_if_any ->
        let row_id = Option.extract selected_rowid_if_any in
        let name = self#get_row_name row_id in
        self#remove_all_states_of_name name);

    self#add_separator_menu_item;

    self#add_menu_item
      (s_ "Delete all states except the most recent ones")
      number_of_states_gt_1
      (fun selected_rowid_if_any ->
        self#remove_all_states_except_the_most_recent_ones);

    self#add_menu_item
      (s_ "Delete all states")
      number_of_states_gt_1
      (fun selected_rowid_if_any ->
        self#remove_all_states);

     (* J.V. *)
      self#set_after_update_callback after_user_edit_callback;

end;;

class treeview = t
module The_unique_treeview = Stateful_modules.Variable (struct
  type t = treeview
  let name = Some "treeview_history"
  end)
let extract = The_unique_treeview.extract

let make ~(window:GWindow.window) ~(hbox:GPack.box) ~after_user_edit_callback () =
  let result = new t ~packing:(hbox#add) ~after_user_edit_callback () in
  let () = Treeview.add_expand_and_collapse_button ~window ~hbox (result:>Treeview.t) in
  The_unique_treeview.set result;
  result

