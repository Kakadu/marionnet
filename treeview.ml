(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007, 2008, 2009  Luca Saiu

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


(* To do: this could be moved to WIDGET/ *)
open PreludeExtra.Prelude;; (* We want synchronous terminal output *)
(* open GTree;; *)
open Forest;;
open Row_item;;

let highlight_foreground_color = "White";;

(* To do: move this into identifier. It's good and useful. *)
class identifier_generator =
let initial_next_identifier = 0 in
object(self)
  val next_identifier = ref initial_next_identifier
  
  method reset_identifier =
    self#set_next_identifier initial_next_identifier
  method get_next_identifier =
    !next_identifier
  method set_next_identifier the_next_identifier =
    next_identifier := the_next_identifier
  method make_identifier =
    let result = !next_identifier in
    next_identifier := result + 1;
    result
end;;

type column_type =
  | StringColumnType
  | CheckBoxColumnType
  | IconColumnType;;

(** A row is simply a list of non-conflicting pairs <column_name, row items>. We
    implement it in this way to make it easy to marshal, and realatively easy
    to manipulate at runtime. Of course by using lists instead of tuples we're
    avoiding potentially helpful type checks here, but here flexibility is more
    important *)
type row = (string * row_item) list;;

let lookup_alist name alist =
  let filtered_alist = List.filter (fun (name_, value) -> name = name_) alist in
  let filtered_alist_length = List.length filtered_alist in
  if filtered_alist_length = 1 then
    snd (List.hd filtered_alist)
  else if filtered_alist_length = 0 then
    failwith (Printf.sprintf "lookup_alist: no match for %s" name)
  else
    failwith ("lookup_alist: "^(string_of_int filtered_alist_length)^" matches");;

let is_bound_in_alist name alist =
  let filtered_alist = List.filter (fun (name_, value) -> name = name_) alist in
  let filtered_alist_length = List.length filtered_alist in
  if filtered_alist_length = 1 then
    true
  else if filtered_alist_length = 0 then
    false
  else
    failwith ("is_bound_in_alist: "^(string_of_int filtered_alist_length)^" matches");;

(** This succeeds also when name is not bound in alist *)
(*let remove_from_alist name alist =
  let alist_length = List.length alist in
  let filtered_alist = List.filter (fun (name_, value) -> not (name = name_)) alist in
  let filtered_alist_length = List.length filtered_alist in
  let removed_elements_no = alist_length - filtered_alist_length in
  if removed_elements_no <= 1 then
    filtered_alist
  else
    failwith ("remove_from_alist: "^name^" was bound "^(string_of_int removed_elements_no)^"times");;*)
let rec remove_from_alist name alist =
  match alist with
  | [] ->
      []
  | (a_name, a_value) :: rest when a_name = name ->
      rest
  | pair :: rest ->
      pair :: (remove_from_alist name rest);;

(** This succeeds also when name is not bound in alist *)
let bind_or_replace_in_alist name value alist =
  let alist_length = List.length alist in
  let filtered_alist = List.filter (fun (name_, value) -> not (name = name_)) alist in
  let filtered_alist_length = List.length filtered_alist in
  let removed_elements_no = alist_length - filtered_alist_length in
  if removed_elements_no <= 1 then
    (name, value) :: filtered_alist
  else
    failwith ("bind_or_replace_in_alist: "^name^" was bound "^(string_of_int removed_elements_no)^"times");;

(** {b To do: Jean, do you know of any general-purpose facility for deep-copying? (here
    copying the cons structure is enough, but this may not always be the case)} *)
let rec clone_alist alist =
  List.rev (List.rev alist);;

type row_id = int;;

class virtual column =
let last_used_id = ref 0 in
fun ~treeview
    ~(hidden:bool)
    ~(reserved:bool)
    ?(default:(unit -> row_item) option)
    ~(header:string)
    ?(shown_header=header)
    ?constraint_predicate:(constraint_predicate = (fun (_ : row_item) -> true))
    () ->
let () = last_used_id := !last_used_id + 1 in
let _ =
  if reserved then
    match default with
      Some _ ->
        ()
    | None ->
        failwith ("The column "^ header ^" is reserved but has no default")
  else
    () in
object(self)
  val id = !last_used_id
  method id = id

  method header = header
  method shown_header = shown_header
  method hidden = hidden
  method is_reserved =
    reserved
  method has_default =
    match default with
      Some _ -> true
    | None -> false
  method default =
    match default with
      Some default -> default
    | None -> failwith (self#header ^ " has no default value, hence it must be specified")
  method virtual can_contain : row_item -> bool
  method virtual gtree_column : 'a . 'a GTree.column
(*   method virtual gtree_column : 'a. 'a GTree.column *)
  method virtual append_to_view : GTree.view -> unit
  method virtual get : string -> row_item
  method virtual set : ?initialize:bool -> ?row_iter:Gtk.tree_iter -> ?ignore_constraints:bool -> string -> row_item -> unit

  val gtree_view_column = ref None

  method set_gtree_view_column (the_gtree_view_column : GTree.view_column) =
    gtree_view_column := Some the_gtree_view_column

  method gtree_view_column =
    match !gtree_view_column with
      None ->
        failwith "set_gtree_view_column has not been called"
    | Some the_gtree_view_column ->
        the_gtree_view_column
end;;

class string_column =
fun ~treeview
    ?hidden:(hidden=false)
    ?reserved:(reserved=false)
    ?default
    ?italic:(italic=false)
    ?bold:(bold=false)
    ~(header:string)
    ?(shown_header)
    ?constraint_predicate:(constraint_predicate = (fun (_ : row_item) -> true))
    () -> object(self)
  inherit column ~hidden ~reserved ?default ~treeview ~header ?shown_header ~constraint_predicate () as super
          
  method can_contain x =
    constraint_predicate x
  val gtree_column : ('a GTree.column) =
    (treeview#gtree_column_list :> GTree.column_list)#add Gobject.Data.string
  method gtree_column (* Ugly kludge which I need to overcome countervariance restrictions *) =
    Obj.magic gtree_column

  method append_to_view (view : GTree.view) =
    let column = (self :> column) in
    let renderer =
      GTree.cell_renderer_text
        [ `EDITABLE false;
          `FOREGROUND highlight_foreground_color;
          `STYLE (if italic then `ITALIC else `NORMAL);
          `WEIGHT (if bold then `BOLD else `NORMAL); ] in
    let highlight_column = (treeview#get_column "_highlight" :> column) in
    let highlight_color_column = (treeview#get_column "_highlight-color" :> column) in
    let col = GTree.view_column
        ~title:self#shown_header
        ~renderer:(renderer, [ "text", column#gtree_column;
                               "foreground_set", (highlight_column#gtree_column);
                               "cell_background_set", (highlight_column#gtree_column);
                               "cell_background", (highlight_color_column#gtree_column); ])
        () in
    let _ = view#append_column col in
    col#set_resizable true;
    self#set_gtree_view_column col
      
  method get row_id =
    let tree_iter = treeview#id_to_iter row_id in
    let store = (treeview#store :> GTree.tree_store) in
    String(store#get ~row:tree_iter ~column:self#gtree_column)

  method set ?(initialize=false) ?row_iter ?(ignore_constraints=false) row_id item =
    (if not (self#header = "_id") then
       (if not initialize then begin
          let current_row =
            treeview#get_complete_row row_id in
          let new_row = bind_or_replace_in_alist self#header item current_row in
            (if not ignore_constraints then
               treeview#check_constraints new_row);
            (* Ok, if we arrived here then no constraint is violated by the update. *)
            treeview#set_row row_id new_row;
        end));
    let tree_iter =
      match row_iter with
        | None -> treeview#id_to_iter row_id
        | Some row_iter -> row_iter in
    let store = (treeview#store :> GTree.tree_store) in
    match item with
      String s ->
        store#set
          ~row:tree_iter
          ~column:self#gtree_column
          s
    | _ ->
        failwith (Printf.sprintf "set: wrong datum type for string column %s" self#header)
end;;

class editable_string_column =
fun ~treeview
    ?hidden:(hidden=false)
    ?reserved:(reserved=false)
    ?default
    ~(header:string)
    ?(shown_header)
    ?constraint_predicate:(constraint_predicate = (fun (_ : row_item) -> true))
    ?italic:(italic=false)
    ?bold:(bold=false)
    () -> object(self)
  inherit string_column ~hidden ~reserved ?default ~treeview ~header ?shown_header
                        ~constraint_predicate ~italic ~bold () as super

  method can_contain x =
    constraint_predicate x

  method append_to_view (view : GTree.view) =
    let column = (self :> column) in
    let highlight_column = treeview#get_column "_highlight" in
    let highlight_color_column = treeview#get_column "_highlight-color" in
    let renderer =
      GTree.cell_renderer_text
        [ `EDITABLE true;
          `FOREGROUND highlight_foreground_color;
          `STYLE (if italic then `ITALIC else `NORMAL);
          `WEIGHT (if bold then `BOLD else `NORMAL); ] in
    let col = GTree.view_column
        ~title:self#shown_header
        ~renderer:(renderer, [ "text", column#gtree_column;
                               "foreground_set", (highlight_column#gtree_column);
                               "cell_background_set", (highlight_column#gtree_column);
                               "cell_background", (highlight_color_column#gtree_column); ])
        () in
    let _ = view#append_column col in
    ignore(renderer#connect#edited ~callback:(fun path new_content -> self#on_edit path new_content));
    col#set_resizable true;
    self#set_gtree_view_column col


  val before_edit_commit_callback = ref (fun _ _ _ -> ())
  val after_edit_commit_callback = ref (fun _ _ _ -> ())

  method private on_edit path new_content =
    let id = treeview#path_to_id path in
    let old_content =
      match self#get id with
        String s -> s
      | _ -> assert false in
    (try
      (!before_edit_commit_callback) id old_content new_content;
      self#set id (String new_content);
      (!after_edit_commit_callback) id old_content new_content;
      treeview#run_after_update_callback id;
    with e -> begin
      Log.printf
        "on_edit: a callback raised an exception (%s), or a constraint was violated.\n"
        (Printexc.to_string e);
      flush_all ();
    end);
    treeview#save;

  (** Bind a callback to be called just before an edit is committed to data structures.
      The callback parameters are the row id, the old and the new content. If the callback
      throws an exception then no modification is committed, and the after_edit_commit
      callback is not called. *)
  method set_before_edit_commit_callback (callback : string -> string -> string -> unit) =
    before_edit_commit_callback := callback

  (** Bind a callback to be called just *after* an edit is committed to data structures.
      The callback parameters are the row id, the old and the new content. *)
  method set_after_edit_commit_callback (callback : string -> string -> string -> unit) =
    after_edit_commit_callback := callback
end;;

class checkbox_column =
fun ~treeview
    ?hidden:(hidden=false)
    ?reserved:(reserved=false)
    ?default
    ~(header:string)
    ?(shown_header)
    ?constraint_predicate:(constraint_predicate = (fun (_ : row_item) -> true))
    () -> object(self)
  inherit column ~hidden ~reserved ?default ~treeview ~header ?shown_header
                 ~constraint_predicate () as super

  method append_to_view (view : GTree.view) =
    let highlight_column = treeview#get_column "_highlight" in
    let highlight_color_column = treeview#get_column "_highlight-color" in
    let renderer =
      GTree.cell_renderer_toggle
        [ `ACTIVATABLE true;
          `RADIO false; ] in
    let col = GTree.view_column
        ~title:self#shown_header
        ~renderer:(renderer, [ "active", self#gtree_column;
                               "cell_background_set", (highlight_column#gtree_column);
                               "cell_background", (highlight_color_column#gtree_column); ])
        () in
    let _ = renderer#connect#toggled ~callback:(fun path -> self#on_toggle path) in
    let _ = view#append_column col in
    col#set_resizable true;
    self#set_gtree_view_column col


  method get row_id =
    let tree_iter = treeview#id_to_iter row_id in
    let store = (treeview#store :> GTree.tree_store) in
    CheckBox(store#get ~row:tree_iter ~column:self#gtree_column)

  method set ?(initialize=false) ?row_iter ?(ignore_constraints=false) row_id (item : row_item) =
    (if not initialize then begin
       let current_row = treeview#get_complete_row row_id in
       let new_row = bind_or_replace_in_alist self#header item current_row in
         (if not ignore_constraints then
            treeview#check_constraints new_row);
         (* Ok, if we arrived here then no constraint is violated by the update. *)
         treeview#set_row row_id new_row;
    end);
    let tree_iter =
      match row_iter with
        | None -> treeview#id_to_iter row_id
        | Some row_iter -> row_iter in
    let store = (treeview#store :> GTree.tree_store) in
    match item with
      CheckBox value ->
        store#set
          ~row:tree_iter
          ~column:self#gtree_column
          value
    | _ ->
        failwith (Printf.sprintf "set: wrong datum type for checkbox column %s" self#header)

  method can_contain x =
    constraint_predicate x
  val gtree_column =
    (treeview#gtree_column_list :> GTree.column_list)#add Gobject.Data.boolean
  method gtree_column (* Ugly kludge which I need to overcome countervariance restrictions *) =
    Obj.magic gtree_column

  val before_toggle_commit_callback = ref (fun _ _ _ -> ())
  val after_toggle_commit_callback = ref (fun _ _ _ -> ())

  method on_toggle path =
    let id = treeview#path_to_id path in
    let old_content =
      match self#get id with
        CheckBox value -> value
      | _ -> assert false in
    let new_content = not old_content in
    (try
      (!before_toggle_commit_callback) id old_content new_content;
      self#set id (CheckBox new_content);
      (!after_toggle_commit_callback) id old_content new_content;
      treeview#run_after_update_callback id;
    with _ -> begin
      Log.print_string "on_toggle: a callback raised an exception, or a constraint was violated.\n";
      flush_all ();
    end);
    treeview#save;

  (** Bind a callback to be called just before an toggle is committed to data structures.
      The callback parameters are the row id, the old and the new value. If the callback
      throws an exception then no modification is committed, and the after_toggle_commit
      callback is not called. *)
  method set_before_toggle_commit_callback (callback : string -> bool -> bool -> unit) =
    before_toggle_commit_callback := callback

  (** Bind a callback to be called just *after* an toggle is committed to data structures.
      The callback parameters are the row id, the old and the new value. *)
  method set_after_toggle_commit_callback (callback : string -> bool -> bool -> unit) =
    after_toggle_commit_callback := callback
end;;

class icon_column =
fun ~treeview
    ?hidden:(hidden=false)
    ?reserved:(reserved=false)
    ?default
    ~(header:string)
    ?(shown_header)
    ~(strings_and_pixbufs:(string * (* To do: gdkpixbuf *) string) list)
    () ->
let strings_and_pixbufs =
  List.map
    (fun (name, pixbuf_pathname) ->
(*       Log.printf "Loading icon %s from file %s\n" name pixbuf_pathname; *)
      name, (GdkPixbuf.from_file pixbuf_pathname))
    strings_and_pixbufs in
object(self)
  inherit column ~hidden ~reserved ?default ~treeview ~header ?shown_header () as super

  method private lookup predicate =
    let singleton = List.filter predicate strings_and_pixbufs in
    if not ((List.length singleton) = 1) then begin
      Log.printf "ERROR: icon name lookup failed: found %i results instead of 1\n" (List.length singleton);
      List.iter
        (fun (name, pixbuf) ->
          Log.printf
            "(predicate is %s for %s)\n"
            (if predicate (name, pixbuf) then "true" else "false")
            name)
        strings_and_pixbufs;
      failwith "Icon lookup failed"
    end else
      List.hd singleton

  method private lookup_by_string string =
    self#lookup (fun (string_, _) -> string_ = string)
  method private lookup_by_pixbuf pixbuf =
    self#lookup (fun (_, pixbuf_) -> pixbuf_ = pixbuf)
  method can_contain x =
    (* If lookup_by_string doesn't fail then x is safe: *)
    match x with
      (Icon icon) ->
        (try
          ignore (self#lookup_by_string icon);
          true;
        with _ ->
          false)
    | _ ->
        false

  val gtree_column =
    (treeview#gtree_column_list :> GTree.column_list)#add Gobject.Data.string
  method gtree_column (* Ugly kludge which I need to overcome countervariance restrictions *) =
    Obj.magic gtree_column

  method append_to_view (view : GTree.view) =
    let highlight_column = treeview#get_column "_highlight" in
    let highlight_color_column = treeview#get_column "_highlight-color" in
    let icon_cell_data_function =
      (fun renderer (model:GTree.model) iter ->
        let icon_as_string = model#get ~row:iter ~column:self#gtree_column in
        renderer#set_properties
         [ `PIXBUF
             (let (_, result) = self#lookup_by_string icon_as_string in
             result);
           `MODE `ACTIVATABLE ]) in
    let icon_renderer =
      GTree.cell_renderer_pixbuf [ (* `CELL_BACKGROUND highlight_background_color; *) ] in
    
(*  ~callback:(fun path new_content -> self#on_edit path new_content) *)
    let col = GTree.view_column
        ~title:self#shown_header
        ~renderer:(icon_renderer, [ "cell_background_set", (highlight_column#gtree_column);
                                    "cell_background", (highlight_color_column#gtree_column);])
        () in
    col#set_cell_data_func icon_renderer (icon_cell_data_function icon_renderer);
    ignore (view#append_column col);
    col#set_resizable true;
    self#set_gtree_view_column col


  method get row_id =
    let tree_iter = treeview#id_to_iter row_id in
    let store = (treeview#store :> GTree.tree_store) in
    Icon(store#get ~row:tree_iter ~column:self#gtree_column)

  method set ?(initialize=false) ?row_iter ?(ignore_constraints=false) row_id (item : row_item) =
    (if not initialize then begin
       let current_row = treeview#get_complete_row row_id in
       let new_row = bind_or_replace_in_alist self#header item current_row in
         (if not ignore_constraints then
            treeview#check_constraints new_row);
         (* Ok, if we arrived here then no constraint is violated by the update. *)
         treeview#set_row row_id new_row;
    end);
    let tree_iter =
      match row_iter with
        | None -> treeview#id_to_iter row_id
        | Some row_iter -> row_iter in
    let store = (treeview#store :> GTree.tree_store) in
    match item with
      Icon name ->
        store#set
          ~row:tree_iter
          ~column:self#gtree_column
          name
    | _ ->
        failwith (Printf.sprintf "set: wrong datum type for icon column %s" self#header)
end;;

let rec zip xs ys =
  match xs, ys with
    [], [] -> []
  | (x::xs),(y::ys) -> (x,y)::(zip xs ys)
  | _ -> failwith "zip: xs and ys have different lengths";;

exception RowConstraintViolated of (* constraint name*)string;;
exception ColumnConstraintViolated of (* column header *)string;;

class treeview = fun
  ~packing
  ?hide_reserved_fields:(hide_reserved_fields=true)
  () ->
let gtree_column_list = new GTree.column_list in
let vbox =
  GPack.box `VERTICAL ~homogeneous:false ~packing ~spacing:0 () in
let hbox =
  GPack.box
    `HORIZONTAL
    ~homogeneous:false
    ~packing:(vbox#pack ~expand:true ~padding:0)
    ~spacing:0
    () in
let view =
  GTree.view
    ~packing:(hbox#pack ~expand:true ~padding:0)
    ~reorderable:false (* Drag 'n drop for lines would be very cool, but here we need *)
                       (* to keep our internal forest data structure consistent with the UI *)
    ~enable_search:false 
    ~headers_visible:true
    ~headers_clickable:true
    ~rules_hint:true
    () in
let _ =
  GRange.scrollbar
    `VERTICAL 
    ~adjustment:view#vadjustment
    ~packing:(hbox#pack ~expand:false ~padding:0)
    () in
let _ =
  GRange.scrollbar
    `HORIZONTAL 
    ~adjustment:view#hadjustment
    ~packing:(vbox#pack ~expand:false ~padding:0)
    () in
object(self)
  inherit identifier_generator 

  method gtree_column_list : GTree.column_list = gtree_column_list

  val tree_store = ref None

  val id_forest =
    ref Empty

  val get_column =
    Hashtbl.create 100

  method get_column header =
    try
      ((Hashtbl.find get_column header) :> column)
    with e -> begin
      Log.printf "treeview: get_column: failed in looking for column \"%s\" (%s)\n" header (Printexc.to_string e); flush_all ();
      raise e; (* re-raise *)
    end

  method is_column_reserved header =
    let column = self#get_column header in
    column#is_reserved

  val id_to_row =
    Hashtbl.create 1000

  val file_name =
    ref None

  val columns : column list ref =
    ref []

  val id_column : column option ref =
    ref None

  val after_update_callback = ref (fun _ -> ())
  
  method set_after_update_callback f =
    after_update_callback := f

  method run_after_update_callback row_id =
    !after_update_callback row_id

  val row_constraints = ref []
  method add_row_constraint
      ?name:(name="<unnamed row constraing>")
      row_constraint =
    row_constraints := (name, row_constraint) :: !row_constraints

  val expanded_row_ids : (string, unit) Hashtbl.t = Hashtbl.create 1000;

  method private row_constraints = !row_constraints

  method check_constraints complete_row =
    List.iter
      (fun (name, row_constraint) ->
        if not (row_constraint complete_row) then begin
          Simple_dialogs.error
            "Invalid value: row constraint violated"
            (Printf.sprintf
               "The value you have chosen for a treeview element violates the row constraint \"%s\"."
               name)
            ();
          raise (RowConstraintViolated name)
        end)
      self#row_constraints;
    List.iter
      (fun (header, value) ->
        let column = self#get_column header in
        if not (column#can_contain value) then begin
          Simple_dialogs.error
            "Invalid column value"
            (Printf.sprintf
               "The value you have chosen for an element of the column \"%s\" is invalid."
               header)
            ();
          raise (ColumnConstraintViolated header)
        end)
      complete_row

  method columns =
    !columns

  val double_click_on_row_callback = ref (fun _ -> ())
  val collapse_row_callback = ref (fun _ -> ())
  val expand_row_callback = ref (fun _ -> ())

  method set_double_click_on_row_callback callback =
    double_click_on_row_callback := callback
  method set_collapse_row_callback callback =
    collapse_row_callback := callback
  method set_expand_row_callback callback =
    expand_row_callback := callback

  (** This returns the just-created column *)
  method private add_column (column : column) =
    columns := !columns @ [ column ];
    Hashtbl.add get_column column#header column;
    column

  method private on_row_activation path column =
    let id = self#path_to_id path in
    Log.printf "A row was double-clicked. Its id is %s\n" id; flush_all ();
    !double_click_on_row_callback id

  method private on_row_collapse iter _ =
    let id = self#iter_to_id iter in
    Hashtbl.remove expanded_row_ids id;
(*     Log.printf "A row was collapsed. Its id is %s\n" id; flush_all (); *)
    !collapse_row_callback id

  method private on_row_expand iter _ =
    let id = self#iter_to_id iter in
    Hashtbl.add expanded_row_ids id ();
(*     Log.printf "A row was expanded. Its id is %s\n" id; flush_all (); *)
    !expand_row_callback id

  method unselect =
    view#selection#unselect_all ()

  method select_row row_id = 
    view#selection#select_path (self#id_to_path row_id)

  method selected_row_id =
    match view#selection#get_selected_rows with
      [] -> None
    | path :: [] -> (Some (self#path_to_id path))
    | _ -> assert false

  method selected_row =
    match self#selected_row_id with
      None -> None
    | Some id -> Some (self#get_row id)

  val menu_items = ref []

  val contextual_menu_title = ref "Treeview commands"
  method set_contextual_menu_title title =
    contextual_menu_title := title

  method add_menu_item label predicate callback =
    menu_items := !menu_items @ [ Some(label, predicate, callback) ]

  method add_separator_menu_item =
    menu_items := !menu_items @ [ None ]

  method private show_contextual_menu event =
    (* Update the selection to be just the pointed row, if any: *)
    let x = int_of_float (GdkEvent.Button.x event) in
    let y = int_of_float (GdkEvent.Button.y event) in
    let selected_row_id =
      (match view#get_path_at_pos ~x ~y with
        Some (path, _, _, _) ->
          let id = self#path_to_id path in
          self#select_row id;
          Some id
      | None ->
          self#unselect;
          None) in
    Log.printf "Showing the contextual menu\n"; flush_all ();
    let menu = GMenu.menu () in
(*
    let _ = GMenu.menu_item ~label:!contextual_menu_title ~packing:menu#append () in
    let _ = GMenu.separator_item ~packing:menu#append () in
    let _ = GMenu.separator_item ~packing:menu#append () in *)
    List.iter
      (fun menu_item ->
         match menu_item with
           Some(label, predicate, callback) ->
             if predicate selected_row_id then
               let menu_item = GMenu.menu_item ~label ~packing:menu#append () in
             ignore (menu_item#connect#activate
                       ~callback:(fun () -> callback selected_row_id))
         | None ->
           ignore (GMenu.separator_item ~packing:menu#append ()))
      !menu_items;
    menu#popup ~button:(GdkEvent.Button.button event) ~time:(GdkEvent.Button.time event)

  method create_store_and_view =
    let the_tree_store = GTree.tree_store gtree_column_list in
    tree_store := Some the_tree_store;
    List.iter
      (fun column ->
        if not column#hidden then
          column#append_to_view view)
      self#columns;
    ignore (view#connect#row_activated ~callback:self#on_row_activation);
    ignore (view#connect#row_collapsed ~callback:self#on_row_collapse);
    ignore (view#connect#row_expanded ~callback:self#on_row_expand);
    ignore (view#event#connect#button_press
              ~callback:(fun event ->
                           if GdkEvent.Button.button event = 3 then begin
                             self#show_contextual_menu event;
                             true (* we handled the event *)
                           end else
                             false (* we didn't handle the event *)));
    view#set_model (Some the_tree_store#coerce)
    
  method store =
    match !tree_store with
      None ->
        failwith "called store before create_store_and_view"
    | (Some the_tree_store) ->
        the_tree_store
    
  method file_name =
    match !file_name with
      None -> failwith "No file name is currently set"
    | (Some file_name) -> file_name

  method is_file_name_defined =
    match !file_name with
      None -> false
    | Some _ -> true

  method reset_file_name =
    file_name := None

  method set_file_name (new_file_name : string) =
    file_name := Some new_file_name

  method add_string_column ~header ?shown_header
                           ?italic:(italic=false) ?bold:(bold=false)
                           ?hidden:(hidden=false) ?reserved:(reserved=false) ?default 
                           ?constraint_predicate () =
    let column = new string_column ~italic ~bold
                                   ~treeview:self ~hidden ~reserved ?default ~header
                                   ?shown_header
                                   ?constraint_predicate () in
    ((self#add_column (column :> column)) :> string_column)
  method add_editable_string_column ~header ?shown_header
                                    ?italic:(italic=false) ?bold:(bold=false)
                                    ?hidden:(hidden=false) ?reserved:(reserved=false) ?default 
                                    ?constraint_predicate () =
    let column = new editable_string_column ~italic ~bold
                                            ~hidden ~reserved ?default ~treeview:self
                                            ~header ?shown_header ?constraint_predicate () in
    ((Obj.magic (self#add_column (column :> column))) :> editable_string_column)
  method add_checkbox_column ~header ?shown_header
                             ?hidden:(hidden=false) ?reserved:(reserved=false) ?default 
                             ?constraint_predicate () =
    let column = new checkbox_column ~treeview:(Obj.magic self) ~header ?shown_header ~hidden
                                     ~reserved ?default ?constraint_predicate() in
    ((Obj.magic (self#add_column (column :> column))) :> checkbox_column)
  method add_icon_column ~header ?shown_header
                         ?hidden:(hidden=false) ?reserved:(reserved=false) ?default
                         ~strings_and_pixbufs () =
    let column = new icon_column ~treeview:(Obj.magic self) ~header ?shown_header ~hidden
                                 ~reserved ?default ~strings_and_pixbufs () in
    ((Obj.magic (self#add_column (column :> column))) :> icon_column)

  method private bind_or_replace_in_row column_name column_value row =
    bind_or_replace_in_alist column_name column_value row

  (* Add non-specified columns with default values. If any constraint is violated raise
     an exception *)
  method private complete_row ?(ignore_constraints=false) row =
    let unspecified_columns =
      List.filter
        (fun column ->
          not (is_bound_in_alist column#header row))
        self#columns in
    let unspecified_alist =
      List.map
        (fun column ->
          column#header, (column#default ())) (* this fails if there's no default: it's intended *)
        unspecified_columns in
    let complete_row = unspecified_alist @ row in
    (if not ignore_constraints then
      self#check_constraints complete_row);
    complete_row

  method private forest_to_id_forest (forest : row forest) =
    match forest with
    | Empty ->
        Empty
    | NonEmpty(row, subtrees, rest) ->
        let id = (match lookup_alist "_id" row with
                    String row_id -> row_id
                   | _ -> assert false) in
        NonEmpty(id,
                 self#forest_to_id_forest subtrees,
                 self#forest_to_id_forest rest)

  method private forest_to_row_list ~call_complete_row (forest : row forest) =
    match forest with
    | Empty ->
        []
    | NonEmpty(row, subtrees, rest) ->
        let id = (match lookup_alist "_id" row with
                    String row_id -> row_id
                   | _ -> assert false) in
        let row = if call_complete_row then
                    self#complete_row ~ignore_constraints:true row
                  else
                    row in
        (* Make the row *start* with the id: *)
        let row = ("_id", (String id)) :: (remove_from_alist "_id" row) in
        (id, row) ::
        (self#forest_to_row_list ~call_complete_row subtrees) @
        (self#forest_to_row_list ~call_complete_row rest)

  method private forest_to_id_forest_and_line_list ~call_complete_row forest =
    (self#forest_to_id_forest forest),
    (self#forest_to_row_list ~call_complete_row forest)

  method private add_complete_row_with_no_checking ?parent_row_id (row:row) =
    (* Add defaults for unspecified fields: *)
    let row = self#complete_row ~ignore_constraints:true row in
    (* Be sure that we set the _id as the *first* column, so that we can make searches by
       id even when setting all the other columns [To do: this may not be needed
       anymore. --L.]: *)
    let row_id =
      match lookup_alist "_id" row with
        String row_id -> row_id
      | _ -> assert false in
    let row = ("_id", (String row_id)) :: (remove_from_alist "_id" row) in
    let store = self#store in
    let parent_iter_option =
      match parent_row_id with
        None -> None
      | (Some parent_row_id) -> Some(self#id_to_iter parent_row_id) in
    (* Update our internal structures holding the forest data: *)
    id_forest :=
      add_tree_to_forest
        (fun some_id ->
           match parent_row_id with
             None -> false
           | Some parent_row_id -> some_id = parent_row_id)
        row_id Empty
        !id_forest;
(*     print_forest !id_forest Log.print_string; *)
    (* Update the hash table, adding the complete row: *)
    Hashtbl.add id_to_row row_id row;
    let new_row_iter = store#append ?parent:parent_iter_option () in
    (* Set all fields (note that the row is complete, hence there's no need to
       worry about unspecified columns now): *)
    List.iter
      (fun (column_header, datum) ->
        try
(*           Log.printf "  * looking up the column %s: begin\n" column_header; flush_all (); *)
          let column = self#get_column column_header in
(*           Log.printf "  * looking up the column %s: end\n" column_header; flush_all (); *)
          (if column_header = "_id" then begin
(*             Log.printf "  * store#set (it should be the _id) %s: begin\n" column_header; flush_all (); *)
            store#set ~row:new_row_iter ~column:column#gtree_column row_id;
(*             Log.printf "  * store#set (it should be the _id) %s: end\n" column_header; flush_all (); *)
           end else begin
(*             Log.printf "  * column#set %s: begin\n" column_header; flush_all (); *)
            column#set
              row_id
              ~ignore_constraints:true
              datum;
(*             Log.printf "  * column#set %s: end\n" column_header; flush_all (); *)
           end);
          (* Log.printf "  + OK     for column %s\n" column_header; flush_all (); *)
        with e -> begin
          Log.printf "  - WARNING: unknown column %s (%s)\n" column_header (Printexc.to_string e); flush_all ();
        end)
      row;
    (* Expand the parent, if any: we want the new row to show up: *)
    (* (* ...no, not anymore. --L. *)
    (if not self#is_view_detached then
      (match parent_row_id with
        Some parent_row_id -> self#expand_row parent_row_id
      | None -> ())); *)
(*     Log.printf "Added the row %s\n" row_id; flush_all () *)

  method add_row ?parent_row_id (row:row) =
    (* Check that no reserved fields are specified: *)
    List.iter
      (fun (column_header, _) ->
         if (self#get_column column_header)#is_reserved then
           failwith "add_row: reserved columns can not be directly specified")
      row;
    (* Add non-specified fields with default values: *)
    let row = self#complete_row row in
    self#add_complete_row_with_no_checking ?parent_row_id row;
    (* Get the row id: *)
    let row_id =
      match lookup_alist "_id" row with
        String row_id -> row_id
      | _ -> assert false in
    (* A just-added row should be collapsed by default *)
    self#collapse_row row_id;
    (* Return the row id. This is important for the caller: *)
    row_id

  (** Return the current id forest: *)
  method get_id_forest =
    !id_forest

  (** Return a row forest (not the internally-used id forest), containing the
      non-reserved fields *)
  method get_forest =
    map
      (fun row_id ->
         self#get_row row_id)
      !id_forest;

  (** Return a row forest (not the internally-used id forest), containing all
      the fields *)
  method get_complete_forest =
    map
      (fun row_id ->
         self#get_complete_row row_id)
      !id_forest;
  
  (** Completely clear the state, and set it to the given forest. *)
  method set_forest (forest : row forest) =
    self#set_complete_forest
      (map
        (fun row -> self#complete_row row)
        forest)

  (** Completely clear the state, and set it to the given complete forest. *)
  method private set_complete_forest (new_forest : row forest) =
    (* Clear our structures and Gtk structures: *)
    self#clear;
    (* Compute our new structures: *)
    let new_id_forest, new_row_list =
      self#forest_to_id_forest_and_line_list ~call_complete_row:true new_forest in
    (* Set the new id forest: *)
    id_forest := new_id_forest;
    (* Fill the hash table with our new rows: *)
    List.iter
      (fun (row_id, row) ->
        Hashtbl.add id_to_row row_id row)
      new_row_list;
    (* Fill Gtk structures, so that the interface shows our new forest: *)
    let store = self#store in
    Forest.iter
      (fun row_id parent ->
        (* Find the correct Gtk place "where to attach" the new line: *)
        let parent_iter_option =
          (match parent with
             None ->
               None
           | (Some parent_row_id) ->
               Some(self#id_to_iter parent_row_id)) in
        let new_row_iter = store#append ?parent:parent_iter_option () in
        (* Set all fields in the row with id row_id (note that the row is complete, hence
           there's no need to worry about unspecified columns now): *)
        let row = Hashtbl.find id_to_row row_id in
        let id_column = self#get_column "_id" in
        store#set ~row:new_row_iter ~column:id_column#gtree_column row_id;
        List.iter
          (fun (column_header, datum) ->
            try
              let column = self#get_column column_header in
              (if not (column_header = "_id") then
                 column#set row_id ~initialize:true ~row_iter:new_row_iter ~ignore_constraints:true datum);
            with e -> begin
              Log.printf "  - WARNING: error (I guess the problem is an unknown column) %s (%s)\n"
                         column_header
                         (Printexc.to_string e); flush_all ();
            end)
          row;)
      new_id_forest;

  (** Return true iff the given row is currently expanded *)
  method is_row_expanded row_id =
    Hashtbl.mem expanded_row_ids row_id

  (** Return the list of ids of all currently expanded rows *)
  method private expanded_row_ids =
    List.filter
      (fun row_id -> self#is_row_expanded row_id)
      (linearize !id_forest)

  (** Expand exactly the rows with the ids in row_id_list, and collapse
      everything else *)
  method set_expanded_row_ids expanded_row_id_list =
    self#collapse_everything;
    List.iter
      (fun row_id -> self#expand_row row_id)
      expanded_row_id_list

  val next_identifier_and_content_forest_marshaler =
    new Oomarshal.marshaller;

  method save =
    let file_name = self#file_name in (* this failwiths if no filename was set *)
    Log.printf "Saving into %s\n" file_name; flush_all ();
    next_identifier_and_content_forest_marshaler#to_file
      (self#get_next_identifier, self#get_complete_forest)
      file_name;
    Log.printf "Saved into %s: success.\n" file_name; flush_all ();

  method load =
    self#detach_view_in
      (fun () ->
        self#clear;
        let file_name = self#file_name in (* this failwiths if no filename was set *)
        try
          let next_identifier, complete_forest = 
            (next_identifier_and_content_forest_marshaler#from_file file_name) in
          self#set_next_identifier next_identifier;
          self#set_complete_forest complete_forest;
          (if Global_options.get_debug_mode () then
            Forest.print_forest complete_forest pretty_print_row);
        with e -> begin
          Log.printf "Loading the treeview %s: failed (%s); I'm setting an empty forest, in the hope that nothing serious will happen\n\n" file_name (Printexc.to_string e); flush_all ();
        end);
    (* This must be executed with the view attached, as it operates on the GUI: *)
    self#collapse_everything;
            
  (** Also return reserved items: *)
  method get_complete_row row_id =
    Hashtbl.find id_to_row row_id

  method remove_reserved_fields row =
    List.filter
      (fun (header, _) -> not (self#get_column header)#is_reserved)
      row

  method get_row row_id =
    self#remove_reserved_fields (self#get_complete_row row_id)

  method get_row_item row_id column_header =
    lookup_alist column_header (self#get_complete_row row_id)

  (** This needs to be public (it would be 'friend' in C++), but please don't directly
      call it. It's meant for use by the subclasses of 'column. *)
  method set_row (row_id : string) row =
    Hashtbl.add id_to_row row_id row

  method set_row_item (row_id : string) column_header new_item =
    let complete_forest = self#get_complete_forest in
    let updated_complete_forest =
      Forest.map
        (fun complete_row ->
          if (lookup_alist "_id" complete_row) = String row_id then
            bind_or_replace_in_alist column_header new_item complete_row 
          else
            complete_row)
        complete_forest in
    self#set_complete_forest updated_complete_forest

  method remove_row (row_id : string) =
    (* Removing the row from the Gtk+ tree model is a little involved.
       We have to first build an updated version of our internal data
       structures, then completely clear the state, and re-build it
       from our updated version.
       This greatly simplifies the GUI part, which is less comfortable
       to work with than our internal data structures. *)
     (* Ok, save the updated state we want to restore later: *)
     let updated_id_forest =
       filter
         (fun an_id -> not (an_id = row_id))
         !id_forest in
     let updated_content_forest =
       map (fun id -> self#get_complete_row id) updated_id_forest in
     let updated_expanded_row_ids_as_list =
       List.fold_left
         (fun list an_id ->
            if Hashtbl.mem expanded_row_ids an_id then
              an_id :: list
            else
              list)
         []
         (linearize updated_id_forest) in
(*     Log.printf "The rows to be expanded are:\n"; flush_all (); *)
(*     List.iter *)
(*       (fun row_id -> Log.printf "%s " row_id) *)
(*       updated_expanded_row_ids_as_list; *)
(*     Log.printf "\nOk, those were the rows to be expanded.\n"; flush_all (); *)
(*     Log.printf "The new id forest will be:\n"; flush_all (); *)
(*     print_forest updated_id_forest Log.print_string; flush_all (); *)
(*     Log.printf "Ok, that was the future id forest.\n"; flush_all (); *)
     (* Clear the full state, which of course includes the GUI: *)
     self#clear;
     (* Restore the state we have set apart before: *)
     iter
       (fun row parent_tree ->
         let parent_row_id =
           match parent_tree with
             None ->
               None
           | Some node->
               (match lookup_alist "_id" node with
                  String id -> Some id
                | _ -> assert false) in
         self#add_complete_row_with_no_checking ?parent_row_id row)
       updated_content_forest;
     (* Parent rows were expanded at child-creation time, but we also
        want to restore the 'expandedness' of every row, so we have to
        first collapse everything and then re-expand exactly what we
        need: *)
(*      self#collapse_everything; *)
(*      Log.printf "The current forest is:\n"; flush_all (); *)
(*      print_forest !id_forest Log.print_string; flush_all (); *)
(*      Log.printf "Ok, that was the current forest.\n"; flush_all (); *)
(*      List.iter *)
(*        (fun row_id -> Log.printf "About to expand row %s\n" row_id; flush_all (); *)
(*                       self#expand_row row_id) *)
(*        (List.rev updated_expanded_row_ids_as_list); *)

  method remove_subtree (row_id : string) =
    let row_iter = self#id_to_iter row_id in
    (* First find out which rows we have to remove: *)
    let ids_of_the_rows_to_be_removed =
      row_id :: (descendant_nodes row_id !id_forest) in
(*     Log.printf "The rows to remove are:\n"; flush_all (); *)
(*     List.iter *)
(*       (fun row_id -> Log.printf "%s " row_id) *)
(*       ids_of_the_rows_to_be_removed; *)
(*     Log.printf "\nOk, those were the rows to remove.\n"; flush_all (); *)
    (* Ok, now update id_forest, id_to_row and expanded_row_ids: *)
    List.iter
      (fun row_id ->
         id_forest :=
           filter
             (fun a_row_id ->
                not (row_id = a_row_id))
             !id_forest)
      ids_of_the_rows_to_be_removed;
    List.iter
      (fun row_id ->
         Hashtbl.remove id_to_row row_id;
         Hashtbl.remove expanded_row_ids row_id)
      ids_of_the_rows_to_be_removed;
    (* Finally remove the row, together with its subtrees, from the Gtk+
       tree model: *)
    ignore (self#store#remove row_iter);
(*     Log.printf "The new forest is:\n"; flush_all (); *)
(*     print_forest !id_forest Log.print_string; flush_all (); *)
(*     Log.printf "Ok, that was the new forest.\n"; flush_all (); *)

  method clear =
(*     Log.printf "Clearing a treeview\n"; flush_all (); *)
    id_forest := Empty;
    Hashtbl.clear id_to_row;
    Hashtbl.clear expanded_row_ids;
    self#store#clear ();
 
  method iter_to_id (iter:Gtk.tree_iter) : string =
    self#store#get ~row:iter ~column:(self#get_column "_id")#gtree_column

  method iter_to_path iter =
    self#store#get_path iter

  method path_to_iter path =
    self#store#get_iter path

  method id_to_iter (id:string) =
    let result = ref None in
    self#for_all_rows (fun iter -> if (self#iter_to_id iter) = id then result := Some iter);
    match !result with
      Some iter -> iter
    | None -> failwith ("id_to_iter: id " ^ ((* string_of_int *) id) ^ " not found")

  method path_to_id path : string =
    self#iter_to_id (self#path_to_iter path)

  method id_to_path (id:string) =
    self#iter_to_path (self#id_to_iter id)
  
  method for_all_rows f =
    let iter_first = self#store#get_iter_first in
    self#iter_on_forest f iter_first
  method iter_on_forest f (iter:(Gtk.tree_iter option)) =
    match iter with
      None ->
        ()
    | (Some iter) ->
        self#iter_on_tree f iter;
        if self#store#iter_next iter then
          self#iter_on_forest f (Some iter)
  method iter_on_tree f (iter:Gtk.tree_iter) =
    (* iter may be destructively modified, but we don't want to expose this to
       the user: *)
    let copy_of_iter = self#store#get_iter (self#store#get_path iter) in
    f copy_of_iter;
    if self#store#iter_has_child iter then
      let subtrees_iter = self#store#iter_children (Some iter) in
      self#iter_on_forest f (Some subtrees_iter)

  method expand_row id =
    view#expand_row (self#id_to_path id)
  method expand_everything =
    view#expand_all ()

  method collapse_everything =
    view#collapse_all ()
  method collapse_row id =
    view#collapse_row (self#id_to_path id)

  method is_row_highlighted row_id =
    match self#get_row_item row_id "_highlight" with
      CheckBox b -> b
  | _ -> assert false

  method highlight_row row_id =
    let highlight_color_column = self#get_column "_highlight" in
    highlight_color_column#set row_id (CheckBox true)

  method unhighlight_row row_id =
    let highlight_color_column = self#get_column "_highlight" in
    highlight_color_column#set row_id (CheckBox false)

  method set_row_highlight_color color row_id =
    let highlight_color_column = self#get_column "_highlight-color" in
    highlight_color_column#set row_id (String color)

  (* Return a list of row_ids such that the complete rows they identify enjoy the
     given property *)
  method row_ids_such_that predicate =
    let filtered_rows =
      List.filter
        predicate
        (Forest.linearize self#get_complete_forest) in
    List.map
      (fun complete_row ->
        item_to_string (lookup_alist "_id" complete_row))
      filtered_rows

  method rows_such_that predicate =
    List.map self#get_row (self#row_ids_such_that predicate)

  method row_such_that predicate =
    Log.print_string "!!!!A1 treeview: row_such_that: begin\n"; flush_all ();
    let result = 
    self#get_row (self#row_id_such_that predicate) in
    Log.print_string "!!!!A1 treeview: row_such_that: end (success)\n"; flush_all ();
    result

  (** Return the row_id of the only row satisfying the given predicate. Fail if more
      than one such row exist: *)
  method row_id_such_that predicate =
    let row_ids = self#row_ids_such_that predicate in
    match row_ids with
      row_id :: [] -> row_id
    | _ -> failwith (Printf.sprintf
                       "row_id_such_that: there were %i results instead of 1"
                       (List.length row_ids))

  (** Return an option containing the the row_id of the parent row, if any. *)
  method parent_of row_id =
    let parent_of_row_id = ref None in
    Forest.iter
      (fun a_row_id its_parent_id_if_any ->
        if a_row_id = row_id then
          parent_of_row_id := its_parent_id_if_any)
      !id_forest;
    (match !parent_of_row_id with
      Some parent_id -> Log.printf "The parent of row %s is %s\n" row_id parent_id; flush_all ()
    | None -> Log.printf "Row %s has no parent\n" row_id; flush_all ());
    !parent_of_row_id

  method children_of row_id =
    Forest.children_nodes row_id !id_forest

  method set_column_visibility header visibility =
    (self#get_column header)#gtree_view_column#set_visible visibility

  val is_view_detached =
    ref false

  (** See detach_view_in: *)
  method is_view_detached =
    !is_view_detached

  (** Temporarily detach the view while executing the thunk, so that updates don't show up in
      the GUI. Using this improves performance when adding/removing a lot of rows. Any exception
      raised by the thunk is correctly propagated after re-attaching the view. *)
  method detach_view_in (thunk : unit -> unit) =
    let model : GTree.model = self#store#coerce in
    view#set_model None;
    (try
      is_view_detached := true;
      thunk ();
      is_view_detached := false;
      view#set_model (Some model);
    with e -> begin
      is_view_detached := false;
      view#set_model (Some model);
      raise e;
    end)

  initializer
    (* Add hidden reserved columns: *)
    let _ =
      self#add_string_column
        ~header:"_id"
        ~reserved:true
        ~default:(fun () -> String (string_of_int self#make_identifier))
        ~hidden:hide_reserved_fields
        () in
    let _ =
      self#add_editable_string_column
        ~header:"_highlight-color"
        ~reserved:true
        ~default:(fun () -> String "dark red")
        ~hidden:hide_reserved_fields
        () in
    let _ =
      self#add_checkbox_column
        ~header:"_highlight"
        ~reserved:true
        ~default:(fun () -> CheckBox false)
        ~hidden:hide_reserved_fields
        () in
    ();

    self#add_menu_item
      "Tout développer"
      (fun _ -> true)
      (fun selected_rowid_if_any ->
        self#expand_everything);
    self#add_menu_item
      "Tout collapser"
      (fun _ -> true)
      (fun selected_rowid_if_any ->
        self#collapse_everything);
    self#add_separator_menu_item;
end;;
