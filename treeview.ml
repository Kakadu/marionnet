(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007, 2008, 2009  Luca Saiu
   Copyright (C) 2009, 2010, 2012  Jean-Vincent Loddo
   Copyright (C) 2007, 2008, 2009, 2010, 2012  Université Paris 13

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

open Gettext;;

module Row_item = struct

  type t =
  | String of string
  | CheckBox of bool
  | Icon of string;; (* Ugly, but this avoids that OCaml bitches about
			parametric polymorphism and classes *)

  (* For debugging: *)
  let sprint = function
  | String s   -> Printf.sprintf "String \"%s\"" s
  | CheckBox b -> Printf.sprintf "CheckBox %b" b
  | Icon s     -> Printf.sprintf "Icon \"%s\"" s

  let failwith x = failwith ("Treeview.Row_item."^x)
  let failwithf frmt t = failwith (Printf.sprintf frmt (sprint t))

  let extract_String = function
  | String s -> s
  | t -> failwithf "Row_item.extract_String: expected a Row_item.String, but found %s" t;;

  let extract_CheckBox = function
  | CheckBox b -> b
  | t -> failwithf "Row_item.extract_CheckBox: expected a Row_item.CheckBox, but found %s" t;;

  let extract_Icon = function
  | Icon s -> s
  | t -> failwithf "Row_item.extract_Icon: expected a Row_item.Icon, but found %s" t;;

  let return_String x   = String x
  let return_CheckBox x = CheckBox x
  let return_Icon x     = Icon x

  module type Projection_injection =
    sig
      val constr_name : string
      type a
      val extract : t -> a
      val return  : a -> t
    end

  module String_prj_inj : (Projection_injection with type a = string) =
    struct
      let constr_name="String"
      type a = string let extract = extract_String let return = return_String
    end

  module CheckBox_prj_inj : (Projection_injection with type a = bool) =
    struct
      let constr_name="CheckBox"
      type a = bool let extract = extract_CheckBox let return = return_CheckBox
    end

  module Icon_prj_inj : (Projection_injection with type a = string) =
    struct
      let constr_name="Icon"
      type a = string let extract = extract_Icon let return = return_Icon
    end

  (** Return a written representation of the given item, suitable for debugging,
      which also includes the constructor: *)
  let to_pretty_string = function
  | String s   -> Printf.sprintf "#string<%s>" s
  | CheckBox b -> Printf.sprintf "#checkbox<%b>" b
  | Icon s     -> Printf.sprintf "#icon<%s>" s

end (* module Row_item *)

(** A row is simply a list of non-conflicting pairs (field, row_item). We
    implement it in this way to make it easy to marshal, and realatively easy
    to manipulate at runtime. Of course by using lists instead of tuples we're
    avoiding potentially helpful type checks here, but here flexibility is more
    important *)
module Row = struct

  (* A row is in practice a record (non-ordered tuple with projections): *)
  type t = (field * Row_item.t) list
   and field = string

  (* Methods for generic fields:  *)

  let get_field ~field t : Row_item.t =
    ListExtra.Assoc.find field t

  let set_field ~field ~(value:Row_item.t) t : t =
    ListExtra.Assoc.set t field value

  let field_exists ~field t : bool =
    ListExtra.Assoc.mem field t

  let failwith x = failwith ("Treeview.Row."^x)

  let field_not_found ~caller ~field =
    failwith (Printf.sprintf "%s: row field `%s' not found" caller field)

  module Make_field_accessors (Prj_inj : Row_item.Projection_injection)
  : sig
      val set : field:string -> value:Prj_inj.a -> t -> t
      val get : ?caller:string -> field:string -> t -> Prj_inj.a
      val eq  : ?fallback:bool -> ?caller:string -> field:string -> value:Prj_inj.a -> t -> bool
    end
  = struct

    let unexpected_item ~caller ~field =
      failwith
        (Printf.sprintf "%s: row field `%s' is not a Row_item.%s"
           caller field Prj_inj.constr_name)

    let set ~field ~value t =
      set_field t ~field ~value:(Prj_inj.return value)

    let get
      ?(caller=(Printf.sprintf "%s_field.get" Prj_inj.constr_name)) ~field t =
      try
        let v = get_field ~field t in
        (try Prj_inj.extract v with _ -> unexpected_item ~caller ~field)
      with
        | Not_found -> field_not_found ~caller ~field

    let eq
      ?fallback
      ?(caller=(Printf.sprintf "%s_field.eq" Prj_inj.constr_name))
      ~field ~value t
      =
      let code () = ((get ~caller ~field t) = value) in
      match fallback with
      | None -> code ()
      | Some b -> try code () with _ -> b (* usually b=false *)

  end (* functor Row.Make_field_accessors *)

  module Icon_field     = Make_field_accessors(Row_item.Icon_prj_inj)
  module String_field   = Make_field_accessors(Row_item.String_prj_inj)
  module CheckBox_field = Make_field_accessors(Row_item.CheckBox_prj_inj)

  (* Specific methods for the string field "Name": *)
  let eq_name ?fallback value = String_field.eq ?fallback ~caller:"eq_name" ~field:"Name" ~value
  let get_name     = String_field.get ~caller:"get_name" ~field:"Name"
  let set_name value t = String_field.set ~field:"Name" ~value t

  (* Specific methods for the string field "_id": *)
  let eq_id ?fallback value = String_field.eq ?fallback ~caller:"eq_id" ~field:"_id" ~value
  let get_id     = String_field.get ~caller:"get_id" ~field:"_id"
  let set_id value t = String_field.set ~field:"_id" ~value t

 (** Print a written representation of the given row, suitable for debugging;
    the printed string also includes the constructor: *)
  let to_pretty_string row =
    let buffer = Buffer.create 100 in
    let print_string x = Buffer.add_string buffer x in
    let rec loop row =
      match row with
      | [] -> ()
      | (name, value) :: rest -> begin
	  print_string (Printf.sprintf "%s=%s " name (Row_item.to_pretty_string value));
	  loop rest;
      end in
    print_string "{ ";
    loop row;
    print_string "}";
    let result = Buffer.contents buffer in
    result

  let pretty_print ~channel row =
    Printf.kfprintf flush channel "%s\n" (to_pretty_string row)

end (* module Row *)

type row_id = int;;

module Defaults = struct
 let highlight_foreground_color = "Black";;
 let highlight_color = "Bisque";;
end;;

type column_type =
  | StringColumnType
  | CheckBoxColumnType
  | IconColumnType;;

class virtual column =
let last_used_id = ref 0 in
fun ~(hidden:bool)
    ~(reserved:bool)
    ?(default:(unit -> Row_item.t) option)
    ~(header:string)
    ?(shown_header=header)
    ?constraint_predicate:(constraint_predicate = (fun (_ : Row_item.t) -> true))
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
  method virtual can_contain : Row_item.t -> bool
  method virtual gtree_column : 'a . 'a GTree.column
  method virtual append_to_view : GTree.view -> unit
  method virtual get : string -> Row_item.t
  method virtual set : ?initialize:bool -> ?row_iter:Gtk.tree_iter -> ?ignore_constraints:bool -> string -> Row_item.t -> unit

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

type constraint_name = string ;;
type column_header = string ;;

exception RowConstraintViolated    of constraint_name ;;
exception ColumnConstraintViolated of column_header ;;

class string_column =
fun ~(treeview:treeview)
    ?hidden:(hidden=false)
    ?reserved:(reserved=false)
    ?default
    ?italic:(italic=false)
    ?bold:(bold=false)
    ~(header:string)
    ?(shown_header)
    ?constraint_predicate:(constraint_predicate = (fun (_ : Row_item.t) -> true))
    () -> object(self)
  inherit column ~hidden ~reserved ?default ~header ?shown_header ~constraint_predicate () as super

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
          `FOREGROUND treeview#get_highlight_foreground_color;
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
    Row_item.String(store#get ~row:tree_iter ~column:self#gtree_column)

  method set ?(initialize=false) ?row_iter ?(ignore_constraints=false) row_id item =
    (if not (self#header = "_id") then
       (if not initialize then begin
          let current_row =
            treeview#get_complete_row row_id in
          let new_row = Row.set_field ~field:self#header ~value:item current_row in
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
      Row_item.String s ->
        store#set
          ~row:tree_iter
          ~column:self#gtree_column
          s
    | _ ->
        failwith (Printf.sprintf "set: wrong datum type for string column %s" self#header)
end

and (*class *) editable_string_column =
fun ~(treeview:treeview)
    ?hidden:(hidden=false)
    ?reserved:(reserved=false)
    ?default
    ~(header:string)
    ?(shown_header)
    ?constraint_predicate:(constraint_predicate = (fun (_ : Row_item.t) -> true))
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
          `FOREGROUND treeview#get_highlight_foreground_color;
          `STYLE (if italic then `ITALIC else `NORMAL);
          `WEIGHT (if bold then `BOLD else `NORMAL); ]
    in
    let col = GTree.view_column
        ~title:self#shown_header
        ~renderer:(renderer, [ "text", column#gtree_column;
                               "foreground_set", (highlight_column#gtree_column);
                               "cell_background_set", (highlight_column#gtree_column);
                               "cell_background", (highlight_color_column#gtree_column); ])
        ()
    in
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
        Row_item.String s -> s
      | _ -> assert false in
    (try
      (!before_edit_commit_callback) id old_content new_content;
      self#set id (Row_item.String new_content);
      (!after_edit_commit_callback) id old_content new_content;
      treeview#run_after_update_callback id;
    with e -> begin
      Log.printf1
        "on_edit: a callback raised an exception (%s), or a constraint was violated.\n"
        (Printexc.to_string e);
      flush_all ();
    end);

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
end

and (*class *) checkbox_column =
fun ~(treeview:treeview)
    ?hidden:(hidden=false)
    ?reserved:(reserved=false)
    ?default
    ~(header:string)
    ?(shown_header)
    ?constraint_predicate:(constraint_predicate = (fun (_ : Row_item.t) -> true))
    () -> object(self)
  inherit column ~hidden ~reserved ?default ~header ?shown_header
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
    Row_item.CheckBox(store#get ~row:tree_iter ~column:self#gtree_column)

  method set ?(initialize=false) ?row_iter ?(ignore_constraints=false) row_id (item : Row_item.t) =
    (if not initialize then begin
       let current_row = treeview#get_complete_row row_id in
       let new_row = Row.set_field ~field:self#header ~value:item current_row in
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
      Row_item.CheckBox value ->
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
        Row_item.CheckBox value -> value
      | _ -> assert false in
    let new_content = not old_content in
    (try
      (!before_toggle_commit_callback) id old_content new_content;
      self#set id (Row_item.CheckBox new_content);
      (!after_toggle_commit_callback) id old_content new_content;
      treeview#run_after_update_callback id;
    with _ -> begin
      Log.printf "on_toggle: a callback raised an exception, or a constraint was violated.\n";
    end);

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
end

and (*class *) icon_column =
fun ~(treeview:treeview)
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
      name, (GdkPixbuf.from_file pixbuf_pathname))
    strings_and_pixbufs in
object(self)
  inherit column ~hidden ~reserved ?default ~header ?shown_header () as super

  method private lookup predicate =
    let singleton = List.filter predicate strings_and_pixbufs in
    if not ((List.length singleton) = 1) then begin
      Log.printf1 "ERROR: icon name lookup failed: found %i results instead of 1\n" (List.length singleton);
      List.iter
        (fun (name, pixbuf) ->
          Log.printf2
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
      (Row_item.Icon icon) ->
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
    Row_item.Icon(store#get ~row:tree_iter ~column:self#gtree_column)

  method set ?(initialize=false) ?row_iter ?(ignore_constraints=false) row_id (item : Row_item.t) =
    (if not initialize then begin
       let current_row = treeview#get_complete_row row_id in
       let new_row = Row.set_field ~field:self#header ~value:item current_row  in
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
      Row_item.Icon name ->
        store#set
          ~row:tree_iter
          ~column:self#gtree_column
          name
    | _ ->
        failwith (Printf.sprintf "set: wrong datum type for icon column %s" self#header)
end

and (*class *) treeview = fun
  ~packing
  ?(hide_reserved_fields=true)
  ?(highlight_foreground_color=Defaults.highlight_foreground_color)
  ?(highlight_color=Defaults.highlight_color)
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
let counter = new Counter.c ~initial_value:0 () in
object(self)

  method gtree_column_list : GTree.column_list = gtree_column_list
  method counter = counter

  val mutable highlight_foreground_color = highlight_foreground_color
  method get_highlight_foreground_color = highlight_foreground_color
  method set_highlight_foreground_color x = highlight_foreground_color <- x

  val mutable highlight_color = highlight_color
  method get_highlight_color = highlight_color
  method set_highlight_color x = highlight_color <- x

  val tree_store = ref None

  val id_forest =
    ref Forest.empty

  val get_column =
    Hashtbl.create 100

  method get_column header =
    try
      ((Hashtbl.find get_column header) :> column)
    with e -> begin
      Log.printf2 "treeview: get_column: failed in looking for column \"%s\" (%s)\n" header (Printexc.to_string e);
      raise e; (* re-raise *)
    end

  method is_column_reserved header =
    let column = self#get_column header in
    column#is_reserved

  val id_to_row =
    Hashtbl.create 1000

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
  method private add_column (column : column) : unit =
    columns := !columns @ [ column ];
    Hashtbl.add get_column column#header column

  method private on_row_activation path column =
    let id = self#path_to_id path in
    !double_click_on_row_callback id

  method private on_row_collapse iter _ =
    let id = self#iter_to_id iter in
    Hashtbl.remove expanded_row_ids id;
    !collapse_row_callback id

  method private on_row_expand iter _ =
    let id = self#iter_to_id iter in
    Hashtbl.add expanded_row_ids id ();
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
    Log.printf "Showing the contextual menu\n";
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

  val filename : string option Chip.wref = Chip.wref ~name:"treeview#filename" None
  method filename = filename

  (** Note that the states directory {e must} be an absolute pathname
      and {e must} include a trailing slash *)
  val directory : string option Chip.wref = Chip.wref ~name:"treeview#directory" None
  method directory = directory

  method add_string_column
    ~header ?shown_header
    ?(italic=false) ?(bold=false)
    ?(hidden=false) ?(reserved=false) ?default
    ?constraint_predicate
    () =
    let string_column =
      new string_column
        ~italic ~bold
        ~treeview:(self :> treeview) ~hidden ~reserved ?default ~header
        ?shown_header
        ?constraint_predicate
        ()
    in
    let () = self#add_column (string_column :> column) in
    string_column

  method add_editable_string_column
    ~header ?shown_header
    ?(italic=false) ?(bold=false)
    ?(hidden=false) ?(reserved=false) ?default
    ?constraint_predicate
    () =
    let editable_string_column =
      new editable_string_column
        ~italic ~bold
        ~hidden ~reserved ?default ~treeview:(self :> treeview)
        ~header ?shown_header ?constraint_predicate
        ()
    in
    let () = self#add_column (editable_string_column :> column) in
    editable_string_column

  method add_checkbox_column
    ~header ?shown_header
    ?(hidden=false) ?(reserved=false) ?default
    ?constraint_predicate
    () =
    let checkbox_column =
      new checkbox_column
        ~treeview:(self :> treeview) ~header ?shown_header ~hidden
        ~reserved ?default ?constraint_predicate
       ()
    in
    let () = self#add_column (checkbox_column :> column) in
    checkbox_column

  method add_icon_column
    ~header ?shown_header
    ?(hidden=false) ?(reserved=false) ?default
    ~strings_and_pixbufs
    () =
    let icon_column =
      new icon_column
        ~treeview:(self :> treeview) ~header ?shown_header ~hidden
        ~reserved ?default ~strings_and_pixbufs
      ()
    in
    let () = self#add_column (icon_column :> column) in
    icon_column

  (* Add non-specified columns with default values.
     If any constraint is violated raise an exception *)
  method private add_unspecified_columns ?ignore_constraints row =
    let unspecified_columns =
      List.filter
        (fun column -> not (Row.field_exists ~field:column#header row))
        self#columns in
    let unspecified_alist =
      List.map
        (* this fails if there's no default: it's intended: *)
        (fun column -> column#header, (column#default ()))
        unspecified_columns
    in
    let complete_row = unspecified_alist @ row in
    (if ignore_constraints = Some () then
      self#check_constraints complete_row);
    complete_row

  method id_of_complete_row row = Row.get_id row

  method private forest_to_id_forest =
    Forest.map (self#id_of_complete_row)

  method private forest_to_id_row_list ?add_unspecified_columns (forest : Row.t Forest.t)
  : (string * Row.t) list
  =
  let mill =
    match add_unspecified_columns with
    | None    ->
       fun row ->
         let id = self#id_of_complete_row row in
         (id,row)
    | Some () ->
       fun row ->
         let id = self#id_of_complete_row row in
         let row = self#add_unspecified_columns ~ignore_constraints:() row in
         (id,row)
  in
  Forest.to_list (Forest.map (mill) forest)

  method private forest_to_id_forest_and_line_list ?add_unspecified_columns forest =
    (self#forest_to_id_forest forest),
    (self#forest_to_id_row_list ?add_unspecified_columns forest)

  method private add_complete_row_with_no_checking ?parent_row_id (row: Row.t) =
    (* Add defaults for unspecified fields: *)
    let row =
      self#add_unspecified_columns ~ignore_constraints:() row
    in
    (* Be sure that we set the _id as the *first* column, so that we can make searches by
       id even when setting all the other columns [To do: this may not be needed
       anymore. --L.]: *)
    let row_id = self#id_of_complete_row row in
    let row = Row.set_id row_id row in
    let store = self#store in
    let parent_iter_option =
      match parent_row_id with
        None -> None
      | (Some parent_row_id) -> Some(self#id_to_iter parent_row_id) in
    (* Update our internal structures holding the forest data: *)
    id_forest :=
      Forest.add_tree_to_forest
        (fun some_id ->
           match parent_row_id with
             None -> false
           | Some parent_row_id -> some_id = parent_row_id)
        row_id Forest.empty
        !id_forest;
    (* Update the hash table, adding the complete row: *)
    Hashtbl.add id_to_row row_id row;
    let new_row_iter = store#append ?parent:parent_iter_option () in
    (* Set all fields (note that the row is complete, hence there's no need to
       worry about unspecified columns now): *)
    List.iter
      (fun (column_header, datum) ->
        try
          let column = self#get_column column_header in
          (if column_header = "_id" then begin
            store#set ~row:new_row_iter ~column:column#gtree_column row_id;
           end else begin
            column#set
              row_id
              ~ignore_constraints:true
              datum;
           end);
        with e -> begin
          Log.printf2 "  - WARNING: unknown column %s (%s)\n" column_header (Printexc.to_string e);
        end)
      row;

  method add_row ?parent_row_id (row:Row.t) =
    (* Check that no reserved fields are specified: *)
    List.iter
      (fun (column_header, _) ->
         if (self#get_column column_header)#is_reserved then
           failwith "add_row: reserved columns can not be directly specified")
      row;
    (* Add non-specified fields with default values: *)
    let row = self#add_unspecified_columns row in
    self#add_complete_row_with_no_checking ?parent_row_id row;
    (* Get the row id: *)
    let row_id = self#id_of_complete_row row in
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
    Forest.map
      (fun row_id ->
         self#get_row row_id)
      !id_forest;

  (** Return a row forest (not the internally-used id forest), containing all
      the fields *)
  method get_complete_forest =
    Forest.map
      (fun row_id ->
         self#get_complete_row row_id)
      !id_forest;

  (** Completely clear the state, and set it to the given forest. *)
  method set_forest (forest : Row.t Forest.t) =
    self#set_complete_forest
      (Forest.map
        (fun row -> self#add_unspecified_columns row)
        forest)

  (** Completely clear the state, and set it to the given complete forest. *)
  method private set_complete_forest (new_forest : Row.t Forest.t) =
    (* Clear our structures and Gtk structures: *)
    self#clear;
    (* Compute our new structures: *)
    let new_id_forest, new_row_list =
      self#forest_to_id_forest_and_line_list ~add_unspecified_columns:() new_forest
    in
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
              Log.printf2
                "WARNING: error (I guess the problem is an unknown column) %s (%s)\n"
                column_header
                (Printexc.to_string e);
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
      (Forest.to_list !id_forest)

  (** Expand exactly the rows with the ids in row_id_list, and collapse
      everything else *)
  method set_expanded_row_ids expanded_row_id_list =
    self#collapse_everything;
    List.iter
      (fun row_id -> self#expand_row row_id)
      expanded_row_id_list

  val next_identifier_and_content_forest_marshaler =
    new Oomarshal.marshaller;

  (* For debugging: *)
  method print =
    let string_of_node node =
      let buffer = Buffer.create 100 in
      let print_string x = Buffer.add_string buffer x in
      print_string "[ ";
      List.iter
	(fun (s, row_item) ->
	  print_string (Printf.sprintf "%s: " s);
	  match row_item with
	  | Row_item.String s ->
	      print_string (Printf.sprintf "%s; " s)
	  | Row_item.CheckBox b ->
	      print_string
		(Printf.sprintf "%s; " (if b then "T" else "F"))
	  | Row_item.Icon i ->
	      print_string (Printf.sprintf "icon:%s; " i))
	node;
      print_string "]\n";
      let result = Buffer.contents buffer in
      result
    in (* end of string_of_node *)
    let forest = self#get_complete_forest in
    let next_identifier = (self#counter#get_next_fresh_value) in
    Printf.kfprintf flush stderr "Next identifier: %i\n" next_identifier;
    Forest.print_forest ~string_of_node ~channel:stderr forest


  method save ?(with_forest_treatment=fun x->x) () =
    let file_name = Option.extract filename#get in
    Log.printf1 "treeview#save: saving into %s\n" file_name;
    let forest = with_forest_treatment (self#get_complete_forest) in
    next_identifier_and_content_forest_marshaler#to_file
      (self#counter#get_next_fresh_value, forest)
      file_name;
    
(*  (* Per ifconfig *)
  method file_version =     
    let file_name = Option.extract filename#get in
    let regexp_v1 = "IPv6 address.*IPv4 broadcast.*IPv4 netmask.*IPv4 address.*MAC address.*MTU.*Type.*Name" in
    let regexp_v2 = "IPv6 gateway.*IPv6 address.*IPv4 gateway.*IPv4 address.*MAC address.*MTU.*Type.*Name" in
    let x = StringExtra.of_charlist (get_first_chars_of_file file_name 250) in
    if StrExtra.First.matchingp (Str.regexp regexp_v1) x then Some "1" else (* continue:*)
    if StrExtra.First.matchingp (Str.regexp regexp_v2) x then Some "2" else (* continue:*)
    None*)
    
  method load =
    self#detach_view_in
      (fun () ->
        self#clear;
        let file_name = Option.extract filename#get in
        try
(*           let () = failwith "LOAD!!!!!!" in (* PROBLEMA: NON SI CHIUDE LA FINESTRA DI APERTURA!!!!!!!!!*) *)
          let next_identifier, complete_forest =
            (next_identifier_and_content_forest_marshaler#from_file file_name) in
          self#counter#set_next_fresh_value_to next_identifier;
          self#set_complete_forest complete_forest;
          (if (Global_options.Debug_level.get ()) >= 3 then (* we are manually setting the verbosity 3 *)
            Forest.print_forest ~string_of_node:Row.to_pretty_string ~channel:stderr complete_forest);
        with e -> begin
          Log.printf2 "Loading the treeview %s: failed (%s); I'm setting an empty forest, in the hope that nothing serious will happen\n\n" file_name (Printexc.to_string e);
        end);
    (* This must be executed with the view attached, as it operates on the GUI: *)
    self#collapse_everything;

  (** Also return reserved fields: *)
  method get_complete_row row_id =
    Hashtbl.find id_to_row row_id

  method remove_reserved_fields row =
    List.filter
      (fun (header, _) -> not (self#get_column header)#is_reserved)
      row

  (** Hide reserved fields: *)
  method get_row row_id =
    self#remove_reserved_fields (self#get_complete_row row_id)

  method get_row_field row_id field =
    Row.get_field (self#get_complete_row row_id) ~field

  method unique_row_exists_with_binding ~(field:string) ~(value:string) =
   let predicate row : bool = (Row.String_field.eq ~fallback:false ~field ~value row) in
   self#is_there_a_unique_row_such_that (predicate)

  (** This needs to be public (it would be 'friend' in C++), but please don't directly
      call it. It's meant for use by the subclasses of 'column. *)
  method set_row (row_id : string) row =
    Hashtbl.add id_to_row row_id row

  method set_row_field (row_id : string) field new_item =
    let complete_forest = self#get_complete_forest in
    let updated_complete_forest =
      Forest.search_and_replace
        (fun row -> (self#id_of_complete_row row) = row_id)
        (fun row -> Row.set_field ~field ~value:new_item row)
        complete_forest
    in
    self#set_complete_forest updated_complete_forest

  method get_String_field field (row_id:string) =
    Row_item.extract_String (self#get_row_field row_id field)

  method set_String_field field (row_id:string) x =
    self#set_row_field row_id field (Row_item.String x)

  method update_String_field field (update:string->string) (row_id:string) =
    let v = self#get_String_field field row_id in
    self#set_String_field field row_id (update v)

  method get_Icon_field field (row_id:string) =
    Row_item.extract_Icon (self#get_row_field row_id field)

  method set_Icon_field field (row_id:string) x =
    self#set_row_field row_id field (Row_item.Icon x)

  method get_CheckBox_field field (row_id:string) =
    Row_item.extract_CheckBox (self#get_row_field row_id field)

  method set_CheckBox_field field (row_id:string) x =
    self#set_row_field row_id field (Row_item.CheckBox x)

  method remove_row (row_id : string) =
    (* Removing the row from the Gtk+ tree model is a little involved.
       We have to first build an updated version of our internal data
       structures, then completely clear the state, and re-build it
       from our updated version.
       This greatly simplifies the GUI part, which is less comfortable
       to work with than our internal data structures. *)
     (* Ok, save the updated state we want to restore later: *)
     let updated_id_forest =
       Forest.filter
         (fun an_id -> not (an_id = row_id))
         !id_forest in
     let updated_content_forest =
       Forest.map (fun id -> self#get_complete_row id) updated_id_forest in
     let _updated_expanded_row_ids_as_list =
       List.fold_left
         (fun list an_id ->
            if Hashtbl.mem expanded_row_ids an_id then
              an_id :: list
            else
              list)
         []
         (Forest.to_list updated_id_forest) in
     (* Clear the full state, which of course includes the GUI: *)
     self#clear;
     (* Restore the state we have set apart before: *)
     Forest.iter
       (fun row parent_tree ->
         let parent_row_id =
           match parent_tree with
           | None -> None
           | Some node-> Some (self#id_of_complete_row node)
         in
         self#add_complete_row_with_no_checking ?parent_row_id row)
       updated_content_forest;

  method remove_subtree (row_id : string) =
    let row_iter = self#id_to_iter row_id in
    (* First find out which rows we have to remove: *)
    let ids_of_the_rows_to_be_removed =
      row_id :: (Forest.descendant_nodes row_id !id_forest) in
    (* Ok, now update id_forest, id_to_row and expanded_row_ids: *)
    List.iter
      (fun row_id ->
         id_forest :=
           Forest.filter
             (fun a_row_id ->
                not (row_id = a_row_id))
             !id_forest)
      ids_of_the_rows_to_be_removed;
    List.iter
      (fun row_id ->
         Hashtbl.remove id_to_row row_id;
         Hashtbl.remove expanded_row_ids row_id)
      ids_of_the_rows_to_be_removed;
    (* Finally remove the row, together with its subtrees, from the Gtk+ tree model: *)
    ignore (self#store#remove row_iter);

  method clear =
    id_forest := Forest.empty;
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
    match self#get_row_field row_id "_highlight" with
      Row_item.CheckBox b -> b
  | _ -> assert false

  method highlight_row row_id =
    let highlight_color_column = self#get_column "_highlight" in
    highlight_color_column#set row_id (Row_item.CheckBox true)

  method unhighlight_row row_id =
    let highlight_color_column = self#get_column "_highlight" in
    highlight_color_column#set row_id (Row_item.CheckBox false)

  method set_row_highlight_color color row_id =
    let highlight_color_column = self#get_column "_highlight-color" in
    highlight_color_column#set row_id (Row_item.String color)

  method get_row_list =
    Forest.to_list self#get_complete_forest

  (* Return a list of row_ids such that the complete rows they identify enjoy the
     given property *)
  method row_ids_such_that predicate =
    let row_list = List.filter predicate self#get_row_list in
    List.map self#id_of_complete_row row_list

  method unique_root_row_id_such_that predicate =
    let roots = Forest.roots_of self#get_complete_forest in
    match (List.filter predicate roots) with
    | [row] -> self#id_of_complete_row row
    | rows ->
       failwith (Printf.sprintf "unique_root_row_id_such_that: there were %i results instead of 1"
                   (List.length rows))

  method rows_such_that predicate =
    List.map self#get_row (self#row_ids_such_that predicate)

  method unique_row_such_that predicate =
    self#get_row (self#unique_row_id_such_that predicate)

  method complete_rows_such_that predicate =
    List.map self#get_complete_row (self#row_ids_such_that predicate)

  method unique_complete_row_such_that predicate =
    self#get_complete_row (self#unique_row_id_such_that predicate)

  (** Return the row_id of the only row satisfying the given predicate. Fail if more
      than one such row exist: *)
  method unique_row_id_such_that predicate =
    let row_ids = self#row_ids_such_that predicate in
    match row_ids with
      row_id :: [] -> row_id
    | _ -> failwith (Printf.sprintf
                       "unique_row_id_such_that: there were %i results instead of 1"
                       (List.length row_ids))


  method private is_there_a_unique_row_such_that (predicate : Row.t -> bool) : bool =
    try
      ignore (self#unique_row_id_such_that predicate); true
    with _ -> false

  (** Return an option containing the the row_id of the parent row, if any. *)
  method parent_of row_id =
   Forest.parent_of row_id !id_forest

  method children_of row_id =
    Forest.children_nodes row_id !id_forest

  method children_no row_id =
    let row_ids = self#children_of row_id in
    List.length row_ids

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
        ~default:(fun () -> Row_item.String (string_of_int (self#counter#fresh ())))
        ~hidden:hide_reserved_fields
        () in
    let _ =
      self#add_editable_string_column
        ~header:"_highlight-color"
        ~reserved:true
        ~default:(fun () -> Row_item.String self#get_highlight_color)
        ~hidden:hide_reserved_fields
        () in
    let _ =
      self#add_checkbox_column
        ~header:"_highlight"
        ~reserved:true
        ~default:(fun () -> Row_item.CheckBox false)
        ~hidden:hide_reserved_fields
        () in
    ();

    self#add_menu_item
      (s_ "Expand all")
      (fun _ -> true)
      (fun selected_rowid_if_any ->
        self#expand_everything);
    self#add_menu_item
      (s_ "Collapse all")
      (fun _ -> true)
      (fun selected_rowid_if_any ->
        self#collapse_everything);
    self#add_separator_menu_item;
end;;

(* Convenient alias: *)
class t = treeview

class virtual treeview_with_a_Name_column = fun
  ~packing
  ?hide_reserved_fields
  ?highlight_foreground_color
  ?highlight_color
  () ->
 object(self)
  inherit t ~packing ?hide_reserved_fields ?highlight_foreground_color ?highlight_color ()

  val name_header = "Name"
  method get_row_name    = self#get_String_field    (name_header)
  method set_row_name    = self#set_String_field    (name_header)
  method update_row_name = self#update_String_field (name_header)

  method rename old_name new_name =
    let row_ids = self#row_ids_of_name old_name in
    List.iter (fun row_id -> self#set_row_name row_id new_name) row_ids

  method unique_root_row_id_of_name name = self#unique_root_row_id_such_that (Row.eq_name name)
  method row_ids_of_name name            = self#row_ids_such_that (Row.eq_name name)
  method rows_of_name name               = self#rows_such_that    (Row.eq_name name)

  method get_row_parent_name row_id =
    let parent_row_id = Option.extract (self#parent_of row_id) in
    self#get_row_name (parent_row_id)

  method get_row_grandparent_name row_id =
    let parent_row_id = Option.extract (self#parent_of row_id) in
    let grandparent_row_id = Option.extract (self#parent_of parent_row_id) in
    self#get_row_name (grandparent_row_id)

  method get_name_list =
    let forest = self#get_complete_forest in
    let forest_of_names = Forest.map (Row.get_name) forest in
    ListExtra.remove_duplicates (Forest.to_list forest_of_names)

  initializer
    let _ =
      self#add_string_column
        ~header:name_header
        ~shown_header:(s_ "Name")
        ()
     in ()

 end (* treeview_with_a_Name_column *)

(* Name is here a primary key: *)
class virtual treeview_with_a_primary_key_Name_column
  ~packing
  ?hide_reserved_fields
  ?highlight_foreground_color
  ?highlight_color
  () =
  object(self)
  inherit treeview_with_a_Name_column ~packing ?hide_reserved_fields ?highlight_foreground_color ?highlight_color ()

  method unique_row_id_of_name name = self#unique_row_id_such_that  (Row.eq_name name)
  method unique_row_of_name name    = self#unique_row_such_that     (Row.eq_name name)

  method children_no_of ~parent_name =
    let row_id = self#unique_row_id_of_name parent_name in
    self#children_no row_id

  (** Do nothing if there is no such name. *)
  method remove_subtree_by_name name =
    try
      let row_id = self#unique_row_id_of_name name in
      self#remove_subtree row_id;
    with _ -> ()

  method update_children_no ~(add_child_of:string -> unit) ~parent_name new_children_no =
    let row_id = self#unique_row_id_of_name parent_name in
    let row_ids = self#children_of row_id in
    let old_children_no = List.length row_ids in
    let delta = new_children_no - old_children_no in
    if delta >= 0 then
      for i = old_children_no + 1 to new_children_no do
        add_child_of parent_name
      done
    else begin
      let reversed_row_ids = List.rev row_ids in
      List.iter self#remove_row (ListExtra.head ~n:(-(delta)) reversed_row_ids);
    end;

  method get_complete_row_of_child ~parent_name ~child_name =
    let row_id = self#unique_row_id_of_name parent_name in
    let row_ids = self#children_of row_id in
    let filtered_data =
      List.filter
        (Row.eq_name child_name)
        (List.map self#get_complete_row row_ids)
    in
    assert((List.length filtered_data) = 1);
    List.hd filtered_data

  method get_row_of_child ~parent_name ~child_name =
    self#remove_reserved_fields (self#get_complete_row_of_child ~parent_name ~child_name)

 end

(* Add the two buttons "Expand all" and "Collapse all" at right side of the treeview. *)
let add_expand_and_collapse_button ~(window:GWindow.window) ~(hbox:GPack.box) (treeview:t) : unit =
  let toolbar =
    let packing w = hbox#pack ~expand:false w in
    GButton.toolbar ~orientation:`VERTICAL ~packing ()
  in
  let packing = toolbar#add in
  let b1 = Gui_bricks.button_image ~window ~packing ~file:"ico.action.zoom.in.png" () in
  let b2 = Gui_bricks.button_image ~window ~packing ~file:"ico.action.zoom.out.png" () in
  let () =
    let set = (GData.tooltips ())#set_tip in
    set b1#coerce ~text:(s_ "Expand all");
    set b2#coerce ~text:(s_ "Collapse all")
  in
  let () =
    let set (b:GButton.button) callback = ignore (b#connect#clicked ~callback) in
    set b1 (fun () -> treeview#expand_everything);
    set b2 (fun () -> treeview#collapse_everything)
  in
  ()
