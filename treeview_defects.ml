(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007, 2008  Luca Saiu
   Copyright (C) 2009, 2010  Jean-Vincent Loddo
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

open Gettext;;
module Assoc = ListExtra.Assoc ;;
module Row_item = Treeview.Row_item ;;

(** The direction in which data flow in a single port; this is the 'resolution'
    of each defect, for each port: *)
type port_direction =
    InToOut | OutToIn;;

let string_of_port_direction d =
  match d with
    InToOut -> "outward"
  | OutToIn -> "inward";;

type column_header = string

(** The direction in which data flow in a single port; this is the 'resolution'
    of each defect, for each port: *)
type cable_direction =
  LeftToRight | RightToLeft;;

let string_of_cable_direction d =
  match d with
    LeftToRight -> "rightward"
  | RightToLeft -> "leftward";;

class t =
fun ~packing
    ~after_user_edit_callback
    () ->
object(self)
  inherit
    Treeview.treeview_with_a_primary_key_Name_column
      ~packing
      ~highlight_color:"Light Coral"
      ~hide_reserved_fields:true
      ()
  as super

  val non_defective_defaults =
    [ "Loss %", Row_item.String "0";
      "Duplication %", Row_item.String "0";
      "Flipped bits %", Row_item.String "0";
      "Minimum delay (ms)", Row_item.String "0";
      "Maximum delay (ms)", Row_item.String "0"; ]

  val defective_defaults =
    [ "Loss %", Row_item.String "5";
      "Duplication %", Row_item.String "5";
      "Flipped bits %", Row_item.String "0.01";
      "Minimum delay (ms)", Row_item.String "50";
      "Maximum delay (ms)", Row_item.String "100"; ]

  method add_device
    ?(defective_by_default=false)
    ~device_name
    ~device_type
    ~port_no
    ~port_prefix
    ~user_port_offset
    ()
    =
    Log.printf
      "Making a defect treeview entry for %s \"%s\" with %d ports (prefix %s, user port offset %d).\n"
      device_type device_name port_no port_prefix user_port_offset;
    let row_id =
      self#add_row
        [ "Name", Row_item.String device_name;
          "Type", Row_item.Icon device_type;
          "_uneditable", Row_item.CheckBox true; ] in
    self#update_port_no ~defective_by_default ~device_name ~port_no ~port_prefix ~user_port_offset ();
    self#collapse_row row_id;
(*
    Log.printf "eth0 InToOut Loss %%: %f\n"
      (self#get_port_attribute device_name "eth0" InToOut "Loss %");
    flush_all ();
*)
  method add_cable ~cable_name ~cable_type ~left_name ~right_name () =
    let cable_type =
      match cable_type with
      | "direct"     -> "straight-cable"
      | "crossover"  -> "crossover-cable"
      | "nullmodem"  -> assert false
      | _ -> assert false in
    let cable_row_id =
      self#add_row
        [ "Name", Row_item.String cable_name;
          "Type", Row_item.Icon cable_type;
          "_uneditable", Row_item.CheckBox true; ] in
    ignore
      (self#add_row
         ~parent_row_id:cable_row_id
         (List.append
            ["Name", Row_item.String left_name;
             "Type", Row_item.Icon "leftward"]
            non_defective_defaults));
    ignore
      (self#add_row
         ~parent_row_id:cable_row_id
         (List.append
            ["Name", Row_item.String right_name;
             "Type", Row_item.Icon "rightward"]
            non_defective_defaults));
    self#collapse_row cable_row_id;

  (* Used importing hub/switch/.. for backward compatibility: *)
  method change_port_user_offset ~device_name ~user_port_offset =
    let offset = user_port_offset in
    let update name =
      try  Scanf.sscanf name "eth%i"  (fun i -> Printf.sprintf "eth%d"  (i+offset)) with _ ->
      try  Scanf.sscanf name "port%i" (fun i -> Printf.sprintf "port%d" (i+offset)) with _ ->
      name
    in
    let device_row_id = self#unique_row_id_of_name device_name in
    let port_row_ids = Forest.children_nodes device_row_id !id_forest in
    List.iter
      (fun row_id ->
         let name =
           match self#get_row_item row_id "Name" with
           | Row_item.String x -> x
           | _ -> assert false
         in
         self#set_row_item row_id "Name" (Row_item.String (update name)))
      port_row_ids

  method private add_port
    ?(defective_by_default=false)
    ~device_name
    ~port_prefix
    ~user_port_offset
    =
    let defaults =
      if defective_by_default then defective_defaults else non_defective_defaults
    in
    let device_row_id = self#unique_row_id_of_name device_name in
    let current_port_no = self#children_no_of ~parent_name:device_name in
    let current_user_port_index = current_port_no + user_port_offset in
    let port_type =
      match Row_item.to_string (self#get_row_item device_row_id "Type") with
      | "machine" (*| "world_bridge"*) -> "machine-port"
      | "gateway" (* retro-compatibility *) -> "machine-port"
      | _ -> "other-device-port" in
    let port_row_id =
      self#add_row
        ~parent_row_id:device_row_id
        [ "Name", Row_item.String (Printf.sprintf "%s%i" port_prefix current_user_port_index);
          "Type", Row_item.Icon port_type;
          "_uneditable", Row_item.CheckBox true; ] in
    let _inward_row_id =
      (self#add_row
         ~parent_row_id:port_row_id
         (List.append
            ["Name", Row_item.String "inward";
             "Type", Row_item.Icon "inward"]
            non_defective_defaults)) in
    let outward_row_id =
      (self#add_row
         ~parent_row_id:port_row_id
         (List.append
            ["Name", Row_item.String "outward";
             "Type", Row_item.Icon "outward"]
            defaults)) in
    if defective_by_default then begin
      (* In a single direction suffice: *)
      self#show_that_it_is_defective outward_row_id;
    end


  method update_port_no
    ?(defective_by_default=false)
    ~device_name
    ~port_no
    ~port_prefix
    ~user_port_offset
    ()
    =
    let add_child_of device_name =
        self#add_port ~defective_by_default ~device_name ~port_prefix ~user_port_offset
    in
    self#update_children_no ~add_child_of ~parent_name:device_name port_no

  method get_port_data device_name port_name port_direction =
    let port_row = self#get_complete_row_of_child ~parent_name:device_name ~child_name:port_name in
    let port_id = self#id_of_complete_row port_row in
    let port_direction_ids = self#children_of port_id in
    List.find
      (fun row -> Assoc.find "Type" row = Row_item.Icon (string_of_port_direction port_direction))
      (List.map self#get_row port_direction_ids)

  method get_cable_data cable_name cable_direction =
    let cable_row_id = self#unique_row_id_of_name cable_name in
    let cable_direction_ids = self#children_of cable_row_id in
    let filtered_cable_directions =
      List.filter
        (fun row -> Assoc.find "Type" row = Row_item.Icon (string_of_cable_direction cable_direction))
        (List.map self#get_row cable_direction_ids) in
    assert(List.length filtered_cable_directions = 1);
    List.hd filtered_cable_directions

  method rename_cable_endpoints cable_name left_endpoint_name right_endpoint_name =
    let cable_row_id = self#unique_row_id_of_name cable_name in
    let cable_direction_ids = self#children_of cable_row_id in
    assert (List.length cable_direction_ids = 2);
    let directions = List.map self#get_complete_row cable_direction_ids in
    let leftward_direction  = List.find (fun row -> Assoc.find "Type" row = Row_item.Icon "leftward")  directions in
    let rightward_direction = List.find (fun row -> Assoc.find "Type" row = Row_item.Icon "rightward") directions in
    self#set_row_item
      (Row_item.to_string (Assoc.find "_id" leftward_direction))
      "Name"
      (Row_item.String left_endpoint_name);
    self#set_row_item
      (Row_item.to_string (Assoc.find "_id" rightward_direction))
      "Name"
      (Row_item.String right_endpoint_name);

  (** Return a single port attribute as an item: *)
  method get_port_attribute device_name port_name port_direction column_header =
    float_of_string
      (Row_item.to_string
         (Assoc.find
            column_header
            (self#get_port_data device_name port_name port_direction)))

  method get_port_attribute_of
    ~device_name
    ~port_prefix
    ~port_index
    ~user_port_offset
    ~port_direction
    ~column_header
    ()
    =
    let user_port_index = port_index + user_port_offset in
    let port_name = Printf.sprintf "%s%i" port_prefix user_port_index in
    self#get_port_attribute device_name port_name port_direction column_header

  (** Return a single cable attribute as an item: *)
  method get_cable_attribute cable_name cable_direction column_header =
    float_of_string
      (Row_item.to_string
         (Assoc.find
            column_header
            (self#get_cable_data cable_name cable_direction)))

  method private is_empty_or_a_number_between s minimum maximum =
    s = "" or
    (try
      Scanf.sscanf s "%f" (fun x -> x >= minimum && x <= maximum)
    with _ -> false)

  method private is_a_valid_percentage s =
    self#is_empty_or_a_number_between s 0.0 100.0

  method private is_a_valid_non_100_percentage s =
    self#is_empty_or_a_number_between s 0.0 99.0

  method private is_a_valid_delay s =
    self#is_empty_or_a_number_between s 0.0 100000.0

  method private show_that_it_is_defective row_id =
    self#highlight_row row_id

  method private show_that_it_is_not_defective row_id =
    self#unhighlight_row row_id

  (** Return true iff there exists at least a defect in the given row.
      If given, use the given values for minimum and maximum delay instead
      of the ones found in the row: *)
  method private is_defective ?minimum_delay ?maximum_delay row_id =
    let row =
      List.filter
        (fun (header, _) ->
          let c = String.get header ((String.length header) - 1) in
          ((c = ')' or c = '%') && (* we're interested in percentages and times *)
          (match minimum_delay with
            None -> true
          | Some _ -> not (header = "Minimum delay (ms)")) &&
          (match maximum_delay with
            None -> true
          | Some _ -> not (header = "Maximum delay (ms)"))))
        (self#get_row row_id) in
    let values =
      List.map
        (fun (_, i) -> let s = Row_item.to_string i in try float_of_string s with _ -> 0.0)
        row in
    let values =
      match maximum_delay with None -> values | Some x -> x :: values in
    let values =
      match minimum_delay with None -> values | Some x -> x :: values in
    List.exists
      (fun x -> x > 0.0)
      values

  method private show_defectiveness ?minimum_delay ?maximum_delay row_id =
    if self#is_defective ?minimum_delay ?maximum_delay row_id then
      self#show_that_it_is_defective row_id
    else
      self#show_that_it_is_not_defective row_id

  (** grandparent for devices, parent for cables: *)
  method private relevant_device_name_for_row_id row_id =
    try  self#grandparent_name_of row_id
    with _ -> self#parent_name_of row_id

  initializer
    let _ =
      self#add_checkbox_column
        ~header:"_uneditable"
        ~hidden:true
        ~default:(fun () -> Row_item.CheckBox false)
        () in
    let _ =
      self#add_icon_column
        ~header:"Type"
        ~shown_header:(s_ "Type")
        ~strings_and_pixbufs:[
	    "machine", Initialization.Path.images^"treeview-icons/machine.xpm";
	    "hub",     Initialization.Path.images^"treeview-icons/hub.xpm";
	    "switch",  Initialization.Path.images^"treeview-icons/switch.xpm";
	    "router",  Initialization.Path.images^"treeview-icons/router.xpm";
	    "cloud",   Initialization.Path.images^"treeview-icons/cloud.xpm";
	    "world_bridge",  Initialization.Path.images^"treeview-icons/world.xpm";
	    "gateway" (* retro-compatibility: *),  Initialization.Path.images^"treeview-icons/world.xpm";
	    "straight-cable",    Initialization.Path.images^"treeview-icons/cable-grey.xpm";
	    "crossover-cable",   Initialization.Path.images^"treeview-icons/cable-blue.xpm";
	    "machine-port",      Initialization.Path.images^"treeview-icons/network-card.xpm";
	    "other-device-port", Initialization.Path.images^"treeview-icons/port.xpm";

	    "rightward", Initialization.Path.images^"treeview-icons/left-to-right.xpm";
	    "leftward",  Initialization.Path.images^"treeview-icons/right-to-left.xpm";
	    "outward",   Initialization.Path.images^"treeview-icons/in-to-out.xpm";
	    "inward",    Initialization.Path.images^"treeview-icons/out-to-in.xpm";
            ]
        () in
    let loss =
      self#add_editable_string_column
        ~header:"Loss %"
        ~shown_header:(s_ "Loss %")
        ~default:(fun () -> Row_item.String "")
        ~constraint_predicate:(fun i -> let i = Row_item.to_string i in self#is_a_valid_percentage i)
        () in
    loss#set_after_edit_commit_callback
      (fun row_id _ _ ->
        self#show_defectiveness row_id);
    let duplication =
      self#add_editable_string_column
        ~header:"Duplication %"
        ~shown_header:(s_ "Duplication %")
        ~default:(fun () -> Row_item.String "")
        ~constraint_predicate:(fun i -> let i = Row_item.to_string i in self#is_a_valid_non_100_percentage i)
        () in
    duplication#set_after_edit_commit_callback
      (fun row_id _ _ ->
        self#show_defectiveness row_id);
    let flipped_bits =
      self#add_editable_string_column
        ~header:"Flipped bits %"
        ~shown_header:(s_ "Flipped bits %")
        ~default:(fun () -> Row_item.String "")
        ~constraint_predicate:(fun i -> let i = Row_item.to_string i in self#is_a_valid_percentage i)
        () in
    flipped_bits#set_after_edit_commit_callback
      (fun row_id _ content ->
        self#show_defectiveness row_id;
        let content = float_of_string content in
        if content > 1.0 then
          Simple_dialogs.warning
            "This value may be too high"
            "Please consider that a flipped bits percentage greater than 1% implies *many* trasmission errors.\n\nAnyway you are free to experiment with any percentage."
            ());
    let minimum_delay =
      self#add_editable_string_column
        ~header:"Minimum delay (ms)"
        ~shown_header:(s_ "Minimum delay (ms)")
        ~default:(fun () -> Row_item.String "")
        ~constraint_predicate:(fun i -> let i = Row_item.to_string i in self#is_a_valid_delay i)
        () in
    minimum_delay#set_after_edit_commit_callback
      (fun row_id _ new_content ->
        let minimum_delay = if new_content = "" then 0.0 else float_of_string new_content in
        let maximum_delay = Row_item.to_string (self#get_row_item row_id "Maximum delay (ms)") in
        let maximum_delay = if maximum_delay = "" then 0.0 else float_of_string maximum_delay in
        (if minimum_delay > maximum_delay then
          self#set_row_item row_id "Maximum delay (ms)" (Row_item.String (string_of_float minimum_delay)));
        self#show_defectiveness
          ~maximum_delay:(max minimum_delay maximum_delay)
          row_id);
    let maximum_delay =
      self#add_editable_string_column
        ~header:"Maximum delay (ms)"
        ~shown_header:(s_ "Maximum delay (ms)")
        ~default:(fun () -> Row_item.String "")
        ~constraint_predicate:(fun i -> let i = Row_item.to_string i in self#is_a_valid_delay i)
        () in
    maximum_delay#set_after_edit_commit_callback
      (fun row_id _ new_content ->
        let maximum_delay = if new_content = "" then 0.0 else float_of_string new_content in
        let minimum_delay = Row_item.to_string (self#get_row_item row_id "Minimum delay (ms)") in
        let minimum_delay = if minimum_delay = "" then 0.0 else float_of_string minimum_delay in
        (if minimum_delay > maximum_delay then
           self#set_row_item row_id "Minimum delay (ms)" (Row_item.String (string_of_float maximum_delay)));
        self#show_defectiveness
          ~minimum_delay:(min minimum_delay maximum_delay)
          row_id);

  self#add_row_constraint
    ~name:(s_ "you should choose a direction to define this parameter")
    (fun row ->
      let uneditable = Row_item.to_bool (Assoc.find "_uneditable" row) in
      (not uneditable) or
      (List.for_all (fun (name, value) ->
                       name = "Name" or
                       name = "Type" or
                       name = "_uneditable" or
                       self#is_column_reserved name or
                       value = Row_item.String "")
                    row));

    self#set_after_update_callback
      (fun row_id ->
        after_user_edit_callback (self#relevant_device_name_for_row_id row_id));

    (* Make internal data structures: no more columns can be added now: *)
    self#create_store_and_view;

    (* Setup the contextual menu: *)
    self#set_contextual_menu_title "Defects operations";
end;;

class treeview = t
module The_unique_treeview = Stateful_modules.Variable (struct
  type t = treeview
  let name = Some "treeview_defects"
  end)
let extract = The_unique_treeview.extract

let make ~packing ~after_user_edit_callback () =
  let result = new t ~packing ~after_user_edit_callback () in
  The_unique_treeview.set result;
  result
;;
