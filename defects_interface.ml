(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007  Luca Saiu

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

open Treeview;;
open Sugar;;
open Row_item;;
open Gettext;;

(** The direction in which data flow in a single port; this is the 'resolution'
    of each defect, for each port: *)
type port_direction =
    InToOut | OutToIn;;

let string_of_port_direction d =
  match d with
    InToOut ->  "outward"
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

(* To do: move this into EXTRA/ *)
let rec take n xs =
  if n = 0 then
    []
  else
    match xs with
      [] -> failwith "take: n is greater than the list length"
    | x :: rest -> x :: (take (n - 1) rest);;

class defects_interface =
fun ~packing
    ~after_user_edit_callback
    () ->
object(self)
  inherit
    treeview
      ~packing
      ~hide_reserved_fields:true
      ()
  as super

  val non_defective_defaults =
    [ "Loss %", String "0";
      "Duplication %", String "0";
      "Flipped bits %", String "0";
      "Minimum delay (ms)", String "0";
      "Maximum delay (ms)", String "0"; ]

  val defective_defaults =
    [ "Loss %", String "5";
      "Duplication %", String "5";
      "Flipped bits %", String "0.01";
      "Minimum delay (ms)", String "50";
      "Maximum delay (ms)", String "100"; ]

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
        [ "Name", String device_name;
          "Type", Icon device_type;
          "_uneditable", CheckBox true; ] in
    self#update_port_no ~defective_by_default ~device_name ~port_no ~port_prefix ~user_port_offset ();
    self#collapse_row row_id;
(*
    Log.printf "eth0 InToOut Loss %%: %f\n"
      (self#get_port_attribute device_name "eth0" InToOut "Loss %");
    flush_all ();
*)
  method add_cable cable_name cable_type left_endpoint_name right_endpoint_name =
    let cable_type =
      match cable_type with
      | "direct"     -> "straight-cable"
      | "crossover"  -> "crossover-cable"
      | "nullmodem"  -> assert false
      | _ -> assert false in
    let cable_row_id =
      self#add_row
        [ "Name", String cable_name;
          "Type", Icon cable_type;
          "_uneditable", CheckBox true; ] in
    ignore
      (self#add_row
         ~parent_row_id:cable_row_id
         (List.append
            ["Name", String left_endpoint_name;
             "Type", Icon "leftward"]
            non_defective_defaults));
    ignore
      (self#add_row
         ~parent_row_id:cable_row_id
         (List.append
            ["Name", String right_endpoint_name;
             "Type", Icon "rightward"]
            non_defective_defaults));
    self#collapse_row cable_row_id;

  method private remove_device_or_cable device_name =
    let row_id =
      self#device_or_cable_row_id device_name in
    self#remove_subtree row_id;

  method remove_device name =
    self#remove_device_or_cable name

  method remove_cable name =
    self#remove_device_or_cable name

  method private device_or_cable_row_id device_name =
    let result =
      self#row_id_such_that (fun row -> (lookup_alist "Name" row) = String device_name) in
    result

  method port_no_of device_name =
    let device_row_id =
      self#device_or_cable_row_id device_name in
    let port_row_ids =
      Forest.children_nodes device_row_id !id_forest in
    List.length port_row_ids


  (* Used importing hub/switch/.. for backward compatibility: *)
  method change_port_user_offset ~device_name ~user_port_offset =
    let offset = user_port_offset in
    let update name = 
      try  Scanf.sscanf name "eth%i"  (fun i -> Printf.sprintf "eth%d"  (i+offset)) with _ ->
      try  Scanf.sscanf name "port%i" (fun i -> Printf.sprintf "port%d" (i+offset)) with _ ->
      name
    in
    let device_row_id = self#device_or_cable_row_id device_name in
    let port_row_ids = Forest.children_nodes device_row_id !id_forest in
    List.iter
      (fun row_id ->
         let name =
           match self#get_row_item row_id "Name" with
           | String x -> x
           | _ -> assert false
         in
         self#set_row_item row_id "Name" (String (update name)))
      port_row_ids

  method private add_port
    ?(defective_by_default=false)
    ~device_name
    ~port_prefix
    ~user_port_offset
    =
    let defaults =
      if defective_by_default then defective_defaults else non_defective_defaults in
    let device_row_id =
      self#device_or_cable_row_id device_name in
    let current_port_no =
      self#port_no_of device_name in
    let current_user_port_index = current_port_no + user_port_offset in
    let port_type =
      match item_to_string (self#get_row_item device_row_id "Type") with
      | "machine" (*| "world_bridge"*) -> "machine-port"
      | "gateway" (* retro-compatibility *) -> "machine-port"
      | _ -> "other-device-port" in
    let port_row_id =
      self#add_row
        ~parent_row_id:device_row_id
        [ "Name", String (Printf.sprintf "%s%i" port_prefix current_user_port_index);
          "Type", Icon port_type;
          "_uneditable", CheckBox true; ] in
    let _inward_row_id =
      (self#add_row
         ~parent_row_id:port_row_id
         (List.append
            ["Name", String "inward";
             "Type", Icon "inward"]
            non_defective_defaults)) in
    let outward_row_id =
      (self#add_row
         ~parent_row_id:port_row_id
         (List.append
            ["Name", String "outward";
             "Type", Icon "outward"]
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
    let new_port_no = port_no in
    let device_row_id =
      self#device_or_cable_row_id device_name in
    let port_row_ids =
      Forest.children_nodes device_row_id !id_forest in
    let old_port_no =
      self#port_no_of device_name in
    let ports_delta = new_port_no - old_port_no in
    if ports_delta >= 0 then
      for i = old_port_no + 1 to new_port_no do
        self#add_port ~defective_by_default ~device_name ~port_prefix ~user_port_offset
      done
    else begin
      let reversed_port_row_ids = List.rev port_row_ids in
      List.iter self#remove_subtree (take (- ports_delta) reversed_port_row_ids);
    end;

  method private rename_device_or_cable old_name new_name =
    let device_row_id =
      self#device_or_cable_row_id old_name in
    self#set_row_item device_row_id "Name" (String new_name);

  method rename_device =
    self#rename_device_or_cable

  method rename_cable =
    self#rename_device_or_cable

  (** Return all the non-reserved data of a given port, in our usual
      <name, item> alist format: *)
  method get_port_data device_name port_name port_direction =
(*     Log.printf "defects: get_port_data\n"; flush_all (); *)
    let device_row_id = self#device_or_cable_row_id device_name in
    let device_port_ids = self#children_of device_row_id in
    let filtered_ports =
      List.filter
        (fun row -> lookup_alist "Name" row = String port_name)
        (List.map self#get_complete_row device_port_ids) in
    assert(List.length filtered_ports = 1);
    let port_id = item_to_string (lookup_alist "_id" (List.hd filtered_ports)) in
    let port_direction_ids = self#children_of port_id in
    let filtered_port_directions =
      List.filter
        (fun row -> lookup_alist "Type" row = Icon (string_of_port_direction port_direction))
        (List.map self#get_row port_direction_ids) in
    assert(List.length filtered_port_directions = 1);
    List.hd filtered_port_directions

  method get_cable_data cable_name cable_direction =
    let cable_row_id = self#device_or_cable_row_id cable_name in
    let cable_direction_ids = self#children_of cable_row_id in
    let filtered_cable_directions =
      List.filter
        (fun row -> lookup_alist "Type" row = Icon (string_of_cable_direction cable_direction))
        (List.map self#get_row cable_direction_ids) in
    assert(List.length filtered_cable_directions = 1);
    List.hd filtered_cable_directions

  method rename_cable_endpoints cable_name left_endpoint_name right_endpoint_name =
    let cable_row_id = self#device_or_cable_row_id cable_name in
    let cable_direction_ids = self#children_of cable_row_id in
    assert (List.length cable_direction_ids = 2);
    let directions = List.map self#get_complete_row cable_direction_ids in
    let leftward_direction =
      List.hd (List.filter (fun row -> lookup_alist "Type" row = Icon "leftward") directions) in
    let rightward_direction =
      List.hd (List.filter (fun row -> lookup_alist "Type" row = Icon "rightward") directions) in
    self#set_row_item
      (item_to_string (lookup_alist "_id" leftward_direction))
      "Name"
      (String left_endpoint_name);
    self#set_row_item
      (item_to_string (lookup_alist "_id" rightward_direction))
      "Name"
      (String right_endpoint_name);

  (** Return a single port attribute as an item: *)
  method get_port_attribute device_name port_name port_direction column_header =
(*     Log.printf "defects: get_port_attribute\n"; flush_all (); *)
    float_of_string
      (item_to_string
         (lookup_alist
            column_header
            (self#get_port_data device_name port_name port_direction)))

  (** Return a single port attribute as an item, where the port is represented
      by an *index* (for example 2 stands for "eth2" or "port2": *)
  method get_port_attribute_by_index device_name port_index port_direction column_header =
    (* First try with the "eth" prefix: *)
    let port_name = Printf.sprintf "eth%i" port_index in
    try
      self#get_port_attribute device_name port_name port_direction column_header
    with _ ->
      (* We failed. Ok, now try with the "port" prefix, before bailing
         out: *)
      let port_name = Printf.sprintf "port%i" port_index in
      self#get_port_attribute device_name port_name port_direction column_header

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
      (item_to_string
         (lookup_alist
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
    self#set_row_highlight_color "dark red" row_id;
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
        (fun (_, i) -> let s = item_to_string i in try float_of_string s with _ -> 0.0)
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

  method private relevant_device_name_for_row_id row_id =
    let id_forest = self#get_id_forest in
    let parent_row_ids =
      Forest.nodes_such_that
        (fun a_row_id ->
          List.mem
             row_id
             (try Forest.children_nodes a_row_id id_forest with _ -> []))
        id_forest in
    let grandparent_row_ids =
      Forest.nodes_such_that
        (fun a_row_id ->
          List.mem
             row_id
             (try Forest.grandchildren_nodes_with_repetitions a_row_id id_forest with _ -> []))
        id_forest in
    let device_row_ids =
      if List.length grandparent_row_ids > 0 then
        grandparent_row_ids
      else
        parent_row_ids in
    Log.printf "List.length devices_row_id = %i\n" (List.length device_row_ids);
    assert (List.length device_row_ids = 1);
    let device_row_id = List.hd device_row_ids in
    item_to_string (self#get_row_item device_row_id "Name")

  initializer
    let _ =
      self#add_checkbox_column
        ~header:"_uneditable"
        ~hidden:true
        ~default:(fun () -> CheckBox false)
        () in
    let _ =
      self#add_string_column
        ~header:"Name"
        ~shown_header:(s_ "Name")
        ~italic:true
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
        ~default:(fun () -> String "")
        ~constraint_predicate:(fun i -> let i = item_to_string i in self#is_a_valid_percentage i)
        () in
    loss#set_after_edit_commit_callback
      (fun row_id _ _ ->
        self#show_defectiveness row_id);
    let duplication =
      self#add_editable_string_column
        ~header:"Duplication %"
        ~shown_header:(s_ "Duplication %")
        ~default:(fun () -> String "")
        ~constraint_predicate:(fun i -> let i = item_to_string i in self#is_a_valid_non_100_percentage i)
        () in
    duplication#set_after_edit_commit_callback
      (fun row_id _ _ ->
        self#show_defectiveness row_id);
    let flipped_bits =
      self#add_editable_string_column
        ~header:"Flipped bits %"
        ~shown_header:(s_ "Flipped bits %")
        ~default:(fun () -> String "")
        ~constraint_predicate:(fun i -> let i = item_to_string i in self#is_a_valid_percentage i)
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
        ~default:(fun () -> String "")
        ~constraint_predicate:(fun i -> let i = item_to_string i in self#is_a_valid_delay i)
        () in
    minimum_delay#set_after_edit_commit_callback
      (fun row_id _ new_content ->
        let minimum_delay = if new_content = "" then 0.0 else float_of_string new_content in
        let maximum_delay = item_to_string (self#get_row_item row_id "Maximum delay (ms)") in
        let maximum_delay = if maximum_delay = "" then 0.0 else float_of_string maximum_delay in
        (if minimum_delay > maximum_delay then
          self#set_row_item row_id "Maximum delay (ms)" (String (string_of_float minimum_delay)));
        self#show_defectiveness
          ~maximum_delay:(max minimum_delay maximum_delay)
          row_id);
    let maximum_delay =
      self#add_editable_string_column
        ~header:"Maximum delay (ms)"
        ~shown_header:(s_ "Maximum delay (ms)")
        ~default:(fun () -> String "")
        ~constraint_predicate:(fun i -> let i = item_to_string i in self#is_a_valid_delay i)
        () in
    maximum_delay#set_after_edit_commit_callback
      (fun row_id _ new_content ->
        let maximum_delay = if new_content = "" then 0.0 else float_of_string new_content in
        let minimum_delay = item_to_string (self#get_row_item row_id "Minimum delay (ms)") in
        let minimum_delay = if minimum_delay = "" then 0.0 else float_of_string minimum_delay in
        (if minimum_delay > maximum_delay then
           self#set_row_item row_id "Minimum delay (ms)" (String (string_of_float maximum_delay)));
        self#show_defectiveness
          ~minimum_delay:(min minimum_delay maximum_delay)
          row_id);

  self#add_row_constraint
    ~name:(s_ "you should choose a direction to define this parameter")
    (fun row ->
      let uneditable = item_to_bool (lookup_alist "_uneditable" row) in
      (not uneditable) or
      (List.for_all (fun (name, value) ->
                       name = "Name" or
                       name = "Type" or
                       name = "_uneditable" or
                       self#is_column_reserved name or
                       value = String "")
                    row));
(*   self#add_row_constraint *)
(*     ~name:"the maximum delay should be at least as large as the minimum delay" *)
(*     (fun row -> *)
(*       let uneditable = item_to_bool (lookup_alist "_uneditable" row) in *)
(*       if uneditable then *)
(*         true *)
(*       else *)
(*         let minimum_delay = item_to_string (lookup_alist "Minimum delay (ms)" row) in *)
(*         let maximum_delay = item_to_string (lookup_alist "Maximum delay (ms)" row) in *)
(*         let minimum_delay = if minimum_delay = "" then 0.0 else float_of_string minimum_delay in *)
(*         let maximum_delay = if maximum_delay = "" then 0.0 else float_of_string maximum_delay in *)
(*         minimum_delay <= maximum_delay); *)

    self#set_after_update_callback
      (fun row_id ->
        after_user_edit_callback (self#relevant_device_name_for_row_id row_id));

    (* Make internal data structures: no more columns can be added now: *)
    self#create_store_and_view;

    (* Setup the contextual menu: *)
    self#set_contextual_menu_title "Defects operations";
end;;

(** Ugly kludge to make a single global instance visible from all modules
    linked *after* this one. Not having mutually-recursive inter-compilation-unit
    modules is a real pain. *)
let the_defects_interface =
  ref None;;
let get_defects_interface () =
  match !the_defects_interface with
    None -> failwith "No network defects interface exists"
  | Some the_defects_interface -> the_defects_interface;;
let make_defects_interface ~packing ~after_user_edit_callback () =
  let result = new defects_interface ~packing ~after_user_edit_callback () in
  the_defects_interface := Some result;
  result;;
