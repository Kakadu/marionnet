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

open Row_item;;
open Gettext;;
module Assoc = ListExtra.Assoc;;

type port_row_completions = (string * (string * Row_item.row_item) list) list

class t =
fun ~packing
    ~after_user_edit_callback
    () ->
object(self)
  inherit
    Treeview.treeview_with_a_primary_key_Name_column
      ~packing
      ~hide_reserved_fields:true
      ()
  as super

  method private currently_used_mac_addresses : string list =
    let xs = List.flatten (Forest.linearize self#get_forest) in
    let xs = ListExtra.filter_map
      (function
       | "MAC address", (Row_item.String s) -> Some s
       | _ -> None
       )
       xs
    in
    (List.tl xs) (* Discard the first line (header) *)

  (** The three leftmost octects are used as the trailing part of
      automatically-generated MAC addresses.
      Interesting side note: we can't use four because of OCaml
      runtime type tagging (yes, Jean: I was also surprised when I
      discovered it, but it was made that way to support precise GC,
      which can't rely on conservative pointer finding). *)
  method private generate_mac_address =
    let b0 = Random.int 256 in
    let b1 = Random.int 256 in
    let b2 = Random.int 256 in
    let result = Printf.sprintf "02:04:06:%02x:%02x:%02x" b2 b1 b0 in
    (* Try again if we generated an invalid or already allocated address: *)
    if not (List.mem result self#currently_used_mac_addresses) then
      begin
        Log.printf "Generated MAC address: %s\n" result;
        result
      end
    else begin
      Log.printf "Generated MAC address: %s already in use!\n" result;
      self#generate_mac_address
    end
  (** This follows exactly the same logic as automatic MAC address generation.
      Two octects are used for a B class network: *)
  val next_ipv4_address_as_int =
    ref 1
  method private generate_ipv4_address =
    let ipv4_address_as_int = !next_ipv4_address_as_int in
    next_ipv4_address_as_int := ipv4_address_as_int + 1;
    let result =
      Printf.sprintf
        "10.10.%i.%i"
        (ipv4_address_as_int / 256)
        (ipv4_address_as_int mod 256)
    in
    (* Try again if we generated an invalid address: *)
    if Ipv4.String.is_valid_ipv4 result then
      result
    else
      self#generate_ipv4_address

  (** This follows exactly the same logic as automatic MAC address generation.
      Two octects are used for a B class network: *)
  val next_ipv6_address_as_int =
    ref Int64.one
  method private generate_ipv6_address =
    let ipv6_address_as_int = !next_ipv6_address_as_int in
    next_ipv6_address_as_int := Int64.succ ipv6_address_as_int;
    let result =
      Printf.sprintf
        "42::%04x:%04x"
        (Int64.to_int (Int64.div ipv6_address_as_int (Int64.of_int (256 * 256))))
        (Int64.to_int (Int64.rem ipv6_address_as_int (Int64.of_int (256 * 256))))
    in
    (* Try again if we generated an invalid address: *)
    if self#is_a_valid_ipv6_address result then
      result
    else
      self#generate_ipv6_address

  method add_device ?port_row_completions device_name device_type port_no =
    let row_id =
      self#add_row
        [ "Name", String device_name;
          "Type", Icon device_type;
          "_uneditable", CheckBox true;
          "MTU", String "";
          "MAC address", String "";
          "IPv4 address", String "";
          "IPv4 netmask", String "";
          "IPv4 broadcast", String "";
          "IPv6 address", String "";
         ]
    in
    self#update_port_no ?port_row_completions device_name port_no;
    self#collapse_row row_id;

  method port_no_of ~device_name =
    self#children_no_of ~parent_name:device_name

  method private add_port ?port_row_completions device_name =
    let device_row_id = self#unique_row_id_of_name device_name in
    let current_port_no =
      self#port_no_of device_name in
    let port_type =
      match item_to_string (self#get_row_item device_row_id "Type") with
      | "machine" | "world_bridge" -> "machine-port"
      | "gateway" (* retro-compatibility *) -> "machine-port"
      | "router"             -> "router-port"
      | _                    -> "other-device-port" in
    let port_prefix =
      match item_to_string (self#get_row_item device_row_id "Type") with
        "machine" | "world_bridge" -> "eth"
      | "gateway" (* retro-compatibility *) -> "eth"
      | _ -> "port" in
    let port_name = (Printf.sprintf "%s%i" port_prefix current_port_no) in
    let port_row_standard =
      [ "Name", String port_name;
        "Type", Icon port_type; ] in
    let port_row = match port_row_completions with
      | None     -> port_row_standard
      | Some lst ->
         (try
           let port_row_specific_settings = (List.assoc port_name lst) in
           List.append (port_row_standard) (port_row_specific_settings)
          with Not_found -> port_row_standard)
    in
    ignore (self#add_row ~parent_row_id:device_row_id port_row)

  method update_port_no ?port_row_completions device_name new_port_no =
    let add_child_of = self#add_port ?port_row_completions in
    self#update_children_no ~add_child_of ~parent_name:device_name new_port_no

  (* To do: these validation methods suck. *)
  method private is_a_valid_mac_address address =
    try
      Scanf.sscanf
        address
        "%x:%x:%x:%x:%x:%x"
        (fun _ _ _ _ _ _ -> Scanf.sscanf address "%c%c:%c%c:%c%c:%c%c:%c%c:%c%c"
                                         (fun _ _ _ _ _ _ _ _ _ _ _ _ -> true))
    with _ ->
      false

  (* TODO: FIX IT: the validity depends on the ip and netmask (broadcast must belong the network addresses range). *)
  method private is_a_valid_ipv4_broadcast x =
(*    self#is_a_valid_ipv4_address x*)
   Ipv4.String.is_valid_ipv4 x

  method private is_a_valid_ipv6_address address =
    true
    (* This heuristic sucked *too* much. It's better to just accept everything. *)
    (*try
      Scanf.sscanf
        address
        "%x:%x:%x:%x:%x:%x:%x:%x"
        (fun o1 o2 o3 o4 o5 o6 o7 o8 ->
           o1 < 65536 && o2 < 65536 && o3 < 65536 && o4 < 65536 &&
           o5 < 65536 && o6 < 65536 && o7 < 65536 && o8 < 65536)
    with _ ->
      false *)
  method private is_a_valid_ipv6_netmask x =
    self#is_a_valid_ipv6_address x
  method private is_a_valid_ipv6_broadcast x =
    self#is_a_valid_ipv6_address x
  method private is_a_valid_mtu x =
    if x = "" then
      true
    else try
      (int_of_string x) >= 0 && (int_of_string x) < 65537
    with _ ->
      false

  method get_port_data device_name port_name =
    self#get_row_of_child ~parent_name:device_name ~child_name:port_name

  (** Return all the non-reserved data of a given port *index* (for example
      2 stands for "eth2" or "port2", in our usual <name, item> alist
      format: *)
  (* TODO: remove it *)
  method get_port_data_by_index device_name port_index =
    (* First try with the "eth" prefix: *)
    let port_name = Printf.sprintf "eth%i" port_index in
    try
      self#get_port_data device_name port_name
    with _ ->
      (* We failed. Ok, now try with the "port" prefix, before bailing out: *)
      let port_name = Printf.sprintf "port%i" port_index in
      self#get_port_data device_name port_name

  (** Return a single port attribute as an item: *)
  method get_port_attribute device_name port_name column_header =
    item_to_string (Assoc.find column_header (self#get_port_data device_name port_name))

  (** Return a single port attribute as an item: *)
  (* TODO: remove it and remove also get_port_data_by_index *)
  method get_port_attribute_by_index device_name port_index column_header =
    item_to_string (Assoc.find column_header (self#get_port_data_by_index device_name port_index))

  (** Update a single port attribute: *)
  method set_port_attribute_by_index device_name port_index column_header value =
    let device_row_id = self#unique_row_id_of_name device_name in
    let device_port_ids = self#children_of device_row_id in
    let port_name = Printf.sprintf "port%i" port_index in
    let filtered_port_data =
      List.filter
        (fun row -> Assoc.find "Name" row = String port_name)
        (List.map self#get_complete_row device_port_ids) in
    let current_row =
      match filtered_port_data with
        | [ complete_row ] -> complete_row
        | _ -> assert false (* either zero or more than one row matched *) in
    let row_id =
      match Assoc.find "_id" current_row with
        String row_id -> row_id
      | _ -> assert false in
    self#set_row_item row_id column_header value

  (** Update a single port attribute of type string: *)
  method set_port_string_attribute_by_index device_name port_index column_header value =
    self#set_port_attribute_by_index device_name port_index column_header (String value)

  (** Clear the interface and set the full internal state back to its initial value: *)
  method clear =
    super#clear;
    next_ipv4_address_as_int := 1;
    next_ipv6_address_as_int := Int64.one

  val counters_marshaler = new Oomarshal.marshaller

  method save ?with_forest_treatment () =
    (* Save the forest, as usual: *)
    super#save ?with_forest_treatment ();
    (* ...but also save the counters used for generating fresh addresses: *)
    let counters_file_name = (Option.extract filename#get)^"-counters" in
    (* For forward compatibility: *)
    let _OBSOLETE_mac_address_as_int = Random.int (256*256*256) in
    counters_marshaler#to_file
      (_OBSOLETE_mac_address_as_int, !next_ipv4_address_as_int, !next_ipv6_address_as_int)
      counters_file_name;

  method load =
    (* Load the forest, as usual: *)
    super#load;
    (* ...but also load the counters used for generating fresh addresses: *)
    let counters_file_name = (Option.extract filename#get)^"-counters" in
    (* _OBSOLETE_mac_address_as_int read for backward compatibility: *)
    let _OBSOLETE_mac_address_as_int, the_next_ipv4_address_as_int, the_next_ipv6_address_as_int =
      counters_marshaler#from_file counters_file_name in
    next_ipv4_address_as_int := the_next_ipv4_address_as_int;
    next_ipv6_address_as_int := the_next_ipv6_address_as_int

  initializer
    let _ =
      self#add_checkbox_column
        ~header:"_uneditable"
        ~hidden:true
        ~default:(fun () -> CheckBox false)
        () in
    let _ =
      self#add_icon_column
        ~header:"Type"
        ~shown_header:(s_ "Type")
        ~strings_and_pixbufs:[
           "machine", Initialization.Path.images^"treeview-icons/machine.xpm";
           "router",  Initialization.Path.images^"treeview-icons/router.xpm";
           "machine-port", Initialization.Path.images^"treeview-icons/network-card.xpm";
           "router-port",  Initialization.Path.images^"treeview-icons/port.xpm";
           "other-device-port", Initialization.Path.images^"treeview-icons/port.xpm";
            ]
        () in
    let _ =
      self#add_editable_string_column
        ~header:"MAC address"
        ~shown_header:(s_ "MAC address")
        ~default:(fun () -> String self#generate_mac_address)
        ~constraint_predicate:(fun i -> let s = item_to_string i in
                                          (self#is_a_valid_mac_address s) or s = "")
        () in
    let _ =
      self#add_editable_string_column
        ~header:"MTU"
        ~default:(fun () -> String "1500")
        ~constraint_predicate:(fun i -> let s = item_to_string i in
                                          (self#is_a_valid_mtu s) or s = "")
        () in
    let _ =
      self#add_editable_string_column
        ~header:"IPv4 address"
        ~shown_header:(s_ "IPv4 address")
        ~default:(fun () ->
                    if Global_options.get_autogenerate_ip_addresses () then
                      String self#generate_ipv4_address
                    else
                      String "")
        ~constraint_predicate:(fun i -> let s = item_to_string i in
                                          (Ipv4.String.is_valid_ipv4 s) or s = "")
        () in
    let _ =
      self#add_editable_string_column
        ~header:"IPv4 broadcast"
        ~shown_header:(s_ "IPv4 broadcast")
        ~default:(fun () ->
                    if Global_options.get_autogenerate_ip_addresses () then
                      String "10.10.255.255"
                    else
                      String "")
        ~constraint_predicate:(fun i -> let s = item_to_string i in
                                          (self#is_a_valid_ipv4_broadcast s) or s = "")
        () in
    let _ =
      self#add_editable_string_column
        ~header:"IPv4 netmask"
        ~shown_header:(s_ "IPv4 netmask")
        ~default:(fun () ->
                    if Global_options.get_autogenerate_ip_addresses () then
                      String "255.255.0.0"
                    else
                      String "")
        ~constraint_predicate:(fun i -> let s = item_to_string i in
                                          (Ipv4.String.is_valid_netmask s) or s = "")
        () in
    let _ =
      self#add_editable_string_column
        ~header:"IPv6 address"
        ~shown_header:(s_ "IPv6 address")
        ~default:(fun () ->
                    if Global_options.get_autogenerate_ip_addresses () then
                      String self#generate_ipv6_address
                    else
                      String "")
        ~constraint_predicate:(fun i -> let s = item_to_string i in
                                          (self#is_a_valid_ipv6_address s) or s = "")
        () in


  self#add_row_constraint
    ~name:(s_ "you should choose a port to define this parameter")
    (fun row ->
      let uneditable = item_to_bool (Assoc.find "_uneditable" row) in
      (not uneditable) or
      (List.for_all (fun (name, value) ->
                       name = "Name" or
                       name = "Type" or
                       name = "_uneditable" or
                       self#is_column_reserved name or
                       value = String "")
                    row));

  self#add_row_constraint
    ~name:(s_ "the router first port must always have a valid configuration address")
    (fun row ->
      let port_name = item_to_string (Assoc.find "Name" row) in
      let port_type = item_to_string (Assoc.find "Type" row) in
      let address   = item_to_string (Assoc.find "IPv4 address" row) in
      let netmask   = item_to_string (Assoc.find "IPv4 netmask" row) in
      (port_name <> "port0") or
      (port_type <> "router-port") or
      ((address <> "") && (netmask <> "")));

    (* In this treeview the involved device is the parent: *)
    self#set_after_update_callback
      (fun row_id ->
        after_user_edit_callback (self#parent_name_of row_id));

    (* Make internal data structures: no more columns can be added now: *)
    self#create_store_and_view;

    (* Setup the contextual menu: *)
    self#set_contextual_menu_title "Network interface's configuration";
end;;

(** Ugly kludge to make a single global instance visible from all modules
    linked *after* this one. Not having mutually-recursive inter-compilation-unit
    modules is a real pain. *)

class treeview = t
module The_unique_treeview = Stateful_modules.Variable (struct
  type t = treeview
  let name = Some "treeview_ifconfig"
  end)
let extract = The_unique_treeview.extract

let make ~packing ~after_user_edit_callback () =
  let result = new t ~packing ~after_user_edit_callback () in
  The_unique_treeview.set result;
  result
;;
