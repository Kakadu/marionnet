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

open Treeview;;
open Initialization;;
open ListExtra;;
open Sugar;;
open Row_item;;
open Gettext;;

(* To do: move this into EXTRA/ *)
let rec take n xs =
  if n = 0 then
    []
  else
    match xs with
      [] -> failwith "take: n is greater than the list length"
    | x :: rest -> x :: (take (n - 1) rest);;

class network_details_interface =
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
  
  (** The three leftmost octects are used as the trailing part of
      automatically-generated MAC addresses.
      Interesting side note: we can't use four because of OCaml
      runtime type tagging (yes, Jean: I was also surprised when I
      discovered it, but it was made that way to support precise GC,
      which can't rely on conservative pointer finding). *)
  val next_mac_address_as_int =
    ref 1
  method private generate_mac_address =
    let mac_address_as_int = !next_mac_address_as_int in
    next_mac_address_as_int := mac_address_as_int + 1;
    let result =
      Printf.sprintf
        "02:04:06:%02x:%02x:%02x"
        (mac_address_as_int / (256 * 256))
        (mac_address_as_int / 256)
        (mac_address_as_int mod 256) in
    (* Try again if we generated an invalid address: *)
    if self#is_a_valid_mac_address result then
      result
    else
      self#generate_mac_address

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

  method add_device ?port_row_completions device_name device_type ports_no =
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
(*          "IPv6 netmask", String "";
          "IPv6 broadcast", String ""; *) ] in
    self#update_ports_no ?port_row_completions device_name ports_no;
    self#collapse_row row_id;
    self#save;

  (** Do nothing if there is no such device. *)
  method remove_device device_name =
    try
      let device_row_id =
        self#device_row_id device_name in
      self#remove_subtree device_row_id;
      self#save;
    with _ ->
      ()

  method private device_row_id device_name =
    let result = 
    self#row_id_such_that (fun row -> (lookup_alist "Name" row) = String device_name) in
    result

  method ports_no_of device_name =
    let device_row_id =
      self#device_row_id device_name in
    let port_row_ids =
      Forest.children_nodes device_row_id !id_forest in
    List.length port_row_ids

  method private add_port ?port_row_completions device_name =
    let device_row_id =
      self#device_row_id device_name in
    let current_ports_no = 
      self#ports_no_of device_name in
    let port_type =
      match item_to_string (self#get_row_item device_row_id "Type") with
        "machine" | "gateway" -> "machine-port"
      |  "router"             -> "router-port"
      | _ -> "other-device-port" in
    let port_prefix =
      match item_to_string (self#get_row_item device_row_id "Type") with
        "machine" | "gateway" -> "eth"
      | _ -> "port" in
    let port_name = (Printf.sprintf "%s%i" port_prefix current_ports_no) in
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

  method update_ports_no ?port_row_completions device_name new_ports_no =
    let device_row_id =
      self#device_row_id device_name in
    let port_row_ids =
      Forest.children_nodes device_row_id !id_forest in
    let old_ports_no =
      self#ports_no_of device_name in
    let ports_delta = new_ports_no - old_ports_no in
    if ports_delta >= 0 then
      for i = old_ports_no + 1 to new_ports_no do
        self#add_port ?port_row_completions device_name
      done
    else begin
      let reversed_port_row_ids = List.rev port_row_ids in
      List.iter self#remove_row (take (- ports_delta) reversed_port_row_ids);
    end;
    self#save;

  method rename_device old_name new_name =
    let device_row_id =
      self#device_row_id old_name in
    self#set_row_item device_row_id "Name" (String new_name);
    self#save;

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

  (* FIX IT: the validity depends on the ip and netmask (broadcast must belong the network addresses range). *)
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

  (** Return all the non-reserved data of a given port, in our usual
      <name, item> alist format: *)
  method get_port_data device_name port_name =
(*     Log.printf "network_details: get_port_data\n"; flush_all (); *)
    let device_row_id = self#device_row_id device_name in
    let device_port_ids = self#children_of device_row_id in
    let filtered_port_data =
      List.filter
        (fun row -> lookup_alist "Name" row = String port_name)
        (List.map self#get_row device_port_ids) in
(*     Log.printf "get_port_data: %s %s (length is %i)\n" device_name port_name (List.length filtered_port_data); flush_all (); *)
    assert((List.length filtered_port_data) = 1);
    List.hd filtered_port_data

  (** Return all the non-reserved data of a given port *index* (for example
      2 stands for "eth2" or "port2", in our usual <name, item> alist
      format: *)
  method get_port_data_by_index device_name port_index =
    (* First try with the "eth" prefix: *)
    let port_name = Printf.sprintf "eth%i" port_index in
    try
      self#get_port_data device_name port_name
    with _ -> 
      (* We failed. Ok, now try with the "port" prefix, before bailing
         out: *)
      let port_name = Printf.sprintf "port%i" port_index in
      self#get_port_data device_name port_name

  (** Return a single port attribute as an item: *)
  method get_port_attribute device_name port_name column_header =
(*     Log.printf "network_details: get_port_attribute\n"; flush_all (); *)
    item_to_string (lookup_alist column_header (self#get_port_data device_name port_name))

  (** Return a single port attribute as an item: *)
  method get_port_attribute_by_index device_name port_index column_header =
(*     Log.printf "network_details: get_port_attribute_by_index\n"; flush_all (); *)
    item_to_string (lookup_alist column_header (self#get_port_data_by_index device_name port_index))
  
  (** Update a single port attribute: *)
  method set_port_attribute_by_index device_name port_index column_header value =
    let device_row_id = self#device_row_id device_name in
    let device_port_ids = self#children_of device_row_id in
    let port_name = Printf.sprintf "port%i" port_index in
    let filtered_port_data =
      List.filter
        (fun row -> lookup_alist "Name" row = String port_name)
        (List.map self#get_complete_row device_port_ids) in
    let current_row =
      match filtered_port_data with
        | [ complete_row ] -> complete_row
        | _ -> assert false (* either zero or more than one row matched *) in
    let row_id =
      match lookup_alist "_id" current_row with
        String row_id -> row_id
      | _ -> assert false in
    self#set_row_item row_id column_header value

  (** Update a single port attribute of type string: *)
  method set_port_string_attribute_by_index device_name port_index column_header value =
    self#set_port_attribute_by_index device_name port_index column_header (String value)
    
  (** Clear the interface and set the full internal state back to its initial value: *)
  method reset =
    self#clear;
    next_mac_address_as_int := 1;
    next_ipv4_address_as_int := 1;
    next_ipv6_address_as_int := Int64.one

  val counters_marshaler = new Oomarshal.marshaller
  
  method save =
    (* Save the forest, as usual: *)
    super#save;
    (* ...but also save the counters used for generating fresh addresses: *)
    let counters_file_name = super#file_name ^ "-counters" in
    counters_marshaler#to_file
      (!next_mac_address_as_int, !next_ipv4_address_as_int, !next_ipv6_address_as_int)
      counters_file_name;
    
  method load =
    (* Load the forest, as usual: *)
    super#load;
    (* ...but also load the counters used for generating fresh addresses: *)
    let counters_file_name = super#file_name ^ "-counters" in
    let the_next_mac_address_as_int, the_next_ipv4_address_as_int, the_next_ipv6_address_as_int =
      counters_marshaler#from_file counters_file_name in
    next_mac_address_as_int := the_next_mac_address_as_int;
    next_ipv4_address_as_int := the_next_ipv4_address_as_int;
    next_ipv6_address_as_int := the_next_ipv6_address_as_int    

  method private relevant_device_name_for_row_id row_id =
    let id_forest = self#get_id_forest in
    let devices_row_id =
      Forest.nodes_such_that
        (fun a_row_id ->
          List.mem
            row_id
            (try Forest.children_nodes a_row_id id_forest with _ -> []))
        id_forest in
    assert (List.length devices_row_id = 1);
    let device_row_id = List.hd devices_row_id in
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
        ~strings_and_pixbufs:[ "machine", marionnet_home_images^"treeview-icons/machine.xpm";
                               "router", marionnet_home_images^"treeview-icons/router.xpm";
(*                                "cloud", marionnet_home_images^"treeview-icons/cloud.xpm"; *)
(*                                "gateway", marionnet_home_images^"treeview-icons/gateway.xpm"; *)
                               "machine-port", marionnet_home_images^"treeview-icons/network-card.xpm";
                               "router-port",       marionnet_home_images^"treeview-icons/port.xpm";
                               "other-device-port", marionnet_home_images^"treeview-icons/port.xpm";
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
      let uneditable = item_to_bool (lookup_alist "_uneditable" row) in
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
      let port_name = item_to_string (lookup_alist "Name" row) in
      let port_type = item_to_string (lookup_alist "Type" row) in
      let address   = item_to_string (lookup_alist "IPv4 address" row) in
      let netmask   = item_to_string (lookup_alist "IPv4 netmask" row) in
      (port_name <> "port0") or
      (port_type <> "router-port") or
      ((address <> "") && (netmask <> "")));

    self#set_after_update_callback
      (fun row_id ->
        after_user_edit_callback (self#relevant_device_name_for_row_id row_id));

    (* Make internal data structures: no more columns can be added now: *)
    self#create_store_and_view;

    (* Setup the contextual menu: *)
    self#set_contextual_menu_title "Network details operations";
end;;

(** Ugly kludge to make a single global instance visible from all modules
    linked *after* this one. Not having mutually-recursive inter-compilation-unit
    modules is a real pain. *)
let the_network_details_interface =
  ref None;;
let get_network_details_interface () =
  match !the_network_details_interface with
    None -> failwith "No network details interface exists"
  | Some the_network_details_interface -> the_network_details_interface;;
let make_network_details_interface ~packing ~after_user_edit_callback () =
  let result = new network_details_interface ~packing ~after_user_edit_callback () in
  the_network_details_interface := Some result;
  result;;
