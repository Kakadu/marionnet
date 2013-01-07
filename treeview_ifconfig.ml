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
module Row_item = Treeview.Row_item ;;
module Row = Treeview.Row ;;

type port_row_completions = (string * (string * Row_item.t) list) list

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

  val uneditable_header = "_uneditable"
  method get_row_uneditable = self#get_CheckBox_field (uneditable_header)

  val type_header = "Type"
  method get_row_type = self#get_Icon_field (type_header)
  method set_row_type = self#set_Icon_field (type_header)

  val mac_address_header = "MAC address"
  method get_row_mac_address = self#get_String_field (mac_address_header)
  method set_row_mac_address = self#set_String_field (mac_address_header)

  val mtu_header = "MTU"
  method get_row_mtu = self#get_String_field (mtu_header)
  method set_row_mtu = self#set_String_field (mtu_header)

  val ipv4_address_header = "IPv4 address"
  method get_row_ipv4_address = self#get_String_field (ipv4_address_header)
  method set_row_ipv4_address = self#set_String_field (ipv4_address_header)

  val ipv4_broadcast_header = "IPv4 broadcast"
  method get_row_ipv4_broadcast = self#get_String_field (ipv4_broadcast_header)
  method set_row_ipv4_broadcast = self#set_String_field (ipv4_broadcast_header)

  val ipv4_netmask_header = "IPv4 netmask"
  method get_row_ipv4_netmask = self#get_String_field (ipv4_netmask_header)
  method set_row_ipv4_netmask = self#set_String_field (ipv4_netmask_header)

  val ipv6_address_header = "IPv6 address"
  method get_row_ipv6_address = self#get_String_field (ipv6_address_header)
  method set_row_ipv6_address = self#set_String_field (ipv6_address_header)

  method private currently_used_mac_addresses : string list =
    let xs = List.flatten (Forest.to_list self#get_forest) in
    let xs = ListExtra.filter_map
      (function
       | header, (Row_item.String s) when header=mac_address_header -> Some s
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
        [ name_header,           Row_item.String device_name;
          type_header,           Row_item.Icon device_type;
          uneditable_header,     Row_item.CheckBox true;
          mtu_header,            Row_item.String "";
          mac_address_header,    Row_item.String "";
          ipv4_address_header,   Row_item.String "";
          ipv4_netmask_header,   Row_item.String "";
          ipv4_broadcast_header, Row_item.String "";
          ipv6_address_header,   Row_item.String "";
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
      match self#get_row_type (device_row_id) with
      | "machine" | "world_bridge" -> "machine-port"
      | "gateway" (* retro-compatibility *) -> "machine-port"
      | "router"             -> "router-port"
      | _                    -> "other-device-port" in
    let port_prefix =
      match self#get_row_type (device_row_id) with
        "machine" | "world_bridge" -> "eth"
      | "gateway" (* retro-compatibility *) -> "eth"
      | _ -> "port"
    in
    let port_name = (Printf.sprintf "%s%i" port_prefix current_port_no) in
    let port_row_standard =
      [ name_header, Row_item.String port_name;
        type_header, Row_item.Icon port_type; ]
    in
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
  method private is_a_valid_ipv6_netmask   x = self#is_a_valid_ipv6_address x
  method private is_a_valid_ipv6_broadcast x = self#is_a_valid_ipv6_address x

  method private is_a_valid_mtu x =
    if x = "" then
      true
    else try
      (int_of_string x) >= 0 && (int_of_string x) < 65537
    with _ ->
      false

  method get_port_data ~device_name ~port_name =
    self#get_row_of_child ~parent_name:device_name ~child_name:port_name

  (** Return all the non-reserved data of a given port *index* (for example
      2 stands for "eth2" or "port2", in our usual <name, item> alist
      format: *)
  (* TODO: remove it *)
  method get_port_data_by_index ~device_name ~port_index =
    (* First try with the "eth" prefix: *)
    let port_name = Printf.sprintf "eth%i" port_index in
    try
      self#get_port_data device_name port_name
    with _ ->
      (* We failed. Ok, now try with the "port" prefix, before bailing out: *)
      let port_name = Printf.sprintf "port%i" port_index in
      self#get_port_data ~device_name ~port_name

  (** Return a single port attribute as an item: *)

  method get_port_attribute ~device_name ~port_name ~field =
    let row = (self#get_port_data ~device_name ~port_name) in
    (Row.String_field.get ~field row)

  (** Return a single port attribute as an item: *)
  (* TODO: remove it and remove also get_port_data_by_index *)
  method get_port_attribute_by_index ~device_name ~port_index ~field =
    let row = (self#get_port_data_by_index ~device_name ~port_index) in
    (Row.String_field.get ~field row)

  (** Update a single port attribute: *)
  method set_port_attribute_by_index ~device_name ~port_index ~field value =
    let port_name = Printf.sprintf "port%i" port_index in
    let row =
      self#get_complete_row_of_child
        ~parent_name:device_name
        ~child_name:port_name
    in
    let row_id = Row.get_id row in
    self#set_row_field row_id field value;

  (** Update a single port attribute of type string: *)
  method set_port_string_attribute_by_index ~device_name ~port_index ~field value =
    self#set_port_attribute_by_index ~device_name ~port_index ~field (Row_item.String value)

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
        ~header:uneditable_header
        ~hidden:true
        ~default:(fun () -> Row_item.CheckBox false)
        () in
    let _ =
      self#add_icon_column
        ~header:type_header
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
        ~header:mac_address_header
        ~shown_header:(s_ "MAC address")
        ~default:(fun () -> Row_item.String self#generate_mac_address)
        ~constraint_predicate:(fun i -> let s = Row_item.extract_String i in
                                          (self#is_a_valid_mac_address s) or s = "")
        () in
    let _ =
      self#add_editable_string_column
        ~header:mtu_header
        ~default:(fun () -> Row_item.String "1500")
        ~constraint_predicate:(fun i -> let s = Row_item.extract_String i in
                                          (self#is_a_valid_mtu s) or s = "")
        () in
    let _ =
      self#add_editable_string_column
        ~header:ipv4_address_header
        ~shown_header:(s_ "IPv4 address")
        ~default:(fun () ->
                    if Global_options.get_autogenerate_ip_addresses () then
                      Row_item.String self#generate_ipv4_address
                    else
                      Row_item.String "")
        ~constraint_predicate:(fun i -> let s = Row_item.extract_String i in
                                          (Ipv4.String.is_valid_ipv4 s) or s = "")
        () in
    let _ =
      self#add_editable_string_column
        ~header:ipv4_broadcast_header
        ~shown_header:(s_ "IPv4 broadcast")
        ~default:(fun () ->
                    if Global_options.get_autogenerate_ip_addresses () then
                      Row_item.String "10.10.255.255"
                    else
                      Row_item.String "")
        ~constraint_predicate:(fun i -> let s = Row_item.extract_String i in
                                          (self#is_a_valid_ipv4_broadcast s) or s = "")
        () in
    let _ =
      self#add_editable_string_column
        ~header:ipv4_netmask_header
        ~shown_header:(s_ "IPv4 netmask")
        ~default:(fun () ->
                    if Global_options.get_autogenerate_ip_addresses () then
                      Row_item.String "255.255.0.0"
                    else
                      Row_item.String "")
        ~constraint_predicate:(fun i -> let s = Row_item.extract_String i in
                                          (Ipv4.String.is_valid_netmask s) or s = "")
        () in
    let _ =
      self#add_editable_string_column
        ~header:ipv6_address_header
        ~shown_header:(s_ "IPv6 address")
        ~default:(fun () ->
                    if Global_options.get_autogenerate_ip_addresses () then
                      Row_item.String self#generate_ipv6_address
                    else
                      Row_item.String "")
        ~constraint_predicate:(fun i -> let s = Row_item.extract_String i in
                                          (self#is_a_valid_ipv6_address s) or s = "")
        () in


  self#add_row_constraint
    ~name:(s_ "you should choose a port to define this parameter")
    (fun row ->
      let uneditable = Row.CheckBox_field.get ~field:uneditable_header row in
      (not uneditable) or
      (List.for_all (fun (name, value) ->
                       name = name_header or
                       name = type_header or
                       name = uneditable_header or
                       self#is_column_reserved name or
                       value = Row_item.String "")
                    row));

  self#add_row_constraint
    ~name:(s_ "the router first port must always have a valid configuration address")
    (fun row ->
      let port_name = (Row.get_name row) in
      let port_type = (Row.Icon_field.get ~field:type_header row) in
      let address   = (Row.String_field.get ~field:ipv4_address_header row) in
      let netmask   = (Row.String_field.get ~field:ipv4_netmask_header row) in
      (port_name <> "port0") or
      (port_type <> "router-port") or
      ((address <> "") && (netmask <> "")));

    (* In this treeview the involved device is the parent: *)
    self#set_after_update_callback
      (fun row_id ->
        after_user_edit_callback (self#get_row_parent_name row_id));

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

let make ~(window:GWindow.window) ~(hbox:GPack.box) ~after_user_edit_callback () =
  let result = new t ~packing:(hbox#add) ~after_user_edit_callback () in
  let () = Treeview.add_expand_and_collapse_button ~window ~hbox (result:>Treeview.t) in
  The_unique_treeview.set result;
  result
;;
