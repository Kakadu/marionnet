(* This file is part of marionnet
   Copyright (C) 2010 Jean-Vincent Loddo

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


(* This function may be useful for testing the widget creation without
   recompiling the whole project. *)
let make
 ?(title="Add a world gateway")
 ?(s_=fun x->x) (* Gettext support. Not useful when testing. *)
 ?(name="")
 ?label
 ?(network_config:Ipv4.config option)
 ?(dhcp_enabled=true)
 ?(user_port_no=4)
 ?help_callback
 ?(ok_callback=(fun data -> Some data))
 ?(dialog_image_file=Initialization.Path.images^"ico.world_gateway.dialog.png")
 () :'result option =
  let oldname = name in
  let ((b1,b2,b3,b4),b5) = match network_config with
   | Some x -> x
   | None   -> ((10,0,2,1),24)
  in
  let icon =
    let icon_file = Initialization.Path.images^"marionnet-launcher.png" in
    GdkPixbuf.from_file icon_file
  in
  let w = GWindow.dialog ~icon ~title ~modal:true ~position:`CENTER () in
  let tooltips = Gui_bricks.make_tooltips_for_container w in

  let (name,label) =
    let hbox = GPack.hbox ~homogeneous:true ~border_width:20 ~spacing:10 ~packing:w#vbox#add () in
    let image = GMisc.image ~file:dialog_image_file ~xalign:0.5 ~packing:hbox#add () in
    tooltips image#coerce (s_ "Gateway");
    let vbox = GPack.vbox ~spacing:10 ~packing:hbox#add () in
    let name  = Gui_bricks.entry_with_label ~packing:vbox#add ~entry_text:name  (s_ "Name") in
    let label = Gui_bricks.entry_with_label ~packing:vbox#add ?entry_text:label (s_ "Label") in
    tooltips name#coerce (s_ "Gateway name. This name must be unique in the virtual network. Suggested: G1, G2, ...");
    tooltips label#coerce (s_ "Label to be written in the network sketch, next to the element icon." );
    (name,label)
  in

  ignore (GMisc.separator `HORIZONTAL ~packing:w#vbox#add ());

  let ((s1,s2,s3,s4,s5), dhcp_enabled, user_port_no) =
    let vbox = GPack.vbox ~homogeneous:false ~border_width:20 ~spacing:10 ~packing:w#vbox#add () in
    let form =
      Gui_bricks.make_form_with_labels
        ~packing:vbox#add
        [(s_ "IPv4 address"); (s_ "DHCP service"); (s_ "Integrated switch ports")]
    in
    let network_config =
      Gui_bricks.spin_ipv4_address_with_cidr_netmask
        ~packing:form#add
        b1 b2 b3 b4 b5
    in
    let dhcp_enabled =
      GButton.check_button
        ~active:dhcp_enabled
        ~packing:form#add ()
    in
    let user_port_no =
      Gui_bricks.spin_byte ~lower:2 ~upper:16 ~step_incr:2 ~packing:form#add user_port_no
    in
    tooltips form#coerce (s_ "IPv4 configuration and required services" );
    tooltips dhcp_enabled#coerce (s_ "The gateway should provide a DHCP service?" );
    (network_config, dhcp_enabled, user_port_no)
  in
  tooltips s1#coerce (s_ "First octet of the IPv4 address" );
  tooltips s2#coerce (s_ "Second octet of the IPv4 address" );
  tooltips s3#coerce (s_ "Third octet of the IPv4 address" );
  tooltips s4#coerce (s_ "Fourth octet of the IPv4 address" );
  tooltips s5#coerce (s_ "Netmask (CIDR notation)" );
  tooltips user_port_no#coerce (s_ "The number of ports of the integrated switch" );
  s4#misc#set_sensitive false;
  s5#misc#set_sensitive false;

  let get_widget_data () :'result =
    let name = name#text in
    let label = label#text in
    let network_config =
      let s1 = int_of_float s1#value in
      let s2 = int_of_float s2#value in
      let s3 = int_of_float s3#value in
      let s4 = int_of_float s4#value in
      let s5 = int_of_float s5#value in
      ((s1,s2,s3,s4),s5)
    in
    let dhcp_enabled = dhcp_enabled#active in
    let user_port_no = int_of_float user_port_no#value in
    (`name name,
     `label label,
     `network_config network_config,
     `dhcp_enabled dhcp_enabled,
     `user_port_no user_port_no,
     `oldname oldname)
  in
  (* The result of make is the result of the dialog loop (of type 'result option): *)
  Gui_bricks.dialog_loop w ~ok_callback ?help_callback ~get_widget_data ()
