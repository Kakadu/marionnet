(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007, 2008, 2009  Luca Saiu
   Copyright (C) 2009  Marco Stronati
   Copyright (C) 2010  Jean-Vincent Loddo
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

(* span foreground="red" *)

let text_title =
  Printf.sprintf
    "<small><b>%s</b></small>"
    (s_ "Marionnet, a virtual network laboratory")
;;

let text_subtitle =
"<small><i>Version " ^ Version.version ^ "</i> revno " ^ Meta.revision ^ " - " ^ Meta.source_date ^ "</small>"
;;

let text =
"<small>Copyright (C) 2007, 2008, 2009, 2010 Jean-Vincent Loddo
Copyright (C) 2007, 2008, 2009, 2010 Luca Saiu
Copyright (C) 2007, 2008, 2009, 2010 Université Paris 13

<i>Marionnet comes with <b>absolutely no warranty</b>.
This is free software, covered by the GNU GPL.
You are welcome to redistribute it under certain
conditions; see the file `COPYING' for details.</i></small>";;

let handle_click window _ =
  Log.printf "handle_click: the splash screen was closed\n";
  window#misc#hide ();
  window#destroy ();
  true;;

let splash_image =
  GDraw.pixmap_from_xpm
    ~file:(Initialization.Path.images^"splash.300x348.xpm")
    ();;

let splash =
  GWindow.window
    ~resizable:false
    ~border_width:10
    ~position:`CENTER
    ~type_hint:`DIALOG
    ~modal:true
    ~wm_name:"Marionnet splash screen"
    ~icon:Icon.icon_pixbuf
    ();;

splash#set_title (s_ "Welcome to Marionnet");;
let event_box = GBin.event_box ~packing:splash#add () in
let box = GPack.vbox ~spacing:5 ~border_width:2 ~packing:event_box#add () in
let _image = GMisc.pixmap splash_image ~packing:(box#pack ~padding:3) () in
let _title =
  let align = GBin.alignment ~xalign:1. ~packing:box#add () in
  let table = GPack.table ~rows:2 ~columns:1 ~row_spacings:0 ~homogeneous:false ~packing:(align#add) () in
  let attach = table#attach ~expand:`X ~fill:`BOTH ~left:0 in
  let _ = GMisc.label ~markup:text_title ~packing:(attach ~top:0) ~xalign:0.5 ~line_wrap:false () in
  let _ = GMisc.label ~markup:text_subtitle ~packing:(attach ~top:1) ~xalign:0.5 ~line_wrap:false () in
  ()
in
let _ = GMisc.label ~markup:text ~packing:box#add ~line_wrap:false () in
let table =
  GPack.table ~rows:1 ~columns:3 ~col_spacings:20
    ~homogeneous:true
    ~packing:box#add ()
in
let _logo_paris13 =
 GMisc.image
   ~file:(Initialization.Path.images^"logo.paris13.96x96.png")
   ~xalign:0.5 ~packing:(table#attach ~left:0 ~top:0) ()
in
let _logo_iutv =
 GMisc.image
   ~file:(Initialization.Path.images^"logo.iutv.96x96.png")
   ~xalign:0.5 ~packing:(table#attach ~left:1 ~top:0) ()
in
let _logo_lipn =
 GMisc.image
   ~file:(Initialization.Path.images^"logo.lipn.96x96.png")
   ~xalign:0.5 ~packing:(table#attach ~left:2 ~top:0) ()
in
let _ = event_box#event#connect#button_press ~callback:(handle_click splash) in
let _ = splash#event#connect#key_press ~callback:(fun ev -> handle_click splash ()) in ()
;;

let show_splash ?timeout () =
  (match timeout with
    Some timeout ->
      ignore
        (GMain.Timeout.add
           ~ms:timeout
           ~callback:(fun () -> ignore (handle_click splash ()); false))
  | None ->
      ());
  splash#show ();;
