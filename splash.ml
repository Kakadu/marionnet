(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007  Luca Saiu
   Updated in 2009 by Luca Saiu

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

let splashscreen_text =
"<i>Marionnet " ^ Version.version ^ "</i>, " ^ Version.build_time ^ ".

Copyright (C) 2007, 2008, 2009 Jean-Vincent Loddo
Copyright (C) 2007, 2008, 2009 Luca Saiu

Sponsored by Université Paris 13

<span style=\"italic\">Marionnet comes with <b>absolutely no warranty</b>.
This is free software, covered by the GNU GPL.
You are welcome to redistribute it under certain
conditions; see the file `COPYING' for details.</span>";;

let handle_click window _ =
  Log.printf "handle_click: the splash screen was closed\n"; flush_all ();
  window#misc#hide ();
  window#destroy ();
  true;;

let splash_image =
  GDraw.pixmap_from_xpm
    ~file:(Initialization.marionnet_home_images^"splash.xpm")
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
let box = GPack.vbox ~border_width:2 ~packing:event_box#add () in
let _ = GMisc.pixmap splash_image ~packing:(box#pack ~padding:3) () in
let _ = GMisc.label ~markup:splashscreen_text ~packing:box#add ~line_wrap:false () in
let _ = event_box#event#connect#button_press ~callback:(handle_click splash) in
();;

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
