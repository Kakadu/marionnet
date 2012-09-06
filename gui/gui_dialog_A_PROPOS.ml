(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007, 2009, 2010  Jean-Vincent Loddo
   Copyright (C) 2007  Luca Saiu
   Copyright (C) 2007, 2009, 2010  Université Paris 13

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

(** Gui completion for the dialog_A_PROPOS widget defined with glade. *)

(* Shortcuts *)
let mkenv = Environment.make_string_env

module Make (State:sig val st:State.globalState end) = struct

  open State

  (* User handler for dialog completion. *)
  let dialog () =

   let d = new Gui.dialog_A_PROPOS () in
   d#toplevel#set_title (s_ "About");

   (* Labels *)
   let () = begin
    let set label text =
      label#set_use_markup true;
      label#set_label text
    in

   set d#label_dialog_A_PROPOS_a_propos (s_ "About");

   let text_title =
     Printf.sprintf "<b>%s</b>" (s_ "Marionnet, a virtual network laboratory")
   in
   let text_subtitle =
     Printf.sprintf "<i>Version %s </i> revno %s - %s" Version.version Meta.revision Meta.source_date
   in
   let title = Printf.sprintf "\n%s\n<small>%s</small>\n" text_title text_subtitle in
   set d#label_dialog_A_PROPOS_title title;

   set d#label_dialog_A_PROPOS_a_propos_content (s_ "<b>Marionnet</b> is an environment for the simulation of a network composed of GNU/Linux machines. This software was thought for students to experiment with bulding and configuring networks, and for teachers to prepare excercises and tests.\n\nMarionnet is based on the UML features of the Linux kernel.\n<tt><u><span color=\"blue\">http://www.marionnet.org</span></u></tt>\n");

   set d#label_dialog_A_PROPOS_authors (s_ "Authors");
   set d#label_dialog_A_PROPOS_authors_content "
Jean-Vincent Loddo <tt><u><span color=\"blue\">&lt;loddo@lipn.univ-paris13.fr&gt;</span></u></tt>
Département R&amp;T - IUT de Villetaneuse
Laboratoire d'Informatique de Paris Nord (LIPN)
Université Paris 13\n
Luca Saiu <tt><u><span color=\"blue\">&lt;saiu@lipn.univ-paris13.fr&gt;</span></u></tt>
Laboratoire d'Informatique de Paris Nord (LIPN)
Université Paris 13\n\n";

   set d#label_dialog_A_PROPOS_license (s_ "License");
   set d#label_dialog_A_PROPOS_license_content "
Copyright (C) 2007, 2008, 2009, 2010  Jean-Vincent Loddo
Copyright (C) 2007, 2008, 2009, 2010  Luca Saiu
Copyright (C) 2007, 2008, 2009, 2010  Université Paris 13\n
<i>Marionnet is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 2 of the License, or (at your option) any later version.</i>\n
This program is distributed in the hope that it will be useful, but <b>WITHOUT ANY WARRANTY</b>; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.\n
You should have received a copy of the GNU General Public License along with this program.  If not, see
<tt><u><span color=\"blue\">http://www.gnu.org/licenses/</span></u></tt>.\n\n";

   set d#label_dialog_A_PROPOS_thanks "Thanks";
   set d#label_dialog_A_PROPOS_thanks_content "We wish to thank Jeff Dike and the other authors of UML for their nice work, which made Marionnet possible; Renzo Davoli for VDE, the powerful communication infrastructure that we used and modified; the authors of OCaml for their nice language; and of course the whole free software community, of which the GNU and Linux projects remain the foremost contributors.\n
This beautiful logo was designed by Silviu Barsanu:\n<tt><u><span color=\"blue\">http://www.silviubarsanu.evonet.ro</span></u></tt>";
   set d#label_dialog_A_PROPOS_thanks_sponsors "Marionnet is sponsored as an\ne-learning project since 2007 by";
   end

   in d

end
