(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2009  Jean-Vincent Loddo

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


(** Gui completion for the dialog_A_PROPOS widget defined with glade. *)

(* Shortcuts *)
module Str = StrExtra.Str
let mkenv = Environment.make_string_env

module Make (State:sig val st:State.globalState end) = struct

  open State

  (* User handler for dialog completion. *)
  let dialog () =

   let d = new Gui.dialog_A_PROPOS () in
   d#toplevel#set_title "À propos";

   (* Labels *)
   let () = begin
    let set label text =
      label#set_use_markup true;
      label#set_label text
    in

    set d#label_dialog_A_PROPOS_a_propos "À propos";
    set d#label_dialog_A_PROPOS_a_propos_content "<b>Marionnet</b> est un environnement de simulation de réseaux composés de machines GNU/Linux. Ce logiciel permet, d'une part, aux étudiants de s'entrainer à la mise en place (cablage et configuration) d'un réseaux et, d'autre part, aux enseignants de définir leurs énoncés de TP et/ou contrôles réseaux.\n\nCe logiciel est basé sur les fonctionnalités UML (User Mode Linux) du noyau Linux.\n
<tt><u><span color=\"blue\">http://www.marionnet.org</span></u></tt>\n";

    set d#label_dialog_A_PROPOS_authors "Auteurs";
    set d#label_dialog_A_PROPOS_authors_content "
Jean-Vincent Loddo <tt><u><span color=\"blue\">&lt;loddo@lipn.univ-paris13.fr&gt;</span></u></tt>
Département R&amp;T - IUT de Villetaneuse
Laboratoire d'Informatique de Paris Nord (LIPN)
Université Paris 13\n
Luca Saiu <tt><u><span color=\"blue\">&lt;saiu@lipn.univ-paris13.fr&gt;</span></u></tt>
Laboratoire d'Informatique de Paris Nord (LIPN)
Université Paris 13\n\n";

    set d#label_dialog_A_PROPOS_license "Licence";
    set d#label_dialog_A_PROPOS_license_content "
Copyright (C) 2007, 2008  Jean-Vincent Loddo;
Copyright (C) 2007, 2008  Luca Saiu\n
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
