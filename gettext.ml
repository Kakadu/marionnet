(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2010  Jean-Vincent Loddo
   Copyright (C) 2010  Université Paris 13

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

let text_domain = "marionnet"
let file_dot_mo = (text_domain ^ ".mo")

(** A simple heuristics to infer the location of the `locale' directory
    in desperate situations, when both MARIONNET_LOCALEPREFIX and Meta.localeprefix
    do not contain a file `marionnet.mo' : *)
let try_to_infer_localeprefix_searching_marionnet_dot_mo_in_usr () =
  let (locale_dirs,_) =
    UnixExtra.find ~kind:'d' ~maxdepth:3 ~basename:"locale" ["/usr"]
  in
  UnixExtra.find_first_and_map ~kind:'f' ~maxdepth:3
    ~basename:file_dot_mo (fun d _ -> d) locale_dirs
;;

(** The method deciding the locale prefix: *)
let localeprefix =
  (* Try to search `marionnet.mo' in MARIONNET_PREFIX and Meta.localeprefix: *)
  let localeprefix_candidates =
    let l1 = Option.to_list (Configuration.get_string_variable "MARIONNET_LOCALEPREFIX") in
    let l2 = [Meta.localeprefix] in
    List.append l1 l2
  in
  let locale =
    UnixExtra.find_first_and_map ~kind:'f' ~maxdepth:3 ~basename:file_dot_mo
      (fun d _ -> d) localeprefix_candidates
  in
  match locale with
  | Some dir ->
      Log.printf "Gettext: `%s' found as expected in a candidate directory\n" file_dot_mo;
      dir
  | None     ->
      (* It's a desperate situation, but we try to find it ourself: *)
      (match try_to_infer_localeprefix_searching_marionnet_dot_mo_in_usr () with
       | Some dir ->
           Log.printf "Gettext: `%s' found in a /usr sub-directory\n" file_dot_mo;
           dir
       | None     ->
           Log.printf "Gettext: Warning: `%s' not found\n" file_dot_mo;
           List.hd localeprefix_candidates (* so much for that... *)
       )
;;

(** Build the module, now *)
include
  Gettext_builder.Make(struct
      let text_domain = text_domain
      let directory = localeprefix
     end);;

Log.printf "Gettext instanciated with directory `%s'\n" localeprefix;;
