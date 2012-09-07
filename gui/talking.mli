(* This file is part of marionnet
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

val check_pathname_validity : string -> string
val does_directory_support_sparse_files : string -> bool

module Msg :
  sig
    val help_repertoire_de_travail : unit -> unit
    val error_saving_while_something_up : unit -> unit
    val help_nom_pour_le_projet : unit -> unit
  end

val check_path_name_validity_and_add_extension_if_needed :
  ?extension:string ->
  string -> string

module EDialog :
  sig
    type edialog = unit -> string Environment.string_env option
    exception BadDialog of string * string
    exception StrangeDialog of string * string * string Environment.string_env
    exception IncompleteDialog

    val compose  : edialog list -> unit -> string Environment.string_env option
    val sequence : edialog list -> unit -> string Environment.string_env option

    val image_filter  : unit -> GFile.filter
    val all_files     : unit -> GFile.filter
    val script_filter : unit -> GFile.filter
    val mar_filter    : unit -> GFile.filter
    val xml_filter    : unit -> GFile.filter
    val jpeg_filter   : unit -> GFile.filter
    val png_filter    : unit -> GFile.filter
    val allfilters    : [> `ALL | `IMG | `JPEG | `MAR | `SCRIPT | `XML ] list

    val get_filter_by_name :
      [< `ALL
       | `DOT of Dot.output_format
       | `IMG
       | `JPEG
       | `MAR
       | `PNG
       | `SCRIPT
       | `XML ] ->
      GFile.filter

    val ask_for_file :
      ?enrich:string Environment.string_env ->
      ?title:string ->
      ?valid:(string -> bool) ->
      ?filter_names:[< `ALL
                     | `DOT of Dot.output_format
                     | `IMG
                     | `JPEG
                     | `MAR
                     | `PNG
                     | `SCRIPT
                     | `XML
                     > `ALL `IMG `JPEG `MAR `SCRIPT `XML ]
                    list ->
      ?filters:GFile.filter list ->
      ?extra_widget:GObj.widget * (unit -> string) ->
      ?action:GtkEnums.file_chooser_action ->
      ?gen_id:string ->
      ?help:(unit -> unit) option ->
      unit -> string Environment.string_env option

    val ask_for_existing_writable_folder_pathname_supporting_sparse_files :
      ?enrich:Shell.filexpr Environment.string_env ->
      ?help:(unit -> unit) option ->
      title:string -> unit -> Shell.filexpr Environment.string_env option

    val ask_for_fresh_writable_filename :
      ?enrich:string Environment.string_env ->
      title:string ->
      ?filters:GFile.filter list ->
      ?filter_names:[< `ALL
                     | `DOT of Dot.output_format
                     | `IMG
                     | `JPEG
                     | `MAR
                     | `PNG
                     | `SCRIPT
                     | `XML
                     > `ALL `IMG `JPEG `MAR `SCRIPT `XML ]
                    list ->
      ?extra_widget:GObj.widget * (unit -> string) ->
      ?help:(unit -> unit) option ->
      unit -> string Environment.string_env option

    val ask_for_existing_filename :
      ?enrich:Shell.filexpr Environment.string_env ->
      title:string ->
      ?filter_names:[< `ALL
                     | `DOT of Dot.output_format
                     | `IMG
                     | `JPEG
                     | `MAR
                     | `PNG
                     | `SCRIPT
                     | `XML
                     > `ALL `IMG `JPEG `MAR `SCRIPT `XML ]
                    list ->
      ?help:(unit -> unit) option ->
      unit -> string Environment.string_env option

    val ask_question :
      ?enrich:string Environment.string_env ->
      ?title:string ->
      ?gen_id:string ->
      ?help:(unit -> unit) option ->
      ?cancel:bool ->
      question:string -> unit -> string Environment.string_env option
  end
