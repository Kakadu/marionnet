(* val mkenv : (string * 'a) list -> 'a Environment.string_env *)

module Msg :
  sig
    val help_repertoire_de_travail : unit -> unit
    val help_machine_insert_update : unit -> unit
    val help_hub_insert_update     : unit -> unit
    val help_switch_insert_update  : unit -> unit
    val help_router_insert_update  : unit -> unit
    val help_device_insert_update  : Mariokit.Netmodel.devkind   -> unit -> unit
    val help_cable_insert_update   : Mariokit.Netmodel.cablekind -> unit -> unit
    val help_cable_direct_insert_update    : unit -> unit
    val help_cable_crossover_insert_update : unit -> unit
    val help_cloud_insert_update  : unit -> unit
    val help_world_bridge_insert_update : unit -> unit
    val error_saving_while_something_up : unit -> unit
    val help_nom_pour_le_projet : unit -> unit
  end

val check_pathname_validity : string -> string
val check_path_name_validity_and_add_extension_if_needed : ?extension:string -> string -> string

module EDialog :
  sig
    type edialog = unit -> string Environment.string_env option

    exception BadDialog of string * string
    exception StrangeDialog of string * string * string Environment.string_env
    exception IncompleteDialog

    val compose  : edialog list -> unit -> (string, string) Environment.env option
    val sequence : edialog list -> unit -> (string, string) Environment.env option
    val default : 'a -> 'a option -> 'a

    val image_filter  : unit -> GFile.filter
    val all_files     : unit -> GFile.filter
    val script_filter : unit -> GFile.filter
    val mar_filter    : unit -> GFile.filter
    val xml_filter    : unit -> GFile.filter
    val jpeg_filter   : unit -> GFile.filter
    val png_filter    : unit -> GFile.filter

    type marfilter = MAR | ALL | IMG | SCRIPT | XML | JPEG | PNG
    val allfilters : marfilter list
    val fun_filter_of : marfilter -> GFile.filter

    val ask_for_file :
      ?enrich:string Environment.string_env ->
      ?title:string ->
      ?valid:(string -> bool) ->
      ?filters:marfilter list ->
      ?action:GtkEnums.file_chooser_action ->
      ?gen_id:string ->
      ?help:(unit -> unit) option ->
      unit -> string Environment.string_env option

    val does_directory_support_sparse_files : string -> bool

    val ask_for_existing_writable_folder_pathname_supporting_sparse_files :
      ?enrich:Shell.filexpr Environment.string_env ->
      ?help:(unit -> unit) option ->
      title:string -> unit -> Shell.filexpr Environment.string_env option

    val ask_for_fresh_writable_filename :
      ?enrich:string Environment.string_env ->
      title:string ->
      ?filters:marfilter list ->
      ?help:(unit -> unit) option ->
      unit -> string Environment.string_env option

    val ask_for_existing_filename :
      ?enrich:Shell.filexpr Environment.string_env ->
      title:string ->
      ?filters:marfilter list ->
      ?help:(unit -> unit) option ->
      unit -> Shell.filexpr Environment.string_env option

    val ask_question :
      ?enrich:string Environment.string_env ->
      ?title:string ->
      ?gen_id:string ->
      ?help:(unit -> unit) option ->
      ?cancel:bool ->
      question:string -> unit -> string Environment.string_env option

  end (* EDialog *)
