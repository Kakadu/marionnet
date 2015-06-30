module Recursive_mutex : MutexExtra.Extended_signature 
  with type t = MutexExtra.Recursive.t

module Refresh_sketch_thunk : sig  type t = unit -> unit  val set : t -> unit  end
val refresh_sketch : unit -> unit

type shuffler = index list
and index = int

class type toolbar_driver =
  object
    method get_extrasize             : float
    method get_iconsize              : string
    method get_image                 : GdkPixbuf.pixbuf
    method get_image_current_height  : int
    method get_image_current_width   : int
    method get_image_original_height : int
    method get_image_original_width  : int
    method get_labeldistance         : float
    method get_nodesep               : float
    method reset_image_size          : unit   -> unit
    method set_extrasize             : float  -> unit
    method set_iconsize              : string -> unit
    method set_labeldistance         : float  -> unit
    method set_nodesep               : float  -> unit
  end

class tuning :
  ?iconsize: string ->
  ?shuffler: shuffler ->
  ?rankdir: string ->
  ?nodesep: float ->
  ?labeldistance: float ->
  ?extrasize: float ->
  ?curved_lines: bool ->
  network: < reversed_cable_set: bool -> string -> unit; reversed_cables: string list; to_forest: Xforest.forest; .. > ->
  unit ->           
  object
    val mutable gui_callbacks_disable : bool
    method crossover_cable_color      : string
    method curved_lines               : bool Cortex.t
    method curved_lines_commute       : bool
    method direct_cable_color         : string
    method disable_gui_callbacks      : unit -> unit
    method enable_gui_callbacks       : unit -> unit
    method eval_forest_attribute      : Xforest.attribute -> unit
    method eval_forest_child          : Xforest.tree -> unit
    method extrasize                  : float Cortex.t
    method extrasize_reset            : unit
    method from_tree                  : Xforest.node -> Xforest.forest -> unit
    method gui_callbacks_disable      : bool
    method iconsize                   : string Cortex.t
    method iconsize_for_dot           : string
    method labeldistance              : float Cortex.t
    method labeldistance_for_dot      : string
    method load_from_file             : project_version:[ `v0 | `v1 | `v2 ] -> string -> unit
    method nodesep                    : float Cortex.t
    method nodesep_for_dot            : string
    method rankdir                    : string Cortex.t
    method rankdir_for_dot            : string
    method ratio                      : string
    method reset_defaults             : unit -> unit
    method save_to_file               : string -> unit
    method set_gui_callbacks_disable  : bool -> unit
    method set_reversed_cables        : string list -> unit
    method set_toolbar_widgets        : unit -> unit
    method shuffler                   : int list Cortex.t
    method shuffler_reset             : unit
    method shuffler_as_function       : int -> int
    method to_forest                  : Xforest.forest
    method to_tree                    : Xforest.tree
    method toolbar_driver             : toolbar_driver
    method set_toolbar_driver         : toolbar_driver -> unit
  end
