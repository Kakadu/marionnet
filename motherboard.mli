class type motherboard =
  object
    method refresh_sketch : unit
    method project_working_directory : string
  end
  
type t = motherboard (* alias *)

val extract  : unit -> t
val set      : t -> unit (* performed once at loading time in Motherboard_builder *)
