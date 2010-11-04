class type motherboard =
 object
  method reversed_rj45cables_cable : (bool,bool) Chip.cable
  method project_working_directory : string
end (* class type motherboard *)

(** Handler to access to some global variables: *)
include Stateful_modules.Variable (struct
  type t = motherboard
  let name = Some "motherboard"
  end)
