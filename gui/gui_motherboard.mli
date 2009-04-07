(* Interface for the sensitiveness manager *)
class type sensitiveness_manager_interface = object
    method stabilize                   : Chip.performed
    method add_sensitive_when_Active   : GObj.widget -> unit
    method add_sensitive_when_Runnable : GObj.widget -> unit
    method add_sensitive_when_NoActive : GObj.widget -> unit
end

module Make :
  functor (S : sig val st : State.globalState end) ->
    sig
      val system : Chip.system
      val sensitiveness_manager : sensitiveness_manager_interface
    end
