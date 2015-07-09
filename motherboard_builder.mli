module Make :
  functor (S : sig val st : State.globalState end) ->
    sig
      val sensitive_widgets_initializer : unit -> unit
    end
