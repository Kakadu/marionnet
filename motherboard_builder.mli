module Make :
  functor (S : sig val st : State.globalState end) ->
    sig
      val system : Chip.system
      val set_treeview_filenames_invariant : unit -> unit
    end
