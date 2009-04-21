(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007, 2008  Luca Saiu

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

open Graph;;
open Message_passing;;

let do_in_parallel thunks =
  (* Make a thread per thunk: *)
  let threads =
    List.map
      (fun thunk ->
        Thread.create
          (fun () ->
            try
              thunk ()
            with e -> begin
              Log.printf "!!!! do_in_parallel: a thunk failed (%s)\n" (Printexc.to_string e);
              flush_all ();
            end)
          ())
      thunks in
  (* Wait that they terminate: *)
  List.iter
    (fun thread ->
      try
        Thread.join thread;
      with e -> begin
        Log.printf "!!!!!!!!!!!!!!! This should not happen: join failed (%s)\n"
          (Printexc.to_string e);
        flush_all ()
      end)
    threads;;

(** A thunk is trivially represented a unit->unit function: *)
type thunk =
    unit -> unit;;

(** A task is a thunk, to be executed by the task-runner thread: *)
type task = thunk;;

(** A graph of tasks combines tasks with their dependency relation: *)
type task_graph =
    thunk graph;;

(** This is only used internally. *)
exception Kill_task_runner;;

(** This class allows to enqueue tasks (represented as thunks) to
    be executed one after another, all in the same thread which is
    created at initialization time. *)
class task_runner = object(self)
  val queue : (string * (unit -> unit)) queue = new queue
  
  val run_again = ref true

  initializer
(*     (\** Handle SIGCHLD by default: *\) *)
(*     Signals.install_sigchld_handler (); *) (* No, absolutely not! :-) *)
    ignore (Thread.create
              (fun () ->
                while !run_again do
                  Log.printf "TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT\n"; flush_all ();
                  Log.printf "= TT I'm ready for the next task...\n"; flush_all ();
                  Log.printf "TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT\n"; flush_all ();
                  let name, task = queue#dequeue in
                  self#run name task;
                done)
              ())

  method run name task =
    flush_all ();
    try
      Log.printf "TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT\n"; flush_all ();
      Log.printf "+ TT Executing the task \"%s\"\n" name; flush_all ();
      Log.printf "TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT\n"; flush_all ();
      task ();
      Log.printf "TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT\n"; flush_all ();
      Log.printf "- TT The task \"%s\" succeeded.\n" name; flush_all ();
      Log.printf "TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT\n"; flush_all ();
    with Kill_task_runner -> begin
      Log.printf "TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT\n"; flush_all ();
      Log.printf "The task runner was explicitly killed.\n"; flush_all ();
      Log.printf "! TT TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT\n"; flush_all ();
      run_again := false;
    end
    | e -> begin
      Log.printf "TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT\n"; flush_all ();
      Log.printf "WARNING: the asynchronous task \"%s\" raised an exception\n" name;
      Log.printf "         (%s).\n" (Printexc.to_string e);
      Log.printf "         THIS MAY BE SERIOUS.\n";
      Log.printf "         Anyway, I'm going to continue with the next task.\n";
      Log.printf "TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT\n"; flush_all ();
      flush_all ();
    end

  method schedule ?(name="[unnamed task]") task =
    queue#enqueue (name, task)

  method prepend ?(name="[unnamed task]") task =
    queue#prepend (name, task)

  (** This queue is only used for synchronization purposes. Message-passing
      synchronizations is way spiffier :-) *)
  val dummy_queue : unit queue = new queue

  (** Wait that all tasks which are currently scheduled terminate, synchronously.
      In the mean time more tasks can be scheduled as usual. *)
  method wait_for_all_currently_scheduled_tasks =
    self#schedule
      ~name:"wait until all scheduled tasks terminate"
      (fun () -> dummy_queue#enqueue ());
    Log.printf "Waiting for all currently enqueued tasks to terminate...\n"; flush_all ();
    let () = dummy_queue#dequeue in
    Log.printf "...all right, we have been signaled: tasks did terminate.\n"; flush_all ();

  method schedule_parallel (names_and_thunks : (string * thunk) list) =
    let parallel_task_name =
      List.fold_left
        (fun s name -> s ^ name ^ " || ")
        "In parallel: "
        (List.map (fun (name, _) -> name) names_and_thunks) in
    let parallel_task_thunk =
      fun () ->
        let threads =
          List.map
            (fun (name, thunk) -> name, Thread.create thunk ())
            names_and_thunks in
        List.iter
          (fun (name, thread) ->
            Log.printf "TT Joining \"%s\"...\n" name; flush_all ();
            (try 
              Thread.join thread;
            with e -> begin
              Log.printf "!!!!!!!!!!!!!!!\n!!!!!!!!!!!!\n!!!!!!!!!!!!!!!\n"; 
              Log.printf "!!!!!!!!!!!!!!! This should not happen: join failed (%s)\n"
                (Printexc.to_string e);
              Log.printf "!!!!!!!!!!!!!!!\n!!!!!!!!!!!!\n!!!!!!!!!!!!!!!\n";
              flush_all ()
            end);
            Log.printf "TT I have joined \"%s\" with success\n" name; flush_all ();)
          threads in
    self#schedule ~name:parallel_task_name parallel_task_thunk

  (** A user-friendly way to schedule a set of tasks with a dependency graph.
      The description is a list of triples
      <name, list of dependencies as names, thunk>. *)
  method schedule_tasks description =
    let g = make_empty_graph () in
    (* First make the nodes, and a local name -> id table: *)
    let table = Hashtbl.create ((List.length (get_node_ids g)) * 2) in
    List.iter
      (fun (name, _, thunk) ->
        let id = add_node (name, thunk) g in
        Hashtbl.add table name id)
      description;
    (* Now make edges: *)
    List.iter
      (fun (name, dependencies, _) ->
        List.iter
          (fun a_dependency ->
            add_edge
              (Hashtbl.find table name)
              (Hashtbl.find table a_dependency)
              g)
          dependencies)
      description;
    (* Ok, we have the graph. Now we can use it to schedule tasks in some
       reasonable order: *)
    List.iter
      (fun id ->
        let name, thunk = get_node id g in
        self#schedule ~name thunk)
      (topological_sort g)
  
  method terminate =
    self#prepend
      ~name:"Destroying the task runner"
      (fun () -> raise Kill_task_runner)
end;;

let the_task_runner = 
  new task_runner;;

(*
let g =
  schedule_tasks
    [ "a", [],
      (fun () -> print_string "a\n");
      "b", ["c"],
      (fun () -> print_string "b\n");
      "c", ["a"],
      (fun () -> print_string "c\n"); ];;

let sorted_nodes = topological_sort g;;
let _ =
  List.iter
    (fun id ->
      let name, thunk = get_node id g in
      Log.printf "Executing %s\n" name;
      thunk ();)
    sorted_nodes;;
*)

(* ------------------------------------------------------------------------------------ *)
(* Example: *)
(*
let _ =
  schedule_tasks
    [ "a", [],
      (fun () -> print_string "(a) This will come first.\n");
      "b", ["c"],
      (fun () -> print_string "(b) This will come after c.\n");
      "c", ["a"],
      (fun () -> print_string "(c) This will come after a.\n"); ];;
Unix.sleep 3;;
*)
