(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007  Luca Saiu
   Minor change in 2008 by Luca Saiu

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


(** This is intended to be compatible with a subset of OCaml's Mutex module. 
    Only [val try_lock : t -> bool] is missing now, and it could be easily
    added if the need ever arose. *)


(** A simple implementation of recursive mutexes. Non-recursive mutexes are
    a real pain to use: *)
type t =
  Mutex.t *               (* the underlying mutex *)
  Mutex.t *               (* a mutex protecting the other fields *)
  (Thread.t option ref) * (* the thread currently holding the mutex *)
  (int ref);;             (* lock counter *)

(** Create a new recursive mutex: *)
let create () : t =
  (Mutex.create ()),
  (Mutex.create ()),
  (ref None),
  (ref 0);;

(** Lock a mutex; only block if *another* thread is holding it. *)
let rec lock (the_mutex, fields_mutex, owning_thread_ref, lock_counter_ref) =
  let my_thread = Thread.self () in
  Mutex.lock fields_mutex;
  match !owning_thread_ref with
    None -> begin
      owning_thread_ref := Some my_thread;
(*       assert (!lock_counter_ref = 0); *)
      lock_counter_ref := !lock_counter_ref + 1;
      Mutex.unlock fields_mutex;
      Mutex.lock the_mutex;
    end
  | Some t when t = my_thread -> begin
      lock_counter_ref := !lock_counter_ref + 1;
      Mutex.unlock fields_mutex;
    end
  | Some _ -> begin
      Mutex.unlock fields_mutex;
      (* the_mutex is locked. Passively wait for someone to free it: *)
      Mutex.lock the_mutex;
      Mutex.unlock the_mutex;
      (* Try again: *)
      lock (the_mutex, fields_mutex, owning_thread_ref, lock_counter_ref);
    end;;

(** Unlock a recursive mutex owned by the calling thread. *)
let unlock (the_mutex, fields_mutex, owning_thread_ref, lock_counter_ref) =
  let _ = Thread.self () in
  Mutex.lock fields_mutex;
  match !owning_thread_ref with
    None ->
      Printf.printf "\n\nrecursive_mutex: UNLOCK OCCURRED BEFORE LOCK. This is currently not supported\n\n\n";
      flush_all ();
      assert false;
  | Some t -> begin
(*       assert (t = my_thread); *)
(*       assert (!lock_counter_ref > 0); *)
      if !lock_counter_ref = 1 then begin
        lock_counter_ref := 0;
        owning_thread_ref := None;
        Mutex.unlock fields_mutex;
        Mutex.unlock the_mutex;
      end
      else begin
        lock_counter_ref := !lock_counter_ref - 1;
        Mutex.unlock fields_mutex;
      end
    end;;

(** Execute thunk in a synchronized block, and return the value of returned
    by the thunk. If executing thunk raises an exception the same exception
    is propagated, after correctly unlocking the mutex: *)
let with_mutex mutex thunk =
  lock mutex;
  try
    let result = thunk () in
    unlock mutex;
    result
  with e -> begin
    unlock mutex;
    Printf.printf
      "with_mutex: exception %s raised in critical section.\n  Unlocking and re-raising.\n"
      (Printexc.to_string e);
    raise e;
  end;;

(*
(* Test *)
let x =
  ref 42;;
let m = create ();;
let t1 =
  Thread.create
    (fun () ->
      while true do
        let n = (Random.int 10) + 1 in
        for i = 1 to n do
          lock m;
        done;
        x := !x + 1;
        Printf.printf "%i " !x; flush_all ();
        x := !x - 1;
        for i = 1 to n do
          unlock m;
        done;
(*         Thread.delay 1.0; *)
      done)
    ();;
let t2 =
  Thread.create
    (fun () ->
      while true do
        let n = (Random.int 10) + 1 in
        for i = 1 to n do
          lock m;
        done;
        x := !x - 100;
        Printf.printf "%i " !x; flush_all ();
        x := !x + 100;
        for i = 1 to n do
          unlock m;
        done;
(*         Thread.delay 1.0; *)
      done)
    ();;
let t3 =
  Thread.create
    (fun () ->
      while true do
        lock m;
        Printf.eprintf "%i\n" !x; flush_all ();
        unlock m;
        Thread.delay 1.0;
      done)
    ();;

Thread.delay 60.0;;
*)
