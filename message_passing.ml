(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007  Luca Saiu

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

(** A general-purpose message-passing facility, with transparent
    thread synchronization *)
class ['a] queue = object(self)
  val elements = ref []
  val mutex = Mutex.create ()
  val empty_condition = Condition.create ()

  (** This is not synchronized *)
  method private __empty =
    !elements = []

  method enqueue x =
    Mutex.lock mutex;
    elements := !elements @ [x];
    Condition.signal empty_condition;
    Mutex.unlock mutex

  (* This allows to use the queue as a deque, for 'urgent' messages, like
     thread termination requests: *)
  method prepend x =
    Mutex.lock mutex;
    elements := x :: !elements;
    Condition.signal empty_condition;
    Mutex.unlock mutex

  method dequeue : 'a =
    Mutex.lock mutex;
    while self#__empty do
      Condition.wait empty_condition mutex;
    done;
    let result =
      match !elements with
        x :: rest -> elements := rest; x
      | _ -> assert false in
    Mutex.unlock mutex;
    result
end;;

(*
let queue = new queue;;

let make_producer () =
  Thread.create
    (fun () ->
      while true do
        queue#enqueue (Random.int 1000);
      done)
    ();;

let make_consumer =
  let consumer_next_id = ref 1 in
  fun () ->
  let id = ! consumer_next_id in
  consumer_next_id := !consumer_next_id + 1;
  Thread.create
    (fun () ->
      while true do
        Log.printf "From consumer %i: got %i\n" id (queue#dequeue);
        flush_all ();
      done)
    ();;

let w = new task_runner;;

let make_producer x =
  Thread.create
    (fun () ->
      while true do
        w#schedule
          (fun () -> Log.printf "%i" x; flush_all ());
      done)
    ();;

let _ = make_producer 1;;
let _ = make_producer 2;;
let _ = make_producer 3;;
let _ = make_producer 4;;
let _ = make_producer 5;;
let _ = make_producer 6;;

Unix.sleep 30;;

w#terminate;;
*)
