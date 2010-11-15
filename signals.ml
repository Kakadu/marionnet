(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007, 2008  Luca Saiu
   Copyright (C) 2007, 2008  Université Paris 13

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

open PreludeExtra.Prelude;; (* We want synchronous terminal output *)
open Unix;;
open Sys;;

let rec do_while body predicate =
  try
    body ();
    if predicate () then
      do_while body predicate;
  with _ -> begin
(*    Log.printf "My (poor man's :-)) do..while loop exited with an exception\n";
    flush_all (); *)
  end;;

let rec sigchld_handler signal =
  try
    Log.printf "SSS: in sigchld_handler.\n"; flush_all (); 
    let pid, _ = Unix.waitpid [Unix.WNOHANG] 0 
    in if pid <> 0 
    then begin
      Log.printf "!!!!!!! The process with pid %i died.\n" pid;
      flush_all ();
      sigchld_handler signal; (* look for *other* dead children *)
    end;
  with
    Unix.Unix_error(_, "waitpid", _) -> begin
      Log.printf "!!!!!!! waitpid() failed (this might also be uninteresting).\n"; flush_all (); 
    end;;

let is_sigchld_handled_ref = ref false;;
let is_sigchld_blocked_ref = ref false;;

(** Install the SIGCHLD handler: *)
let rec install_sigchld_handler () =
  set_signal
    sigchld
    (Signal_handle sigchld_handler);
  (* No need for synchronization here: we only do this once at startup, if ever: *)
  is_sigchld_handled_ref := true;;

(* and sigchld_handler _ = *)
(*   print_string "[in the SIGCHLD handler]\n"; flush_all (); *)
(*   let returned_pid = ref (-1) in *)
(*   do_while *)
(*     (fun () -> *)
(*       print_string "OK-W 1\n"; flush_all (); *)
(*       let pid, process_status = waitpid [WNOHANG] (-1) in *)
(*       print_string "OK-W 2\n"; flush_all (); *)
(*       returned_pid := pid; *)
(*       if not (pid = 0) && not (pid = -1) then begin *)
(*         print_string "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"; *)
(*         Log.printf *)
(*           "!!!! The process with pid %i %s\n" *)
(*           pid *)
(*           (match process_status with *)
(*             WEXITED i -> Printf.sprintf "exited via exit(), returning %i" i *)
(*           | WSIGNALED i -> Printf.sprintf "was killed by signal %i" i *)
(*           | WSTOPPED i -> Printf.sprintf "was stopped by signal %i" i); *)
(*         print_string "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"; *)
(*       end; *)
(*       flush_all ()) *)
(*     (fun () -> not (!returned_pid = 0)); *)
(*   handle_sigchld (); *)
(*   print_string "[About to return from the SIGCHLD handler]\n"; flush_all ();; *)

(** Block SIGCHLDs, until the next handle_sigchld call: *)
let block_sigchld () =
  print_string "SSS: Blocking SIGCHLD.\n"; flush_all ();
(*   ignore (Unix.sigprocmask SIG_BLOCK [sigchld]); *)
  ignore (Thread.sigmask SIG_BLOCK [sigchld]);
  is_sigchld_blocked_ref := true;;

(** Resume handling SIGCHLDs, starting from the ones which were enqueued while the
    signal was blocked, if any: *)
let unblock_sigchld () =
  print_string "SSS: Unblocking SIGCHLD.\n"; flush_all ();
  is_sigchld_blocked_ref := false;
(*   ignore (Unix.sigprocmask SIG_UNBLOCK [sigchld]);; *)
  ignore (Thread.sigmask SIG_UNBLOCK [sigchld]);;

let is_sigchld_handled () =
  !is_sigchld_handled_ref;;

let is_sigchld_blocked () =
  !is_sigchld_blocked_ref;;

(* (\** Handle SIGCHLD by default: *\) *)
(* install_sigchld_handler ();; *)

(** Execute thunk in a block temporarily blocking signals. Return the result
    of evaluating the thunk. This correctly unblocks the signal (if it was
    blocked before, of course) even if the thunk raises an exception, and in
    that case with_sigchld_blocked raises the same exception: *)
let with_sigchld_blocked thunk =
  if not (is_sigchld_handled ()) then
    thunk ()
  else
  let was_sigchld_blocked = is_sigchld_blocked () in
  (if not was_sigchld_blocked then block_sigchld ());
  try
    let result = thunk () in
    (if not was_sigchld_blocked then unblock_sigchld ());
    result
  with e -> begin
    (if not was_sigchld_blocked then unblock_sigchld ());
    Log.printf
      "! Re-raising an exception (%s) from with_sigchld_blocked\n"
      (match e with
        Unix.Unix_error(code, "waitpid", _) -> 
          (Printf.sprintf "the infamous waitpid failure with code %s" (Unix.error_message code))
      | Unix.Unix_error(code, primitive, _) -> 
          (Printf.sprintf "a Unix failure: %s: %s" primitive (Unix.error_message code))
      | Failure s -> Printf.sprintf "Failure \"%s\"" s
      | _ -> "some unrecognized exception")
      ;
    flush_all ();
    raise e;
  end;;

(*   with_sigchld_blocked *)
(*     (fun () -> Unix.system argument);; *\) *)
let my_system argument =
    match fork() with
      0 -> begin try
        execv "/bin/sh" [| "/bin/sh"; "-c"; argument |]
      with _ ->
        exit 127
      end
    | id ->
        print_string "system: calling waitpid (this must NOT fail)\n"; flush_all ();
        try
          let result =
            snd(waitpid [] id)
          in
          print_string "system: returned fom waitpid, still alive\n"; flush_all ();
          result
        with e -> begin
          print_string "system: !!!!, waitpid failed, but here it shouldn't!\n"; flush_all ();
          Log.printf
            "system: the failure is %s\n"
            (match e with
              Unix.Unix_error(code, "waitpid", _) -> 
                (Printf.sprintf "the infamous waitpid failure: %s" (Unix.error_message code))
            | Unix.Unix_error(code, primitive, _) -> 
                (Printf.sprintf "a Unix failure: %s: %s" primitive (Unix.error_message code))
            | Failure s ->
                Printf.sprintf "Failure \"%s\"" s
            | _ ->
                "some unrecognized exception");
            Log.printf "my_system: re-raising %s.\n" (Printexc.to_string e);
            raise e;
        end;;
          
(** Unix.system is implemented using waitpid, and this interferes with our
    signal system. Here I provide a wrapper implemented with with_sigchld_blocked
    to temporarily block signals around a call to system: *)
let system argument =
  with_sigchld_blocked
(*     (fun () -> my_system argument);; *)
    (fun () -> Unix.system argument);;
