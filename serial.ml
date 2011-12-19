(* This file is part of marionnet
   Copyright (C) 2011 Jean-Vincent Loddo

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


(* Ex: send_command ~pts:"/dev/pts/33" ~cmd:"reboot" () *)
let send_command ~pts ~cmd () : unit =
  let fd = Unix.openfile pts [ Unix.O_RDWR; Unix.O_NOCTTY; ] 0o640  in
  let cmd = if String.get cmd ((String.length cmd) - 1) = '\n' then cmd else (cmd^"\n") in
  let _ = Unix.write fd cmd 0 (String.length cmd) in
  Unix.close fd
;;

(* Modify the buffer and return the specification (offset, length) of the buffer substring which has been read: *)
let get_unread_chars_from ?blocking ~fd ~buffer () : int * int =
  let fd' = Unix.dup fd in
  let blocking = (blocking <> None) in
  let one_shot_action = lazy (Unix.set_nonblock fd') in
  let () = if blocking then Unix.clear_nonblock fd' else Lazy.force one_shot_action in
  let x = String.create 100 in
  let rec loop count =
    let n = try Unix.read fd' x 0 100 with Unix.Unix_error (Unix.EAGAIN,_,_) -> 0 in
    let () = Buffer.add_substring buffer x 0 n in
    let count = count + n in
    if n < 100 then count
    else begin
      (* The blocking mode (if set) concerns only the first read call, not the successive.
         Thus we set now the non blocking flag (if not already done): *)
      Lazy.force one_shot_action;
      loop count
      end
  in
  begin
    let offset = Buffer.length buffer in
    let count = loop 0 in
    Unix.close fd';
    (offset, count)
  end
;;

(* Is the delimiter included in the string, immediately before the last \n or \r\n ?
   In the positive case, the result is the last index before the delimiter. *)
let is_delimiter_included ~delimiter s =
 let n = String.length delimiter in
 try
   let j = String.rindex_from s ((String.length s)-1) '\n' in
   let start_index =
     match (String.get s (j-1)) = '\r' with
     | true  -> (j-n-1)
     | false -> (j-n) 
   in
   let candidate = String.sub s start_index n in
   (* The result: *)
   if (candidate = delimiter) then Some start_index else None
 with _ -> None

(* Ex: send_command_and_wait_answer ~pts:"/dev/pts/33" ~cmd:"find /usr -name foo" () *)
let send_command_and_wait_answer ?(timeout=10.) ?(buffer_size=1024) ~pts ~cmd () : string list =
  let fd = Unix.openfile pts [ Unix.O_RDWR; Unix.O_NOCTTY; ] 0o640  in
  let buffer = Buffer.create buffer_size in
  let (cmd, cmd_length) =
    let n = String.length cmd in
    if String.get cmd (n-1) = '\n' then (cmd, n) else ((cmd^"\n"), n+1)
  in
  (* The command will be echoed replacing '\n' by '\r\n', so: *)
  let echoed_cmd =
    let result = cmd^"\n" in
    let () = String.set result (cmd_length-1) '\r' in
    result
  in
  let (_, offset_answer) = get_unread_chars_from ~buffer ~fd () in
  let _ = Unix.write fd cmd 0 cmd_length in
  let _ = Unix.select [fd] [] [] timeout in
  (* Now we will try to detect the end of answer with an ad-hoc echo command: *)
  let delimiter =
    let _32_hex_chars = Digest.to_hex (Digest.string "end-of-answer-delimiter") in
    Printf.sprintf "### %s" _32_hex_chars
  in
  let echo_command = Printf.sprintf "echo '%s'\n" delimiter in
  let echoed_echo_command = Printf.sprintf "echo '%s'\r\n" delimiter
  in
  (* Note that the echo_command may be echoed one or two times by the terminal... *)
  let _ = Unix.write fd echo_command 0 (String.length echo_command) in
  let rec loop ?blocking () =
       (* Wait a bit for the answer: *)
       Thread.delay 0.1; 
    let (off,len) = get_unread_chars_from ?blocking ~buffer ~fd () in
    let (current_trailer, current_trailer_starting_offset) =
      (* Get the last 80 chars (approximatively a line): *)
      let b = Buffer.length buffer in
      let j = max offset_answer (b - 80) in
      ((Buffer.sub buffer j (b-j)), j)
    in
    match is_delimiter_included ~delimiter current_trailer with
    | None   -> loop ()
    | Some i -> current_trailer_starting_offset + i (* comment_starting_index *)
  in
  let comment_starting_index =
    loop ~blocking:() ()
  in
  let answer = Buffer.sub buffer offset_answer (max 0 (comment_starting_index - offset_answer)) in
  (* Cleaning and structuring the answer: *)
  let answer = StrExtra.Global.substitute (Str.regexp_string echoed_echo_command) (fun _ -> "") answer in
  let answer = StrExtra.First.substitute  (Str.regexp_string echoed_cmd) (fun _ -> "") answer in
  let answer = Str.global_replace (Str.regexp "\r\n") "\n" answer in
  let answer = StringExtra.Text.of_string answer in
  let answer =
    if List.length answer >= 1
      then List.rev (List.tl (List.rev answer)) (* removing last line (the prompt) *)
      else answer
  in
  Unix.close fd;
  answer
;;
