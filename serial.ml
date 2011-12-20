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


(* Examples: 

# get_info_by_shell_command "uml_mconsole m1 config con8" ;;
  : string = "OK pts:/dev/pts/29"

# get_info_by_shell_command ~pattern:"/dev/pts/[0-9]+" "uml_mconsole m1 config con8" ;;
  : string = "/dev/pts/29"

# let xx="[A-Fa-f0-9][A-Fa-f0-9]" in
  get_info_by_shell_command ~pattern:(String.concat ":" [xx;xx;xx;xx;xx;xx]) "ifconfig eth0" ;;
  : string = "00:50:04:45:d5:b0"
*)
let get_info_by_shell_command ?exn ?shell ?input ?pattern cmd =
  let info = UnixExtra.shell ?shell ?input cmd in
  match pattern with
  | None -> StringExtra.strip info
  | Some regexp_as_string ->
      let regexp = StrExtra.mkregexp ~groups:[regexp_as_string] () in
      (match (StrExtra.First.matching regexp info), exn with
       | None, None -> raise Not_found
       | None, Some exn -> raise (Lazy.force exn)
       | Some (_, _, [group]), _ -> group
       | _ -> assert false
       )

(* Examples:
# get_infos_by_shell_command "ifconfig eth1" ;;
  : StringExtra.word list =
["eth1"; "Link"; "encap:Ethernet"; "HWaddr"; "00:1e:8c:cc:4d:50"; "inet";
 "adr:192.168.0.3"; "Bcast:192.168.0.255"; "Masque:255.255.255.0"; "adr";
 "inet6:"; "fe80::21e:8cff:fecc:4d50/64"; "Scope:Lien"; "UP"; "BROADCAST";
 "RUNNING"; "MULTICAST"; "MTU:1500"; "Metric:1"; "Packets";
 "re\195\167us:792675"; "erreurs:0"; ":0"; "overruns:0"; "frame:0"; "TX";
 "packets:676479"; "errors:0"; "dropped:0"; "overruns:0"; "carrier:0";
 "collisions:0"; "lg"; "file"; "transmission:1000"; "Octets";
 "re\195\167us:672198706"; "(672.1"; "MB)"; "Octets"; "transmis:138468927";
 "(138.4"; "MB)"; "Interruption:19"; "Adresse"; "de"; "base:0x6000"]
 
# get_infos_by_shell_command ~patterns:["MTU:[0-9]+"; "HWaddr [A-Fa-f0-9:]+"] "ifconfig eth1" ;;
- : StringExtra.word list = ["MTU:1500"; "HWaddr 00:1e:8c:cc:4d:50"]
*)
let get_infos_by_shell_command ?shell ?input ?patterns cmd =
  let info = UnixExtra.shell ?shell ?input cmd in
  match patterns with
  | None -> StringExtra.Text.collapse_and_split (StringExtra.Text.of_string (StringExtra.strip info))
  | Some regexps_as_strings ->
      List.concat
        (List.map
           (function regexp_as_string ->
	      let regexp = StrExtra.mkregexp ~groups:[regexp_as_string] () in
	      (List.map
		 (function
		 | (_, _, [group]) -> group
		 | _ -> assert false
		 )
	         (StrExtra.Global.matching regexp info)
	         ))
	    regexps_as_strings)


let get_pts_from_optional_pts_umid_con ?umid ?con ?pts caller =
  match pts, umid, con with
  | (Some pts), _, _ -> pts
  | None, (Some umid), (Some con) ->
      let cmd = Printf.sprintf "uml_mconsole %s config %s" umid con in
      let error_msg = lazy (Printf.sprintf "%s: no pts assigned to umid=%s and con=%s" caller umid con) in
      let exn = lazy (Invalid_argument (Lazy.force error_msg)) in
      get_info_by_shell_command ~exn ~pattern:"/dev/pts/[0-9]+"  cmd
  | None, _, _ ->
      let error_msg = Printf.sprintf "%s: ~pts or both ~umid and ~con must be provided" caller in
      invalid_arg error_msg
;;


(* Ex: send_command ~pts:"/dev/pts/33" ~cmd:"reboot" () *)
let send_command ?umid ?con ?pts cmd : unit =
  let pts = get_pts_from_optional_pts_umid_con ?umid ?con ?pts "Serial.send_command" in
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
let is_delimiter_included ~delimiter_pattern s =
 try
   let last_index  = String.rindex_from s ((String.length s)-1) '\n' in
   let start_index = Str.search_backward delimiter_pattern s last_index in
   let candidate = String.sub s start_index (last_index - start_index) in
   (* The result: *)
   match StrExtra.First.matching delimiter_pattern candidate with
   | None -> None
   | Some (_, _, [exit_code]) -> Some (start_index, (int_of_string exit_code))
   | _ -> assert false
 with _ -> None

(* Ex: send_command_and_wait_answer ~pts:"/dev/pts/33" ~cmd:"find /usr -name foo" () *)
let send_command_and_wait_answer ?(timeout=10.) ?(buffer_size=1024) ?umid ?con ?pts cmd =
  let pts = get_pts_from_optional_pts_umid_con ?umid ?con ?pts "Serial.send_command_and_wait_answer" in
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
  let delimiter, delimiter_pattern =
    let _32_hex_chars = Digest.to_hex (Digest.string "end-of-answer-delimiter") in
    (Printf.sprintf "### $? ### %s" _32_hex_chars),
    (Str.regexp (Printf.sprintf "### \\([0-9]+\\) ### %s" _32_hex_chars))
  in
  let echo_command = Printf.sprintf "echo \"%s\"\n" delimiter in
  let echoed_echo_command = Printf.sprintf "echo \"%s\"\r\n" delimiter
  in
  (* Note that the echo_command may be echoed one or two times by the terminal... *)
  let _ = Unix.write fd echo_command 0 (String.length echo_command) in
  let rec loop ?blocking () =
       (* Wait a bit for the answer: *)
       Thread.delay 0.1; 
    let _ = get_unread_chars_from ?blocking ~buffer ~fd () in
    (* Get now the last 80 chars (approximatively a line): *)
    let (current_trailer, current_trailer_starting_offset) =
      let b = Buffer.length buffer in
      let j = max offset_answer (b - 80) in
      ((Buffer.sub buffer j (b-j)), j)
    in
    match is_delimiter_included ~delimiter_pattern current_trailer with
    | None   -> loop ()
    | Some (i, exit_code) -> (current_trailer_starting_offset + i, exit_code)
  in
  let (delimiter_starting_index, exit_code) =
    loop ~blocking:() ()
  in
  let answer = Buffer.sub buffer offset_answer (max 0 (delimiter_starting_index - offset_answer)) in
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
  (answer, exit_code)
;;
