(*  This file is part of Marionnet, a virtual network laboratory
    Copyright (C) 2008-2009 Jean-Vincent Loddo
 
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 2 of the License, or
    (at your option) any later version.
 
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
 
    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.*)

(* ocamlc -I +camlp4 camlp4lib.cma -pp camlp4orf -c include_as_string_p4.ml *)

(* Do not remove the following comment: it's an ocamldoc workaround. *)
(** *)

open Camlp4.PreCast
open Syntax

(** Tools for strings. *)
module Tool = struct 

(** Import a file into a string. *)
let from_descr (fd:Unix.file_descr) : string = 
 let q = Queue.create () in
 let buffer_size = 8192 in
 let buff = String.create buffer_size in
 let rec loop1 acc_n = 
  begin
   let n = (Unix.read fd buff 0 buffer_size)    in
   if (n=0) then acc_n else ((Queue.push ((String.sub buff 0 n),n) q); loop1 (acc_n + n))
   end in
 let dst_size = loop1 0 in
 let dst = String.create dst_size in
 let rec loop2 dstoff = if dstoff>=dst_size then () else
  begin
  let (src,src_size) = Queue.take q in
  (String.blit src 0 dst dstoff src_size);
  loop2 (dstoff+src_size)
  end in
 (loop2 0);
 (Printf.eprintf "Preprocessing: include_as_string: the length of the included string is %d\n" dst_size);
 dst
;;

let from_file (filename:string) : string = 
 let fd = (Unix.openfile filename [Unix.O_RDONLY;Unix.O_RSYNC] 0o640) in 
 let result = from_descr fd in
 (Unix.close fd);
 result
;;

end;;


EXTEND Gram
    GLOBAL: expr;
    expr: LEVEL "top"
      [ [ "INCLUDE_AS_STRING"; fname = STRING -> 
	    let s = Tool.from_file fname in <:expr< $str:s$ >>
      ] ]
    ;
END;


