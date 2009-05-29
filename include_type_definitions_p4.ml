(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2009  Jean-Vincent Loddo

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

(** Include type definitions of an interface into an implementation.
    Usage (in your .ml):

      #load "include_type_definitions_p4.cmo";;
      INCLUDE DEFINITIONS "<filename>.mli"

    Type definitions are, in outline, mli phrases with '=' or exception definitions.
    More precisely, only phrases of the following form will be imported:
    1)  type ... = ...
    2)  module type ... = ...
    3)  class type ... = ...
    4)  exception ...
    Any other phrase of <filename>.mli will be ignored.
  *)

(* ocamlc -c -pp camlp4of -I +camlp4 include_type_definitions_p4.ml *)

open Camlp4 (* -*- camlp4o -*- *)

module Id = struct
  let name = "Include_type_definitions"
  let version = "$Id: include_type_definitions_p4.ml,v 0.1 2009/03/18 16:16:16 $"
end

module Make (Syntax : Sig.Camlp4Syntax) = struct
  open Sig
  include Syntax

  EXTEND Gram
    GLOBAL: str_item;

    str_item: FIRST
      [ [
         "INCLUDE"; "DEFINITIONS"; fname = STRING ->

           let parse_file file =
             let ch = open_in file in
             let st = Stream.of_channel ch in
             (Gram.parse sig_items (Loc.mk file) st)
           in

           let rec list_of_sgSem = function
            | Ast.SgNil _ -> []
            | Ast.SgSem (_, x, xs) -> x :: (list_of_sgSem xs)
            | x -> [x]
           in

           let t = parse_file fname in
           let pred = function
             | Ast.SgTyp (_, Ast.TyDcl (_, _, _, Ast.TyNil _, _)) -> false
             | Ast.SgTyp (_, Ast.TyDcl (_, _, _,    _       , _)) -> true
             | Ast.SgExc (_,_)
             | Ast.SgMty (_,_,_)
             | Ast.SgClt (_,_) -> true
             | _ -> false
           in
           let l = List.filter pred (list_of_sgSem t) in
           let mill = function
             | Ast.SgTyp (a, Ast.TyDcl (b,c,d,e,f)) -> Ast.StTyp (a, Ast.TyDcl (b,c,d,e,f))
             | Ast.SgExc (a,b)                      -> Ast.StExc (a,b,Ast.ONone)
             | Ast.SgMty (a,b,c)                    -> Ast.StMty (a,b,c)
             | Ast.SgClt (a,b)                      -> Ast.StClt (a,b)
             | _ -> assert false
           in

           Ast.stSem_of_list (List.map mill l)

      ] ]
    ;

  END

end

let module M = Register.OCamlSyntaxExtension (Id) (Make) in ()
