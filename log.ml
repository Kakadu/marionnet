(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2008  Luca Saiu

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


(** Are we in debug mode? We can check this once and for all, as this
    setting can't be changed at runtime: *)
let debug_mode =
  Initialization.configuration#bool "MARIONNET_DEBUG";;

(** This is useful to suppress logs in non-debugging mode: we simply
    print to /dev/null instead of /dev/stdout: *)
let dev_null_output_channel =
  open_out "/dev/null";;

module type MyOverriddenFunctionsSignature = sig
  (** There's obviously something fishy in the type of printf. Without
      this explicit declaration the type is always inferred as
      (('_a, out_channel, unit) format) -> '_a, which of course is not
      what I want. A little magic can work around the problem... *)
  val printf : (('a, out_channel, unit) format) -> 'a;;
  val print_string : string -> unit;;
  val print_int : int -> unit;;
  val print_float : float -> unit;;
  val print_newline : unit -> unit;;
  val print_endline : string -> unit;;
end;;

module MyOverriddenFunctions : MyOverriddenFunctionsSignature = struct
  (** Take a format string and either use it for Printf.printf, or use it
      for a dummy printf-like function which does nothing, according to
      whether we're in debug mode or not: *)
  let printf format_string =
    Obj.magic
      (if Initialization.configuration#bool "MARIONNET_DEBUG" then
        Printf.printf format_string
      else
        Printf.ifprintf dev_null_output_channel format_string);;

  (** Take a function from pervasives and either return it unchanged or
      return a noop function, according to whether we're in debug mode: *)
  let pervasive_or_noop pervasive parameter =
    if Initialization.configuration#bool "MARIONNET_DEBUG" then
      pervasive parameter
    else
      ();;

  (** Let's override some pervasives: *)
  let print_string = pervasive_or_noop Pervasives.print_string;;
  let print_int = pervasive_or_noop Pervasives.print_int;;
  let print_float = pervasive_or_noop Pervasives.print_float;;
  let print_newline = pervasive_or_noop Pervasives.print_newline;;
  let print_endline = pervasive_or_noop Pervasives.print_endline;;
end;;

let printf = MyOverriddenFunctions.printf;;
let print_string = MyOverriddenFunctions.print_string;;
let print_int = MyOverriddenFunctions.print_int;;
let print_float = MyOverriddenFunctions.print_float;;
let print_newline = MyOverriddenFunctions.print_newline;;
let print_endline = MyOverriddenFunctions.print_endline;;
