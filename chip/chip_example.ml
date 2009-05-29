(* ocamlc -pp 'camlp4of chip_pa.cmo' chip_example.ml *)
(* camlp4of chip_pa.cmo chip_example.ml *)

#load "chip_parser_p4.cmo";;
 (* Use the "chip" syntax extension. *)

open Chip ;;
 (* if you wan't Chip opened, you can alternatively call:
    Chip.teach_ocamldep;;   - in order to teach ocamldep on Chip dependency, or
    Chip.initialize ();;    - in order to teach ocamldep and create an "current" implicit system. *)

(*Chip.initialize ();;*)
(*
let r = Chip.get_current_system () ;;
let r  = new system ~max_number_of_iterations:2048 () ;;
*)

(** CREAZIONE DEL SISTEMA DI CHIP INTERCONNESSI *)

(* Chip class definition *)
chip toto (e:int) : (a,b,c) -> (y:int,z:int) = (a*b+e, b*c) ;;

(* Wire creation. *)
let w1 = wref 2  ;;
let w2 = wref 3  ;;
let w3 = wref 5  ;;
let w4 = wref 7  ;;
let w5 = wref 11 ;;
let w6 = wref 13 ;;
let w7 = wref 19 ;;

(* Chip instance creation and immediate connection. *)
let t1 = new toto 0 ~a:w1 ~b:w2 ~c:w3 ~y:w4 ~z:w5 () ;;

(* Chip instance creation with differed connection. *)
let t2 = new toto 0 () ;;
t2#connect (w1,w4,w5) (w6,w7) ;;
(* t2#connect_a w1 ;; *)


(Printf.printf "w7#get=%d\n" w7#get) ;;
w3#set 6 ;;
(Printf.printf "w7#get=%d\n" w7#get) ;;
Chip.stabilize () ;;
(Printf.printf "w7#get=%d\n" w7#get) ;;

chip selector : (x1:int,x2:int,s) -> (y) =
 if s then x1 else x2
;;

let w8 = wref true ;;
let s = new selector ~x1:w1 ~x2:w2 ~s:w8 ~y:w7 () ;;

(*chip distributor : (x,s) -> (y1,y2) =
 let (y1,y2) = self#get in
  if s then (x,y2) else (y1,x)
;;*)

(*chip distributor : (x,s) -> (y1,y2) =
  match self#get with
  | (y1,y2) -> (if s then (x,y2) else (y1,x))
;;*)

chip distributor : (x:int list,s) -> (y1,y2,y3:int list,y4) =
  let (y1,y2,_,_) = self#get in
  if s then (x,y2,x,x) else (y1,x,x,x)
;;

chip virtual tata : (x:int list) -> () =
  let () = self#get in ()
;;

chip titi : () -> (z:float, w:int) =
   fun x -> (x *. 3.14, int_of_float x)
;;

let w9 = wref 6.28 ;;
let t = new titi ~z:w9 ~w:w1 () ;;
t#emit 9.887 ;;

(*let w8 = wref true ;;
let s = new distributor ~x:w1 ~s:w8 ~y1:w7 ~y2:w6 () ;;*)

