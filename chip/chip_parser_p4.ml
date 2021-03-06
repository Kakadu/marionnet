(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2009  Jean-Vincent Loddo
   Copyright (C) 2009  Université Paris 13
   Applied an OCaml 3.12 compatibility patch by Clément Démoulins in 2011

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


open Camlp4 (* -*- camlp4o -*- *)

(* ocamlc -I +camlp4 -pp camlp4of.opt camlp4lib.cma chip_parser.p4.ml *)

(* For a strange bug, Camlp4of goes in a loop (StackOverflow) when a chip
   with more than 1 port is declared virtual. Thus, in practice, if the chip
   is virtual, we are limited to 1 port! *)

module Id = struct
  let name = "Chip_pa"
  let version = "$Id: chip_pa.ml,v 0.1 2009/02/16 16:16:16 $"
end

module Make (Syntax : Sig.Camlp4Syntax) = struct
  open Sig
  include Syntax

  (* Make a multiple application. *)
  let apply _loc f xs = List.fold_left (fun f x -> <:expr< $f$ $x$>>) f xs

  (* Make a string list ast from... a string list. *)
  let rec string_list_of_string_list _loc = function
   | []    -> <:expr< [] >>
   | x::xs -> let xs' = string_list_of_string_list _loc xs in <:expr< $str:x$ :: $xs'$>>

  let rec expr_of_expr_list _loc = function
   | []    -> <:expr< [] >>
   | x::xs -> let xs' = expr_of_expr_list _loc xs in <:expr< $x$ :: $xs'$>>

  (* Make a multiple abstraction to a class_expr. *)
  let lambda_class_expr _loc xs body = List.fold_right (fun x b -> <:class_expr< fun $x$ -> $b$>>) xs body

  (* Make a multiple abstraction to a type. *)
  let lambda_ctyp _loc xs body = List.fold_right (fun x b -> <:ctyp< ($x$ -> $b$)>>) xs body

  (* Make a fresh identifier (string). *)
  let fresh_var_name ~blacklist ~prefix =
   let rec loop n =
    let candidate = (prefix^(string_of_int n)) in
    if not (List.mem candidate blacklist) then candidate else loop (n+1)
   in
   if (List.mem prefix blacklist) then (loop 0) else prefix

  let fresh_var_names ~blacklist ~prefixes =
   let r = List.fold_left (fun l p -> (fresh_var_name ~blacklist:(l@blacklist) ~prefix:p) :: l) [] prefixes in
   List.rev r

  (* List.combine for 3 lists *)
  let rec combine3 l1 l2 l3 = match (l1,l2,l3) with
  | []    , []    , []     -> []
  | x1::r1, x2::r2, x3::r3 -> (x1,x2,x3)::(combine3 r1 r2 r3)
  | _ -> raise (Invalid_argument "combine3")

  let rec combine4 l1 l2 l3 l4 = match (l1,l2,l3,l4) with
  | []    , []    , []   , []  -> []
  | x1::r1, x2::r2, x3::r3, x4::r4 -> (x1,x2,x3,x4)::(combine4 r1 r2 r3 r4)
  | _ -> raise (Invalid_argument "combine4")

  (* Check unicity in a string list. Raises a failure with the duplicated string. *)
  let rec check_unicity = function
  | []   -> ()
  | x::r -> if (List.mem x r) then failwith ("Duplicated port name '"^x^"'")
  		              else (check_unicity r)


  EXTEND Gram
    GLOBAL: str_item;

    where:
      [ [  "complement"; w = LIST0 class_str_item ; "end" -> w ] ] ;

    port_ident:
      [ [  "("; x = LIDENT ; ":"; t = ctyp; ")" -> (x, Some t)
        |       x = LIDENT ; ":"; t = ctyp      -> (x, Some t)
        |       x = LIDENT                      -> (x, None  )  ] ] ;

    str_item: FIRST
      [ [ "chip"; virt = OPT "virtual"; class_name = LIDENT; user_parameters = LIST0 [ x=patt -> x];  ":" ;

          input_ports  = [ "(" ; l = LIST0 [ x = port_ident -> x] SEP ","; ")" -> l ];

          output_ports = [ "->"; "("; l = LIST0 [ x = port_ident -> x] SEP "," ; ")"; "=" -> l
                         | "->"; "unit"; "=" -> []
                         | "="               -> [] ];

          e = expr ; w = OPT where ->

          (* Class type *)
          let is_virtual = (virt <> None) in

          (* Are there where clauses: *)
          let user_complement_section = match w with None -> [] | Some w -> w in

          (* Port names (inputs + outputs) *)
          let is_source = (input_ports = []) in
          let inputs  = List.map fst input_ports  in
          let outputs = List.map fst output_ports in
          let port_names = List.append inputs outputs in
          let () = check_unicity port_names in

          let ancestor = match input_ports, output_ports with
            | [],[] ->  failwith "Autistic chips not allowed!"
            | [], _ -> "source"
            | _ ,[] -> "sink"
            | _ ,_  -> "relay"
          in

          (* Support functions and values *)

          let set_alone_application  a = <:expr< self#$lid:"set_alone_" ^a$ $lid:a$ >>  in
          let connect_application    a = <:expr< self#$lid:"connect_"   ^a$ $lid:a$ >>  in
          let disconnect_application a = <:expr< self#$lid:"disconnect_"^a$ >>          in

	  (* Class type variables *)
          let ctv =
            let type_variables = List.map (fun i -> <:ctyp< '$lid:i$ >>) port_names in
            Ast.tyCom_of_list type_variables
          in

          (* Inherit section *)
          let inherit_section =  [ <:class_str_item< inherit $lid:ancestor$ ?name system >> ] in

          (* Tool for creating a pattern depending on the length of a list of identifiers
             encoded by strings. If the list is empty, the pattern () is built. If the list is
             a singleton, the pattern is simply itself. If the list has almost two elements,
             a tuple pattern is created. *)
          let pattern_of_string_list = function
           | []  -> <:patt< () >>
 	   | [x] -> <:patt< $lid:x$>>
 	   |  l  -> Ast.PaTup (_loc, Ast.paCom_of_list (List.map (fun x-> <:patt< $lid:x$>>) l))
          in

          let type_of_ctyp_list = function
           | []  -> <:ctyp< unit >>
 	   | [t] -> t
 	   |  l  -> Ast.TyTup (_loc, Ast.tySta_of_list l)
          in

          (* Firmware section *)

          let domains =
            let mill =
             fun (x,ot) -> match ot with None -> Ast.TyLab(_loc,x,<:ctyp< _ >>) | Some t -> Ast.TyLab(_loc,x,t) in
            List.map mill input_ports
          in

          let codomain =
            let mill = fun (_,ot) -> match ot with None -> <:ctyp< _ >> | Some t -> t in
            type_of_ctyp_list (List.map mill output_ports)
          in

          let signature = lambda_ctyp _loc domains codomain in

          let firmware =
            let pl = List.map (fun i -> Ast.PaLab (_loc, i, <:patt< >>)) inputs             in
            let firmware_expr   = List.fold_right (fun p e -> <:expr< fun $p$ -> $e$ >>) pl e in
            let firmware_method =
              match is_source with
              | false -> <:class_str_item< method firmware : $signature$ = $firmware_expr$ >>
              | true  -> <:class_str_item< method firmware = $firmware_expr$ >>
            in
            [firmware_method]
          in

          (* in_port fields section *)
          let in_port_fields =
            let mill a = <:class_str_item< val $lid:a$ = (new in_port ~name:$str:a$ ?wire:$lid:a$ ()) >> in
            List.map mill inputs
          in

          (* out_port fields section *)
          let out_port_fields =
            let mill x = <:class_str_item< val $lid:x$ = (let wire = $lid:x$ in new out_port ~name:$str:x$ ?wire ()) >> in
            List.map mill outputs
          in

          let port_names_fields =
            let  in_port_names = string_list_of_string_list _loc inputs  in
            let out_port_names = string_list_of_string_list _loc outputs in
            [ <:class_str_item< val  in_port_names =  $in_port_names$ >> ;
              <:class_str_item< val out_port_names = $out_port_names$ >> ]
          in

          (* connect_? methods for input ports *)
          let connect_i =
            let mill a =
	     let b  = fresh_var_name ~blacklist:port_names ~prefix:a in
             let tv = <:ctyp< '$lid:a$ >> in
             let tv'= <:ctyp< '$lid:b$ >> in
	     let t  = <:ctyp< ($tv'$,$tv$) wire -> unit >> in
	     let method_type = Ast.TyPol (_loc, Ast.TyQuo(_loc,b), t) in
             <:class_str_item< method $lid:"connect_"^a$ : $method_type$ = $lid:a$#connect >> in
            List.map mill inputs
          in

          (* connect_? methods for output ports *)
          let connect_o =
            let mill a =
	     let b  = fresh_var_name ~blacklist:port_names ~prefix:a in
             let tv = <:ctyp< '$lid:a$ >> in
             let tv'= <:ctyp< '$lid:b$ >> in
	     let t   = <:ctyp< ($tv$,$tv'$) wire -> unit >> in
	     let method_type = Ast.TyPol (_loc, Ast.TyQuo(_loc,b), t) in
             <:class_str_item< method $lid:"connect_"^a$ : $method_type$ = $lid:a$#connect >> in
            List.map mill outputs
          in

          (* connect method *)
          let connect =
	   let ivl = fresh_var_names ~blacklist:port_names       ~prefixes:inputs  in
	   let ovl = fresh_var_names ~blacklist:(port_names@ivl) ~prefixes:outputs in
	   let wire_type a b =
             let va = <:ctyp< '$lid:a$ >> in
             let vb = <:ctyp< '$lid:b$ >> in
	     <:ctyp< ($va$,$vb$) wire >>
	   in
	   let it = type_of_ctyp_list (List.map2 wire_type ivl inputs) in
	   let ot = type_of_ctyp_list (List.map2 wire_type outputs ovl) in
	   let qivl = List.map (fun x -> Ast.TyQuo(_loc,x)) ivl in
	   let qovl = List.map (fun x -> Ast.TyQuo(_loc,x)) ovl in
	   let pi = pattern_of_string_list inputs  in
	   let po = pattern_of_string_list outputs in
           let connect_actions = Ast.exSem_of_list (List.map connect_application port_names) in
           match inputs, outputs with
           | [] , [] -> assert false
           |  _ , [] ->
	      let i_forall_vars = List.fold_left (fun x y -> Ast.TyApp (_loc,x,y)) (List.hd qivl) (List.tl qivl) in
	      let method_type = Ast.TyPol (_loc, i_forall_vars, <:ctyp< ($it$ -> unit)>>) in
	      [ <:class_str_item< method connect : $method_type$ = function $pi$ -> begin $connect_actions$ end >> ]

	   | [] , _  ->
	      let o_forall_vars = List.fold_left (fun x y -> Ast.TyApp (_loc,x,y)) (List.hd qovl) (List.tl qovl) in
	      let method_type = Ast.TyPol (_loc, o_forall_vars, <:ctyp< ($ot$ -> unit)>>) in
	      [ <:class_str_item< method connect : $method_type$ = function $po$ -> begin $connect_actions$ end >> ]

	   |  _ , _  ->
	      let qvl = List.append qivl qovl in
	      let forall_vars = List.fold_left (fun x y -> Ast.TyApp (_loc,x,y)) (List.hd qvl) (List.tl qvl) in
	      let method_type = Ast.TyPol (_loc, forall_vars, <:ctyp< ($it$ -> $ot$ -> unit)>>) in
	      [ <:class_str_item< method connect : $method_type$ = function $pi$ -> function $po$ -> begin $connect_actions$ end >> ]
          in

	  (* disconnect_? methods for all ports *)
          let disconnect_x =
            let mill a = <:class_str_item< method $lid:"disconnect_"^a$ = $lid:a$#disconnect >> in
            List.map mill port_names
          in

          (* disconnect method *)
          let disconnect =
           let disconnect_actions = Ast.exSem_of_list (List.map disconnect_application port_names)
            in
            [ <:class_str_item<
             method disconnect =
              begin $disconnect_actions$ end
            >> ]
          in

          let get_wire_connections =
            let connections_i =
             let mill a =
              <:expr< ($str:a$, $lid:a$#connection#map (fun x -> x#readonly_wire#as_common)) >> in
             List.map mill inputs
            in
            let connections_o =
             let mill a =
              <:expr< ($str:a$, $lid:a$#connection#map (fun x -> x#writeonly_wire#as_common)) >> in
             List.map mill outputs
            in
            let il = expr_of_expr_list _loc connections_i in
            let ol = expr_of_expr_list _loc connections_o in
            [ <:class_str_item< method $lid:"input_wire_connections"$  = $il$ >> ;
              <:class_str_item< method $lid:"output_wire_connections"$ = $ol$ >> ]
          in

          (* set_alone_? methods for output ports *)
          let set_alone_o =
            let mill a =
             <:class_str_item< method private $lid:"set_alone_"^a$ = $lid:a$#connection#extract#writeonly_wire#set_alone >> in
            List.map mill outputs
          in

          (* stabilize method section *)
          let stabilize =
	    let unchanged_inputs = List.map (fun a -> fresh_var_name ~blacklist:port_names ~prefix:("unchanged_"^a)) inputs in

            let input_bindings =
              let mill (a,unchanged_a) =
                let p = <:patt< ($lid:a$ , $lid:unchanged_a$) >> in
                <:binding< $p$ = ($lid:a$#read_and_compare) >>
              in Ast.biAnd_of_list (List.map mill (List.combine inputs unchanged_inputs))
            in

            let unchanged_test =
              let l = (List.map (fun unchanged_a -> <:expr< $lid:unchanged_a$ >>) unchanged_inputs) in
              List.fold_right (fun e1 e2 -> <:expr< $e1$ && $e2$ >>) l <:expr< true >>
            in

            let output_bindings =
             let inputs_as_lid_expressions = List.map (fun x -> <:expr< $lid:x$>>) inputs            in
             let firmware_application = apply _loc <:expr< self#firmware>> inputs_as_lid_expressions in
 	     let output_pattern = pattern_of_string_list outputs in
 	     <:binding< $output_pattern$ = $firmware_application$ >>
            in

            let set_actions =
             Ast.exSem_of_list (List.map set_alone_application outputs)
            in

            [ <:class_str_item<
             method stabilize : performed =
               let $binding:input_bindings$ in
                if ($unchanged_test$)
                then
                 (Performed false)
                else
                 let $binding:output_bindings$ in
                  $set_actions$ ;
                 (Performed true)
            >>]

          in

          (* emit method section (for sources) *)
          let emit =

            let arg = fresh_var_name ~blacklist:port_names ~prefix:"arg" in

            let output_bindings =
             let inputs_as_lid_expressions = List.map (fun x -> <:expr< $lid:x$>>) inputs            in
             let firmware_application = apply _loc <:expr< self#firmware $lid:arg$ >> inputs_as_lid_expressions in
             let output_pattern = pattern_of_string_list outputs in
             <:binding< $output_pattern$ = $firmware_application$ >>
            in

            let set_actions = Ast.exSem_of_list (List.map set_alone_application outputs)
            in

            [ <:class_str_item<
             method emit $lid:arg$ =
                 let actions () =
                   let $binding:output_bindings$ in
                   $set_actions$ ;
                   self#system#stabilize
                 in self#system#mutex_methods#with_mutex actions
            >>]

          in

          (* Merging sections *)
          let cst = Ast.crSem_of_list
           (List.concat
            [ inherit_section   ;
              firmware          ;
              in_port_fields    ;
              out_port_fields   ;
              port_names_fields ;
              connect_i         ;
              connect_o         ;
              connect           ;
              disconnect_x      ;
              disconnect        ;
              get_wire_connections;
              set_alone_o       ;
              if is_source then emit else stabilize ;
              user_complement_section ;
            ])
           in

          (* Class expression *)

          let ce =
            <:class_expr<
             fun ?(system : system = (Chip.get_or_initialize_current_system ())) () -> object (self) $cst$ end >> in

          (* Class expression with optional parameters *)
          let cop_list = List.map (fun i -> Ast.PaOlb (_loc, i, <:patt< >>)) port_names in
          let cewop = List.fold_right (fun op ce -> <:class_expr< fun $op$ -> $ce$ >>) cop_list ce in
          let cewop = <:class_expr< fun ?name ?(equality=(Pervasives.(=))) -> $cewop$ >> in
          let cewop_with_user_parameters = lambda_class_expr _loc user_parameters cewop in
          let internal_module_name = ("Chip_class_definition_"^class_name) in
          let class_def = match is_virtual with
          | false -> <:str_item< class         [ $ctv$ ] $lid:class_name$ = $cewop_with_user_parameters$ >>
          | true  -> <:str_item< class virtual [ $ctv$ ] $lid:class_name$ = $cewop_with_user_parameters$ >>
          in
          <:str_item<

            module $uid:internal_module_name$ = struct
             open Chip
             $class_def$
            end
            include $uid:internal_module_name$
          >>

         ] ]
    ;

  END

end

let module M = Register.OCamlSyntaxExtension (Id) (Make) in ()
