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


open Camlp4 (* -*- camlp4o -*- *)

(* ocamlc -I +camlp4 -pp camlp4of.opt camlp4lib.cma chip_parser.p4.ml *)

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

  (* Make a multiple abstraction to a class_expr. *)
  let lambda_class_expr _loc xs body = List.fold_right (fun x b -> <:class_expr< fun $x$ -> $b$>>) xs body

  (* Make a multiple abstraction to a type. *)
  let lambda_ctyp _loc xs body = List.fold_right (fun x b -> <:ctyp< $x$ -> $b$>>) xs body

  (* Make a fresh identifier (string). *)
  let fresh_var_name ~blacklist ~prefix =
   let rec loop n =
    let candidate = (prefix^(string_of_int n)) in
    if not (List.mem candidate blacklist) then candidate else loop (n+1)
   in
   if (List.mem prefix blacklist) then (loop 0) else prefix

  (* List.combine for 3 lists *)
  let rec combine3 l1 l2 l3 = match (l1,l2,l3) with
  | []    , []    , []     -> []
  | x1::r1, x2::r2, x3::r3 -> (x1,x2,x3)::(combine3 r1 r2 r3)
  | _ -> raise (Invalid_argument "combine3")

  (* Check unicity in a string list. Raises a failure with the duplicated string. *)
  let rec check_unicity = function
  | []   -> ()
  | x::r -> if (List.mem x r) then failwith ("Duplicated port name '"^x^"'")
  		              else (check_unicity r)


  EXTEND Gram
    GLOBAL: str_item;

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

          e = expr ->

          (* Class type *)
          let is_virtual = (virt <> None) in

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

          let (pat_w, exp_w) =
            let str = (fresh_var_name ~blacklist:port_names ~prefix:"w") in
            (<:patt< $lid:str$ >> , <:expr< $lid:str$ >>)
          in

          let (pat_v, exp_v) =
            let str = (fresh_var_name ~blacklist:port_names ~prefix:"v") in
            (<:patt< $lid:str$ >> , <:expr< $lid:str$ >>)
          in

          (* Class type variables *)
          let ctv =
            let type_variables = List.map (fun i -> <:ctyp< '$lid:i$ >>) port_names in
            Ast.tyCom_of_list type_variables
          in

          (* Inherit section *)
          let inherit_section =  [ <:class_str_item< inherit $lid:ancestor$ ~name system >> ] in

          (* Tool for creating a pattern depending on the length of a list of identifiers
             encoded by strings. If the list is empty, the pattern () is built. If the list is
             a singleton, the pattern is simply itself. If the list has almost two elements,
             a tuple pattern is created. *)
          let pattern_of_string_list = function
           | []  -> <:patt< () >>
 	   | [x] -> <:patt< $lid:x$>>
 	   |  l  -> Ast.PaTup (_loc, Ast.paCom_of_list (List.map (fun x-> <:patt< $lid:x$>>) l))
          in

          (* Similar to pattern_of_string_list, but for making an expression from a list of
             sub-expressions. *)
          let expression_of_expr_list = function
           | []  -> <:expr< () >>
 	   | [e] -> e
 	   |  l  -> Ast.ExTup (_loc, Ast.exCom_of_list l)
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
            let mill a = <:class_str_item< val mutable $lid:a$ = in_port_of_wire_option $lid:a$ >> in
            List.map mill inputs
          in

          (* out_port fields section *)
          let out_port_fields =
            let mill x = <:class_str_item< val mutable $lid:x$ = $lid:x$ >> in
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
             let tv = <:ctyp< '$lid:a$ >> in
             <:class_str_item< method $lid:"connect_"^a$ ($pat_w$ : ($tv$,_) wire) = $lid:a$ <- Some ($exp_w$,None) >> in
            List.map mill inputs
          in

          (* connect_? methods for output ports *)
          let connect_o =
            let mill a =
             let tv = <:ctyp< '$lid:a$ >> in <:class_str_item< method $lid:"connect_"^a$ ($pat_w$ : (_,$tv$) wire) = $lid:a$ <- Some $exp_w$ >> in
            List.map mill outputs
          in

          (* connect method *)
          let connect =
      	   let pi = pattern_of_string_list inputs  in
      	   let po = pattern_of_string_list outputs in
           let connect_actions = Ast.exSem_of_list (List.map connect_application port_names) in
           match inputs, outputs with
           | [] , [] -> assert false
           |  _ , [] -> [ <:class_str_item< method connect $pi$ = begin $connect_actions$ end >> ]
           | [] , _  -> [ <:class_str_item< method connect $po$ = begin $connect_actions$ end >> ]
	   |  _ , _  -> [ <:class_str_item< method connect $pi$ $po$ = begin $connect_actions$ end >> ]
          in

          (* disconnect_? methods for all ports *)
          let disconnect_x =
            let mill a = <:class_str_item< method $lid:"disconnect_"^a$ = $lid:a$ <- None >> in
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

          (* get_wire_? methods for input ports *)
          let get_wire_i =
            let mill a =
             let str = <:expr< $str:class_name^"#get_wire_"^a$ >> in
             <:class_str_item< method $lid:"get_wire_"^a$ = fst (extract ~caller:$str$ $lid:a$) >> in
            List.map mill inputs
          in

          (* get_wire_? methods for output ports *)
          let get_wire_o =
            let mill a =
             let str = <:expr< $str:class_name^"#get_wire_"^a$ >> in
             <:class_str_item< method $lid:"get_wire_"^a$ = extract ~caller:$str$ $lid:a$ >> in
            List.map mill outputs
          in

          (* set_in_port_? methods *)
          let set_in_port_i =
            let mill a =
             <:class_str_item< method $lid:"set_in_port_"^a$  $pat_w$ $pat_v$ = $lid:a$ <- Some ($exp_w$,(Some $exp_v$)) >> in
            List.map mill inputs
          in

          (* set_alone_? methods for input ports *)
          let set_alone_i =
            let mill a =
             let str = <:expr< $str:class_name^"#set_alone_"^a$ >> in
             <:class_str_item< method $lid:"set_alone_"^a$ $pat_v$ = (fst (extract ~caller:$str$ $lid:a$))#set_alone $exp_v$ >> in
            List.map mill inputs
          in

          (* set_alone_? methods for output ports *)
          let set_alone_o =
            let mill a =
             let str = <:expr< $str:class_name^"#set_alone_"^a$ >> in
             <:class_str_item< method $lid:"set_alone_"^a$ v = (extract ~caller:$str$ $lid:a$)#set_alone v >> in
            List.map mill outputs
          in

          (* stabilize method section *)
          let stabilize =
            let str = <:expr< $str:class_name^"#stabilize"$ >> in

	    let winputs          = List.map (fun a -> fresh_var_name ~blacklist:port_names ~prefix:("w"^a)) inputs in
	    let unchanged_inputs = List.map (fun a -> fresh_var_name ~blacklist:port_names ~prefix:("unchanged_"^a)) inputs in

            let input_bindings =
              let mill (wa,a,unchanged_a) =
                let p = <:patt< ($lid:wa$, $lid:a$ , $lid:unchanged_a$) >> in
                <:binding< $p$ = (extract_and_compare $str$ $lid:"equality"$ $lid:a$) >>
              in Ast.biAnd_of_list (List.map mill (combine3 winputs inputs unchanged_inputs))
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
             let set_in_port_application (wa,a) = <:expr< self#$lid:"set_in_port_"^a$ $lid:wa$ $lid:a$ >> in
             Ast.exSem_of_list ((List.map set_in_port_application (List.combine winputs inputs))
                               @(List.map set_alone_application outputs))
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
                 in self#system#with_mutex actions
            >>]

          in

          (* set method section *)
          let set =
      	   let input_pattern = pattern_of_string_list inputs in
           let set_alone_actions = Ast.exSem_of_list (List.map set_alone_application inputs)
            in
            [ <:class_str_item<
             method set $input_pattern$ =
              $set_alone_actions$ ;
              self#system#stabilize
            >> ]

          in

          (* get method section (outputs) *)
          let get =
           let mill a =
            let str = <:expr< $str:class_name^"#get (reading "^a^")"$ >> in
            <:expr< (extract ~caller:$str$ $lid:a$)#get_alone >> in
           let output_expression = expression_of_expr_list (List.map mill outputs)
           in [ <:class_str_item< method get = $output_expression$ >> ]

          in

          (* get_? methods for inputs *)
          let get_i =
            let mill a =
             let str = <:expr< $str:class_name^"#get_"^a$ >> in
             <:class_str_item< method $lid:"get_"^a$ = (fst (extract ~caller:$str$ $lid:a$))#get_alone >> in
            List.map mill inputs
          in

          (* get_? methods for outputs *)
          let get_o =
            let mill a =
             let str = <:expr< $str:class_name^"#get_"^a$ >> in
             <:class_str_item< method $lid:"get_"^a$ = (extract ~caller:$str$ $lid:a$)#get_alone >> in
            List.map mill outputs
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
              get_wire_i        ;
              get_wire_o        ;
              set_in_port_i     ;
              set_alone_i       ;
              set_alone_o       ;
              get               ;
              get_i             ;
              get_o             ;
              if is_source then emit else stabilize ;
              if is_source then [] else set       ;
            ])
           in

          (* Class expression *)

          let ce =
            <:class_expr<
             fun ?(system : system = (Chip.get_or_initialize_current_system ())) () -> object (self) $cst$ end >> in

          (* Class expression with optional parameters *)
          let cop_list = List.map (fun i -> Ast.PaOlb (_loc, i, <:patt< >>)) port_names in
          let cewop = List.fold_right (fun op ce -> <:class_expr< fun $op$ -> $ce$ >>) cop_list ce in
          let cewop = <:class_expr< fun ?(name=fresh_chip_name "chip") ?(equality=(Pervasives.(=))) -> $cewop$ >> in
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
