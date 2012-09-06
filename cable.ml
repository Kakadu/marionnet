(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2010  Jean-Vincent Loddo
   Copyright (C) 2010  Université Paris 13

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


(** "Cable" component implementation. *)

#load "where_p4.cmo"
;;

open Gettext
module Recursive_mutex = MutexExtra.Recursive

(* The type of data exchanged with the dialog: *)
module Data = struct
type t = {
  name                : string;
  label               : string;
  left_user_endpoint  : string * string;
  right_user_endpoint : string * string;
  old_name            : string;
  }

let to_string t = "<obj>" (* TODO? *)
end (* Data *)

module Make_menus
 (Params : sig
    val st      : State.globalState
    val packing : [ `toolbar of GButton.toolbar | `menu_parent of Menu_factory.menu_parent ]
 end)
 (Cablekind : sig
    val crossover:bool
 end)
= struct

  open Params
  open Cablekind

  module Toolbar_entry = struct

   let imagefile = match crossover with
      | false -> "ico.cable.direct.palette.png"
      | true  -> "ico.cable.crossed.palette.png"

   let tooltip = match crossover with
      | false -> (s_ "Straight cable")
      | true  -> (s_ "Crossover cable")

   let packing   = Params.packing

  end (* Toolbar_entry *)

  module Add = struct
    include Data

    let key = match crossover with
      | false -> Some GdkKeysyms._D
      | true  -> Some GdkKeysyms._C

    let ok_callback t =
      Gui_bricks.Ok_callback.check_name t.name t.old_name st#network#name_exists t

    let dialog () =
      let (name, title) = match crossover with
      | false -> (st#network#suggestedName "d" , (s_ "Add straight cable"))
      | true  -> (st#network#suggestedName "c" , (s_ "Add crossover cable"))
      in
    Dialog_add_or_update.make ~network:st#network ~title ~name ~crossover ~ok_callback ()

    let reaction (r:Data.t) =
      let action () =
        ignore
          (new User_level_cable.cable
                 ~network:st#network
                 ~crossover
                 ~name:r.name
                 ~label:r.label
                 ~left_user_endpoint:r.left_user_endpoint
                 ~right_user_endpoint:r.right_user_endpoint
                 ())
      in
      st#network_change action ();

  end

  module Properties = struct
    include Data

    let dynlist () = match crossover with
    | false -> st#network#get_direct_cable_names
    | true  -> st#network#get_crossover_cable_names

    let dialog name () =
     let c = (st#network#get_cable_by_name name) in
     let c = ((Obj.magic c):> User_level_cable.cable) in
     let title = match crossover with
      | false -> ((s_ "Modify straight cable")^" "^name)
      | true  -> ((s_ "Modify crossover cable")^" "^name)
     in
     let label = c#get_label in
     let left_user_endpoint  = (c#get_left#node#get_name,  c#get_left#user_port_name) in
     let right_user_endpoint = (c#get_right#node#get_name, c#get_right#user_port_name) in
     Log.printf "Calling Dialog_add_or_update with (%s,%s) (%s,%s)\n"
       c#get_left#node#get_name  c#get_left#user_port_name
       c#get_right#node#get_name c#get_right#user_port_name
       ;
     Dialog_add_or_update.make
       ~network:st#network
       ~title
       ~name
       ~label
       ~left_user_endpoint
       ~right_user_endpoint
       ~crossover
       ~ok_callback:Add.ok_callback
       ()

    let reaction r =
      let c = (st#network#get_cable_by_name r.old_name) in
      let c = ((Obj.magic c):> User_level_cable.cable) in
      (* Make a new cable; it should have a different identity from the old one, and it's
         important that it's initialized anew, to get the reference counter right: *)
      c#destroy;
      Add.reaction r;

  end

  module Remove = struct
    type t = string (* just the name *)
    let to_string = (Printf.sprintf "name = %s\n")

    let dynlist = Properties.dynlist

    let dialog name () =
      let question = match crossover with
       | false -> Printf.sprintf (f_ "Are you sure that you want to remove the straight cable %s?") name
       | true  -> Printf.sprintf (f_ "Are you sure that you want to remove the crossover cable %s?") name
      in
      Gui_bricks.Dialog.yes_or_cancel_question
        ~title:(s_ "Remove")
        ~markup:question
        ~context:name
        ()

    let reaction name =
      let c = (st#network#get_cable_by_name name) in
      let c = ((Obj.magic c):> User_level_cable.cable) in
      let action () = c#destroy in
      st#network_change action ();

  end

  module Disconnect = struct
    type t = string (* just the name *)
    let to_string = (Printf.sprintf "name = %s\n")

    let dynlist () =
      List.filter
        (fun x -> (st#network#get_cable_by_name x)#can_suspend)
        (Properties.dynlist ())

    let dialog = Menu_factory.no_dialog_but_simply_return_name
    let reaction name = (st#network#get_cable_by_name name)#suspend

  end

  module Reconnect = struct
    type t = string (* just the name *)
    let to_string = (Printf.sprintf "name = %s\n")

    let dynlist () =
      List.filter
        (fun x -> (st#network#get_cable_by_name x)#can_resume)
        (Properties.dynlist ())

    let dialog = Menu_factory.no_dialog_but_simply_return_name
    let reaction name = (st#network#get_cable_by_name name)#resume

  end

 module Create_entries =
  Gui_toolbar_COMPONENTS_layouts.Layout_for_network_edge (Params) (Toolbar_entry) (Add) (Properties) (Remove) (Disconnect) (Reconnect)

 (* Subscribe this kind of component to the network club: *)
 st#network#subscribe_a_try_to_add_procedure Eval_forest_child.try_to_add_cable;

end

(*-----*)
  WHERE
(*-----*)

module Dialog_add_or_update = struct

let make
 ~(network:User_level.network)
 ?(title="Add cable")
 ?(name="")
 ?label
 ?(left_user_endpoint:  (string * string) option)
 ?(right_user_endpoint: (string * string) option)
 ?(crossover=false)
 ?(help_callback=help_callback ~crossover) (* defined backward with "WHERE" *)
 ?(ok_callback=(fun data -> Some data))
 () :'result option =
  let dialog_image_file =
   Filename.concat
     Initialization.Path.images
     (match crossover with
     | false -> "ico.cable.direct.dialog.png"
     | true  -> "ico.cable.crossed.dialog.png")
  in
  let link_image_file =
   Filename.concat
     Initialization.Path.images
     (match crossover with
     | false -> "ico.cable.direct.dialog.link.png"
     | true  -> "ico.cable.crossed.dialog.link.png")
  in
  let image_tooltip =
    match crossover with
    | false -> (s_ "Straight cable")
    | true  -> (s_ "Crossover cable")
  in
  let name_tooltip =
    match crossover with
    | false ->
       (s_ "Straight cable name. This name must be unique in the virtual network. Suggested: d1, d2, ... ")
    | true  ->
       (s_ "Crossover cable name. This name must be unique in the virtual network. Suggested: c1, c2, ... ")
  in
  let old_name = name in
  let (w,_,name,label) =
    Gui_bricks.Dialog_add_or_update.make_window_image_name_and_label
      ~title
      ~image_file:dialog_image_file
      ~image_tooltip
      ~name
      ~name_tooltip
      ?label
      ()
  in
  let cable_input_widget =
    let vbox = GPack.vbox ~homogeneous:false ~border_width:20 ~spacing:10 ~packing:w#vbox#add () in
    let table = GPack.table ~rows:3 ~columns:4 ~row_spacings:10 ~col_spacings:20 ~homogeneous:false ~packing:vbox#add (*why not w#vbox#add *)() in
    let lname = GMisc.label ~xalign:0. ~text:(s_ "Name") ~packing:(table#attach ~left:0 ~top:1) () in
    let lport = GMisc.label ~xalign:0. ~text:(s_ "Port") ~packing:(table#attach ~left:0 ~top:2) () in
    let lfrom = GMisc.label ~xalign:0. ~text:(s_ "From") ~packing:(table#attach ~left:1 ~top:0) () in
    let lto   = GMisc.label ~xalign:0. ~text:(s_ "To")   ~packing:(table#attach ~left:3 ~top:0) () in
    let _link = GMisc.image ~file:link_image_file ~xalign:0.5 ~packing:(table#attach ~left:2 ~top:2) () in
    let _ = GMisc.image ~stock:`GO_FORWARD ~packing:(table#attach ~left:2 ~top:1) () in
    let tooltips = Gui_bricks.make_tooltips_for_container table in
    tooltips lname#coerce (s_ "Node name");
    tooltips lport#coerce (s_ "Ethernet port");
    tooltips lfrom#coerce (s_ "The first network node connected to the cable");
    tooltips lto#coerce   (s_ "The second network node connected to the cable");
    let decompose = function None -> (None,None) | Some (x,y) -> (Some x, Some y) in
    let (n0,p0) = decompose left_user_endpoint in
    let (n1,p1) = decompose right_user_endpoint in
    let force_to_be_included : ((string*string) list) =
      ListExtra.filter_map (fun e->e) [left_user_endpoint; right_user_endpoint]
    in
    let updating = (force_to_be_included <> []) in
    List.iter (fun (n,p) -> Log.printf "Forced to be included: (%s,%s)\n" n p) force_to_be_included;
    let free_node_port_list as xys = network#free_endpoint_list_humanly_speaking ~force_to_be_included in
    let ((n0,p0),(n1,p1)) = match updating with
    | true  -> ((n0,p0),(n1,p1))
    | false -> Gui_bricks.Reactive_widget.guess_humanly_speaking_enpoints xys n0 p0 n1 p1
    in
    new Gui_bricks.Reactive_widget.cable_input_widget
	  (*~width:100 ?height*)
	  ?n0 ?p0 ?n1 ?p1
	  ~packing_n0:(table#attach ~left:1 ~top:1)
	  ~packing_p0:(table#attach ~left:1 ~top:2)
	  ~packing_n1:(table#attach ~left:3 ~top:1)
	  ~packing_p1:(table#attach ~left:3 ~top:2)
	  ~free_node_port_list
	  ()
  in

  let get_widget_data () :'result =
    let ((n0,p0),(n1,p1)) = cable_input_widget#get_widget_data in
    let name = name#text in
    let label = label#text in
      { Data.name = name;
        Data.label = label;
        Data.left_user_endpoint  = ((Option.extract n0),(Option.extract p0));
        Data.right_user_endpoint = ((Option.extract n1),(Option.extract p1));
        Data.old_name = old_name;
        }
  in
  (* The result of make is the result of the dialog loop (of type 'result option): *)
  Gui_bricks.Dialog_run.ok_or_cancel w ~ok_callback ~help_callback ~get_widget_data ()


(*-----*)
  WHERE
(*-----*)

 let rec help_callback_straight () =
   let title = (s_ "ADD OR MODIFY A STRAIGHT CABLE") in
   let msg   = (s_ "\
In this dialog window you can define the name of a straight Ethernet cable \
and set its parameters:\n\n\
- Label: a string appearing near the edge representing the cable in the \
network graph\n\n\
- Devices: the two network devices (machine, hub, ...) linked by the cable and \
their two connected interfaces\n\n\
WARNING: this dialog allows to define straight cables even in contexts where \
they won't work (for example between two machines); allowing users to define 'wrong' \
connections may be of some pedagogical interest.")
   in Simple_dialogs.help title msg

 and help_callback_crossover () =
   let title = (s_ "ADD OR MODIFY A CROSSOVER CABLE") in
   let msg   = (s_ "\
In this dialog window you can define the name of a crossover Ethernet cable \
and set its parameters:\n\n\
- Label: a string appearing near the edge representing the cable in the \
network graph\n\n\
- Devices: the two network devices (machine, hub, ...) linked by the cable and \
their two connected interfaces\n\n\
WARNING: this dialog allows to define crossover cables even in contexts where \
they won't work (for example between two machines); allowing users to define 'wrong' \
connections may be of some pedagogical interest.")
   in Simple_dialogs.help title msg

 and help_callback ~crossover =
   match crossover with
   | false -> help_callback_straight ()
   | true  -> help_callback_crossover () ;;

end

(*-----*)
  WHERE
(*-----*)


module Eval_forest_child = struct

 let try_to_add_cable (network:User_level.network) (f:Xforest.tree) =
  try
   (match f with
   | Forest.NonEmpty (("cable", attrs) , childs , Forest.Empty) ->
	(* Cables represent a special case: they must be builded knowing their endpoints. *)
	let name = List.assoc "name"    attrs in
        Log.printf "Importing cable \"%s\"...\n" name;
	let ln = List.assoc "leftnodename"    attrs in
	let lr = List.assoc "leftreceptname"  attrs in
	let rn = List.assoc "rightnodename"   attrs in
	let rr = List.assoc "rightreceptname" attrs in
	let crossover =
 	  try (List.assoc "kind" attrs) = "crossover" (* backward-compatibility *)
 	  with Not_found -> bool_of_string (List.assoc "crossover" attrs)
 	in
	let x =
	  new User_level_cable.cable ~network ~crossover ~name
	    ~left_user_endpoint:(ln,lr)
	    ~right_user_endpoint:(rn,rr)
	    ()
        in
        x#from_forest ("cable", attrs) childs ;
        Log.printf "Cable \"%s\" successfully imported.\n" name;
        true
   | _ ->
        false
   )
  with _ -> false

end (* module Eval_forest_child *)


(*-----*)
  WHERE
(*-----*)


module User_level_cable = struct

class virtual cable_dot_zone ?(reversed=false) ~(motherboard:Motherboard.t) () =
 object (self)

   method virtual crossover : bool

   val reversed =
     Chip.wref_in_cable
       ~name:"reversed"
       ~cable:motherboard#reversed_rj45cables_cable
       reversed

   method is_reversed = reversed#get
   method set_reversed b = reversed#set b

   initializer
      self#add_destroy_callback (lazy (reversed#destroy ()));

   method dot_color = match self#crossover with
   | false -> "#949494"
   | true  -> "#6d8dc0"

   method dot_traduction (*?(edgeoptions="")*) ~labeldistance =
    let edgeoptions="" in
    let labeldistance_base = labeldistance in
    let n1 = self#get_left#node in
    let r1 = self#get_left#user_port_name in
    let n2 = self#get_right#node in
    let r2 = self#get_right#user_port_name in
    begin
    let vertexlab node iden recept =
      let port      = node#dotPortForEdges  recept in
      let portlabel = node#dotLabelForEdges recept in
      match port,portlabel with
      | "",""  -> ("")
      | _ , "" -> (","^iden^"=\""^port^"\"")
      | _ , _  ->
          begin
          let port_line      = (StringExtra.assemble_if_not_empty ~prefix:"<TR><TD>" ~suffix:"</TD></TR>" port ) in
          let portlabel_line = (StringExtra.assemble_if_not_empty ~prefix:"<TR><TD><FONT COLOR=\"#3a3936\">" ~suffix:"</FONT></TD></TR>" portlabel) in
(","^iden^"=<
<TABLE BORDER=\"0\" CELLBORDER=\"0\" CELLSPACING=\"0\" CELLPADDING=\"0\">
"^port_line^"
"^portlabel_line^"
</TABLE>
>")       end
    in
    let labeldistance =
      begin
      let p1  = n1#dotPortForEdges  r1 in
      let pl1 = n1#dotLabelForEdges r1 in
      let p2  = n2#dotPortForEdges  r2 in
      let pl2 = n2#dotLabelForEdges r2 in
      (* if there is a vertex with both port and portlabel not empty => set labeldistance +0.5 *)
      if ((p1<>"" && pl1<>"") or (p2<>"" && pl2<>""))
      then ("labeldistance="^(string_of_float (labeldistance_base +. 0.5))^",") else ""
      end
    in
    let (tail, head, taillabel, headlabel) =
      (* Reverse left and right sides of the cable if required *)
      let (n1,r1,n2,r2) = if self#is_reversed then (n2,r2,n1,r1) else (n1,r1,n2,r2)
      in
      match (n1#get_label, n2#get_label) with
      | "", "" -> (n1#get_name^":img"), (n2#get_name^":img"), (vertexlab n1 "taillabel" r1), (vertexlab n2 "headlabel" r2)
      | "", l2 -> (n1#get_name^":img"), (n2#get_name)       , (vertexlab n1 "taillabel" r1), (vertexlab n2 "headlabel" r2)
      | l1, "" -> (n1#get_name)       , (n2#get_name^":img"), (vertexlab n1 "taillabel" r1), (vertexlab n2 "headlabel" r2)
      | l1, l2 -> (n1#get_name)       , (n2#get_name)       , (vertexlab n1 "taillabel" r1), (vertexlab n2 "headlabel" r2)
    in
    let edgeoptions = if edgeoptions = "" then "" else (edgeoptions^",") in
    let cable_label = self#get_name ^ (if self#get_label = "" then "" else ("  "^self#get_label)) in
    let edgeoptions = edgeoptions ^ "arrowhead=obox, arrowtail=obox, arrowsize=0.4," ^
      (if self#is_connected then "" else "style=dashed,")
    in
    let label_color = self#dot_color in
    (tail^" -> "^head^" ["^edgeoptions^labeldistance^"label=<<FONT COLOR=\""^label_color^"\">"^cable_label^"</FONT>>"^taillabel^headlabel^"];")
  end (* method dot_traduction *)


 end (* cable_dot_tuning_zone *)


and virtual cable_defects_zone ~network () =
 object (self)

  method virtual defects_cable_type : string
  method virtual get_name : string
  method virtual get_left  : endpoint
  method virtual get_right : endpoint
  method virtual add_destroy_callback : unit Lazy.t -> unit

  method private add_my_defects =
   match
     (network#defects:Treeview_defects.t)#unique_row_exists_with_binding "Name" self#get_name
   with
   | true -> Log.printf "The cable %s has already defects defined...\n" self#get_name
   | false ->
       network#defects#add_cable
         ~cable_name:self#get_name
         ~cable_type:self#defects_cable_type
         ~left_name:self#get_left#my_name_in_treeview_defects
         ~right_name:self#get_right#my_name_in_treeview_defects
         ()

  method private destroy_my_defects =
    Log.printf "component \"%s\": destroying my defects.\n" self#get_name;
    network#defects#remove_subtree_by_name self#get_name;

  initializer
    self#add_my_defects;
    self#add_destroy_callback (lazy self#destroy_my_defects);

end

and virtual ledgrid_management_zone ~network () =
 object (self)

  method virtual get_left  : endpoint
  method virtual get_right : endpoint
  method virtual add_destroy_callback : unit Lazy.t -> unit

  method private set_ports_connection_state value =
   begin
    let left  = self#get_left in
    let right = self#get_right in
    if left#node#has_ledgrid then
    network#ledgrid_manager#set_port_connection_state
      ~id:(left#node#id)
      ~port:left#port_index
      ~value
      ();
    if right#node#has_ledgrid then
    network#ledgrid_manager#set_port_connection_state
      ~id:(right#node#id)
      ~port:right#port_index
      ~value
      ();
   end

  initializer
    self#set_ports_connection_state true;
    self#add_destroy_callback (lazy (self#set_ports_connection_state false));

end


(** Essentially a triple:  (node, port_index, direction) *)
and endpoint
 ~(node:User_level.node)
 ~(port_index:int)
 ~(direction:[ `leftward | `rightward ])
 =
 (* direction interpreted as defined in the treeview: *)
 let direction = match direction with
 | `rightward -> Treeview_defects.LeftToRight
 | `leftward  -> Treeview_defects.RightToLeft
 in
 object (self)
  method node = node
  method port_index = port_index

  (* Initialized by the owner cable itself: *)
  val mutable owner : < get_name : string > option = None
  method set_owner x = owner <- Some x
  method owner = Option.extract owner

  method user_port_name =
    node#ports_card#user_port_name_of_internal_index port_index

  method user_port_index =
    node#ports_card#user_port_index_of_internal_index port_index

  (* Just a type conversion, as a pair: *)
  method involved_node_and_port_index = (node, port_index)

  method get_my_defects =
    let defects = Treeview_defects.extract () in
    let get = defects#get_cable_attribute self#owner#get_name direction in
    object
      method loss        = get "Loss %"
      method duplication = get "Duplication %"
      method flip        = get "Flipped bits %"
      method min_delay   = get "Minimum delay (ms)"
      method max_delay   = get "Maximum delay (ms)"
    end

  method my_name_in_treeview_defects =
    let node_name = self#node#get_name in
    let port_name = self#user_port_name in
    Printf.sprintf "to %s (%s)" node_name port_name

 end (* class endpoint *)

(** A cable defines an edge in the network graph.
    Defects may be added after creation. *)
and cable =
   fun
   ~network
   ~name
   ?label
   ~crossover
   ~(left_user_endpoint:  string * string)
   ~(right_user_endpoint: string * string)
   () ->
  let (n0,p0) = left_user_endpoint in
  let (n1,p1) = right_user_endpoint in
  let make_endpoint_of ~node_name ~user_port_name ~direction =
    let node = network#get_node_by_name node_name in
    let port_index = node#ports_card#internal_index_of_user_port_name user_port_name in
    (new endpoint ~node ~port_index ~direction)
  in
  let left_endpoint  = make_endpoint_of ~node_name:n0 ~user_port_name:p0 ~direction:`leftward in
  let right_endpoint = make_endpoint_of ~node_name:n1 ~user_port_name:p1 ~direction:`rightward in
  let defects_cable_type = match crossover with
    | false -> "direct"
    | true  -> "crossover"
  in
  let network_alias = network in
  let network_alias1 = network in
  let network_alias2 = network
  in
  object (self)
  initializer
    left_endpoint#set_owner  (self :> <get_name:string>);
    right_endpoint#set_owner (self :> <get_name:string>);

  inherit OoExtra.destroy_methods ()
  inherit User_level.component ~network ~name ?label ()
  inherit [cable] User_level.simulated_device () as self_as_simulated_device

  initializer
    network#add_cable (self :> User_level.cable);
    self#add_destroy_callback (lazy (network#del_cable self#get_name));

  inherit cable_defects_zone ~network:network_alias () as cable_defects_zone
  inherit cable_dot_zone ~motherboard:network_alias1#motherboard ()
  inherit ledgrid_management_zone ~network:network_alias2 ()

  method crossover = crossover

  (* Redefinition: *)
  method is_correct =
    let polarity0 = self#get_left#node#polarity  in
    let polarity1 = self#get_right#node#polarity in
    let module M = User_level in
    (* We need a crossover cable if the polarity is the same: *)
    match polarity0, polarity1 with
     | M.Intelligent , _        | _     , M.Intelligent -> true
     | M.MDI_X       , M.MDI    | M.MDI , M.MDI_X       -> (not crossover)
     | M.MDI_X       , M.MDI_X  | M.MDI , M.MDI         -> crossover

  method defects_cable_type = defects_cable_type

  (** A cable has two connected endpoints: *)
  val mutable left_endpoint  : endpoint = left_endpoint
  val mutable right_endpoint : endpoint = right_endpoint

  (** Accessors *)
  method get_left  = left_endpoint
  method get_right = right_endpoint
  method set_left  x = left_endpoint  <- x
  method set_right x = right_endpoint <- x

  (** The li st of two names of nodes (machine/device) linked by the cable *)
  method involved_node_names = [left_endpoint#node#name; right_endpoint#node#name]

  (** Is a node connected to something with this cable? *)
  method is_node_involved node_name =
    List.mem node_name self#involved_node_names

  (** Return the list of devices (i.e. hubs, switches or routers) directly linked to this cable: *)
  method involved_node_and_port_index_list =
    [ (left_endpoint#node, left_endpoint#port_index); (right_endpoint#node, right_endpoint#port_index) ]

  (** Show its definition. Useful for debugging. *)
  method show prefix =
    (prefix^self#name^" ("^(self#defects_cable_type)^")"^
    " ["^left_endpoint#node#name ^","^left_endpoint#user_port_name^"] -> "^
    " ["^right_endpoint#node#name^","^right_endpoint#user_port_name^"]")

  method to_forest =
    Forest.leaf ("cable",
		    [ ("name"            ,  self#get_name )  ;
		      ("label"           ,  self#get_label)  ;
		      ("crossover"       , (string_of_bool self#crossover)) ;
		      ("leftnodename"    ,  self#get_left#node#name)    ;
		      ("leftreceptname"  ,  self#get_left#user_port_name)  ;
		      ("rightnodename"   ,  self#get_right#node#name)   ;
		      ("rightreceptname" ,  self#get_right#user_port_name) ;
		    ])

  (** A cable has just attributes (no childs) in this version. The attribute "kind" cannot be set,
      must be considered as a constant field of the class. *)
  method eval_forest_attribute =
    function
      | ("name"            , x ) -> self#set_name  x
      | ("label"           , x ) -> self#set_label x
      | _ -> assert false

    (** A cable may be either connected or disconnected; it's connected by default: *)
    val connected = ref true

    (** Access method *)
    method is_connected = !connected

    (** Make the cable connected, or do nothing if it's already connected: *)
    method private connect_right_now =
      Recursive_mutex.with_mutex mutex
        (fun () ->
          (if not self#is_connected then begin
            Log.printf "Connecting the cable %s...\n" self#get_name;
            (* Turn on the relevant LEDgrid lights: *)
            let involved_node_and_port_index_list = self#involved_node_and_port_index_list in
            List.iter
              (fun (device, port) ->
                network#ledgrid_manager#set_port_connection_state
                  ~id:(device#id)
                  ~port
                  ~value:true
                  ())
              involved_node_and_port_index_list;
            connected := true;
            self#increment_alive_endpoint_no;
            Log.printf "Ok: connected\n";
          end);
          User_level.refresh_sketch ());

    (** Make the cable disconnected, or do nothing if it's already disconnected: *)
    method private disconnect_right_now =
      Recursive_mutex.with_mutex mutex
        (fun () ->
          (if self#is_connected then begin
            Log.printf "Disconnecting the cable %s...\n" self#get_name;
            (* Turn off the relevant LEDgrid lights: *)
            let involved_node_and_port_index_list = self#involved_node_and_port_index_list in
            List.iter
              (fun (device, port) ->
                network#ledgrid_manager#set_port_connection_state
                  ~id:(device#id)
                  ~port
                  ~value:false
                  ())
              involved_node_and_port_index_list;
            connected := false;
            self#decrement_alive_endpoint_no;
            Log.printf "Ok: disconnected\n";
          end);
          User_level.refresh_sketch ());

   (** 'Suspending means disconnecting for cables *)
   method suspend_right_now =
     self#disconnect_right_now

   (** 'Resuming' means connecting for cables *)
   method resume_right_now =
     self#connect_right_now

   (** An always up-to-date 'reference counter' storing the number of alive
       endpoints plus the cable connection state (either 0 for 'disconnected' or
       1 for 'connected'). A cable can be started in the simulation when this is
       exactly 3, and must be terminated when it becomes less than 3. *)
   val alive_endpoint_no = ref 1 (* cables are 'connected' by default *)

   (** Check that the reference counter is in [0, 3]. To do: disable this for
   production. *)
   method private check_alive_endpoint_no =
    Recursive_mutex.with_mutex mutex
      (fun () ->
        assert((!alive_endpoint_no >= 0) && (!alive_endpoint_no <= 3));
        Log.printf "The reference count is now %d\n" !alive_endpoint_no;
       )

   (** Record the fact that an endpoint has been created (at a lower level
       this means that its relevant {e hublet} has been created), and
       startup the simulated cable if appropriate. *)
   method increment_alive_endpoint_no =
     Recursive_mutex.with_mutex mutex
       (fun () ->
         Log.printf "Increment_alive_endpoint_no\n";
         self#check_alive_endpoint_no;
         alive_endpoint_no := !alive_endpoint_no + 1;
         self#check_alive_endpoint_no;
         if !alive_endpoint_no = 3 then begin
           Log.printf "The reference count raised to three: starting up a cable\n";
           self#startup_right_now
         end)

   (** Record the fact that an endpoint is no longer running (at a lower level
       this means that its relevant {e hublet} has been destroyed), and
       shutdown the simulated cable if appropriate. *)
   method decrement_alive_endpoint_no =
     Recursive_mutex.with_mutex mutex
       (fun () ->
         Log.printf "Decrement_alive_endpoint_no\n";
         self#check_alive_endpoint_no;
         alive_endpoint_no := !alive_endpoint_no - 1;
         self#check_alive_endpoint_no;
         if !alive_endpoint_no < 3 then begin
           (* Note that we destroy rather than terminating. This enables to re-create the
              simulated device later, at startup time, referring the correct hublets
              that will exist then, rather than the ones existing now *)
           Log.printf "The reference count dropped below three: destroying a cable\n";
           self#destroy_right_now;
         end)

   (** Make a new simulated device according to the current status *)
   method private make_simulated_device =
     Recursive_mutex.with_mutex mutex
       (fun () ->
         let left_hublet_process =
           left_endpoint#node#get_hublet_process_of_port left_endpoint#port_index
         in
         let right_hublet_process =
           right_endpoint#node#get_hublet_process_of_port right_endpoint#port_index
         in
         let left_blink_command =
           match left_endpoint#node#has_ledgrid with
           | false -> None
           | true  -> Some (Printf.sprintf "(id: %i; port: %i)" left_endpoint#node#id left_endpoint#port_index)
         in
         let right_blink_command =
           match right_endpoint#node#has_ledgrid with
           | false -> None
           | true  -> Some (Printf.sprintf "(id: %i; port: %i)" right_endpoint#node#id right_endpoint#port_index)
         in
         Log.printf "Left hublet process socket name is \"%s\"\n" left_hublet_process#get_socket_name;
         Log.printf "Right hublet process socket name is \"%s\"\n" right_hublet_process#get_socket_name;
         new Simulation_level.ethernet_cable
           ~parent:self
           ~left_end:left_hublet_process
           ~right_end:right_hublet_process
           ~blinker_thread_socket_file_name:(network#ledgrid_manager#blinker_thread_socket_file_name)
           ?left_blink_command
           ?right_blink_command
           ~unexpected_death_callback:self#destroy_because_of_unexpected_death
           ())

   (** This has to be overridden for cables, because we can't 'poweroff' as easily as the
       other devices: *)
   method private destroy_because_of_unexpected_death () =
     Recursive_mutex.with_mutex mutex
       (fun () ->
         (* Refresh the process in some (ugly) way: *)
         if self#is_connected then begin
           (try self#disconnect_right_now with _ -> ());
           (try self#connect_right_now with _ -> ());
           connected := true;
         end else begin
           let current_alive_endpoint_no = !alive_endpoint_no in
           self_as_simulated_device#destroy_because_of_unexpected_death ();
           connected := true;
           alive_endpoint_no := 0;
           for i = 1 to current_alive_endpoint_no do
             self#increment_alive_endpoint_no;
           done
         end)

   (** To do: remove this ugly kludge, and make cables stoppable *)
   method can_startup = true (* To do: try reverting this *)
   method can_gracefully_shutdown = true (* To do: try reverting this *)
   method can_poweroff = true (* To do: try reverting this *)
   (** Only connected cables can be 'suspended' *)
   method can_suspend =
     Recursive_mutex.with_mutex mutex
       (fun () -> !connected)
   (** Only non-connected cables with refcount exactly equal to 2 can be 'resumed' *)
   method can_resume =
     Recursive_mutex.with_mutex mutex
       (fun () -> not !connected)

   (** Get the reference count right at the beginning: it starts at zero, but
       it's immediately incremented if endpoint hublet processes already
       exist: *)
   initializer
     (if left_endpoint#node#has_hublet_processes  then self#increment_alive_endpoint_no);
     (if right_endpoint#node#has_hublet_processes then self#increment_alive_endpoint_no);
     Log.printf "The reference count for the just-created cable %s is %d\n" self#get_name !alive_endpoint_no;
end

end (* module User_level_cable *)

(*-----*)
  WHERE
(*-----*)

module Simulation_level = struct

(** Implementation of a user-level cable or of a link to a switch/hub to a hublet.
    It's meant to always link two switch/hub processes, and as an interesting particular
    case two hublets. The 'straight' and 'cross-over' cases are not distinguished. *)
class ['parent] ethernet_cable =
  fun ~(parent:'parent)
      ~(left_end)
      ~(right_end)
      ?blinker_thread_socket_file_name
      ?left_blink_command
      ?right_blink_command
      ~(unexpected_death_callback : unit -> unit)
      () ->
object(self)
  inherit ['parent] Simulation_level.device
      ~parent
      ~hublet_no:0
      ~unexpected_death_callback
      ()
      as super
  val ethernet_cable_process = ref None
  method private get_ethernet_cable_process =
    match !ethernet_cable_process with
      Some ethernet_cable_process -> ethernet_cable_process
    | None -> failwith "ethernet_cable: get_ethernet_cable_process was called when there is no such process"
  initializer
    let leftward_defects  = parent#get_left#get_my_defects in
    let rightward_defects = parent#get_right#get_my_defects in
    ethernet_cable_process :=
      Some(Simulation_level.make_ethernet_cable_process
             ~left_end
             ~right_end
             ~blinker_thread_socket_file_name
             ~left_blink_command
             ~right_blink_command
             ~leftward_defects
             ~rightward_defects
             ~unexpected_death_callback:self#execute_the_unexpected_death_callback
             ())
  method device_type = "Ethernet cable"

  method spawn_processes = self#get_ethernet_cable_process#spawn
  method terminate_processes =
    (try self#get_ethernet_cable_process#terminate with _ -> ())
  method stop_processes = self#get_ethernet_cable_process#stop
  method continue_processes = self#get_ethernet_cable_process#continue
end (* class ethernet_cable *)

end (* module Simulation_level *)

(** Just for testing: *)
(* let test = Dialog_add_or_update.make *)
