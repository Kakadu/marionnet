(* This file is part of Marionnet
   Copyright (C) 2010 Jean-Vincent Loddo

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

open Gettext

(** {b Example}:
{[
let tooltips = Gui_Bricks.make_tooltips_for_container window in
tooltips label#coerce "hello";
tooltips entry#coerce "salut";
]}
*)
let make_tooltips_for_container w =
  let result = (GData.tooltips ()) in
  let _ = w#connect#destroy ~callback:(fun _ -> result#destroy ()) in
  fun (widget:GObj.widget) text -> result#set_tip widget ~text

(** Make a classic rectangular input form with field labels at the left side
    and input widgets at the right side of each line.
    Labels are get from the input string list while input widgets are
    added later using the method [add]. {b Example}:
{[...
let form =
  Gui_Bricks.make_form_with_labels
    ~packing:vbox#add
    ["IPv4 address"; "DHCP service"]
in
let ipv4address  = GEdit.entry ~text:"10.0.2.1" ~packing:form#add () in
let dhcp_enabled = GButton.check_button ~packing:form#add () in
...}]
*)
let make_form_with_labels ?(section_no=0) ?(row_spacings=10) ?(col_spacings=10) ?packing string_list =
 let rows = (List.length string_list) + (section_no * 2) in
 let table = GPack.table ~row_spacings ~col_spacings ~rows ~columns:2 ~homogeneous:false ?packing () in
 let labels =
   Array.mapi
     (fun i label_text ->
        let label = GMisc.label ~xalign:0. ~markup:label_text () in
        label)
     (Array.of_list string_list)
 in
 let tooltip = make_tooltips_for_container table in
 object (self)
   method table = table
   method coerce = table#coerce
   val mutable field_index = 0
   val mutable row_index = 0
   method private aligned_widget widget =
     let box = GBin.alignment ~xalign:0. ~yalign:0.5 ~xscale:0.0 ~yscale:0.0 () in
     box#add widget#coerce;
     box

   method add =
     let top = row_index in (* top is in the closure *)
     let field = field_index in
     table#attach ~left:0 ~top (Array.get labels field)#coerce;
     row_index <- row_index+1;
     field_index <- field_index+1;
     (function widget ->
       table#attach ~left:1 ~top (self#aligned_widget widget)#coerce;
       )

   method add_section ?(fg="lightgray") ?(size="large") ?no_line markup =
     let markup = 
       Printf.sprintf "<span foreground='%s' size='%s'><b>%s</b></span>" fg size markup;
     in
     let label = GMisc.label ~xalign:0. ~markup () in
     let top = row_index+1 in
     row_index <- row_index+2; (* additional line for vertical spacing *)
     table#attach ~left:0 ~top label#coerce;
     (match no_line with
     | None -> table#attach ~left:1 ~top (GMisc.separator `HORIZONTAL ())#coerce
     | _    -> ());

   method add_with_tooltip ?just_for_label text =
     let top = row_index in (* top is in the closure *)
     let field = field_index in
     table#attach ~left:0 ~top (Array.get labels field)#coerce;
     row_index <- row_index+1;
     field_index <- field_index+1;
     (function widget ->
       table#attach ~left:1 ~top (self#aligned_widget widget)#coerce;
       (if just_for_label = None then tooltip widget text);
       tooltip ((Array.get labels field)#coerce) text;
       )

 end

(** Wrap the given widget with a label, using an hidden table which will be packaged
    in its container (if provided). The result is the input widget itself.
    {b Example}:
{[
let entry_with_label ?packing ?max_length ?entry_text ?labelpos label_text =
  let entry = GEdit.entry ?text:entry_text ?max_length () in
  Gui_Bricks.wrap_with_label ?packing ?labelpos label_text entry
]}
*)
let wrap_with_label ?tooltip ?packing ?(labelpos=`NORTH) label_text widget =
 let label = GMisc.label ~text:label_text () in
 let (rows, columns) =
   match labelpos with
   | `NORTH | `SOUTH -> 2,1
   | `EAST  | `WEST  -> 1,2
 in
 let table = GPack.table ~rows ~columns ~homogeneous:true ?packing () in
 let () = match labelpos with
   | `NORTH -> table#attach ~left:0 ~top:0 label#coerce;  table#attach ~left:0 ~top:1 widget#coerce
   | `SOUTH -> table#attach ~left:0 ~top:0 widget#coerce; table#attach ~left:0 ~top:1 label#coerce
   | `WEST  -> table#attach ~left:0 ~top:0 label#coerce;  table#attach ~left:1 ~top:0 widget#coerce
   | `EAST  -> table#attach ~left:0 ~top:0 widget#coerce; table#attach ~left:1 ~top:0 label#coerce
 in
 Option.iter ((make_tooltips_for_container table) table#coerce) tooltip;
 widget

(** A simple [GEdit.entry] equipped by a label specified as a string. *)
let entry_with_label ?tooltip ?packing ?max_length ?entry_text ?labelpos label_text =
  let entry = GEdit.entry ?text:entry_text ?max_length () in
  wrap_with_label ?tooltip ?packing ?labelpos label_text entry

(** Not in the interface.*)
let add_tooltip_label_and_labelpos_parameters ?tooltip ?label ?labelpos ?packing maker =
  match label with
  | None   -> maker ?tooltip ?packing ()
  | Some label_text ->
      let result = maker ?tooltip:None ?packing:None () in
      let _ = wrap_with_label ?tooltip ?packing ?labelpos label_text result in
      result

(** A spin for bytes, i.e. for values in the range [0..255].  *)
let spin_byte ?tooltip ?label ?labelpos ?(lower=0) ?(upper=255) ?(step_incr=1) ?packing value =
  let lower = float_of_int lower in
  let upper = float_of_int upper in
  let step_incr = float_of_int step_incr in
  let maker ?tooltip ?packing () =
    let sb = GEdit.spin_button ?packing (*~width:50*) (* 60 *) ~digits:0 ~numeric:true () in
    sb#adjustment#set_bounds ~lower ~upper ~step_incr ();
    sb#set_value (float_of_int value);
    Option.iter ((make_tooltips_for_container sb) sb#coerce) tooltip;
    sb
  in
  add_tooltip_label_and_labelpos_parameters ?tooltip ?label ?labelpos ?packing maker
;;

let byte_tooltips_default_array =
  Array.of_list [
    (s_ "First byte of the IPv4 address" );
    (s_ "Second byte of the IPv4 address" );
    (s_ "Third byte of the IPv4 address" );
    (s_ "Fourth byte of the IPv4 address" );
    (s_ "Netmask (CIDR notation)" );
    ]

(** Four spins for asking for an ipv4 address. *)
let spin_ipv4_address ?tooltip ?byte_tooltips ?label ?labelpos ?packing v1 v2 v3 v4 =
  let byte_tooltips = match byte_tooltips with
  | None   -> byte_tooltips_default_array
  | Some a -> a
  in
  let (tooltip_s1, tooltip_s2, tooltip_s3, tooltip_s4) =
   let a = byte_tooltips in
   (a.(0), a.(1), a.(2), a.(3))
  in
  let maker ?packing () =
    let table = GPack.table ~rows:1 ~columns:7 ~homogeneous:false ?packing () in
    let dot ~left = GMisc.label ~packing:(table#attach ~left ~top:0) ~width:15 ~markup:"<b>.</b>" () in
    let s1 = spin_byte ~tooltip:tooltip_s1 ~packing:(table#attach ~left:0 ~top:0) v1 in
    let _1 = dot ~left:1 in
    let s2 = spin_byte ~tooltip:tooltip_s2 ~packing:(table#attach ~left:2 ~top:0) v2 in
    let _2 = dot ~left:3 in
    let s3 = spin_byte ~tooltip:tooltip_s3 ~packing:(table#attach ~left:4 ~top:0) v3 in
    let _3 = dot ~left:5 in
    let s4 = spin_byte ~tooltip:tooltip_s4 ~packing:(table#attach ~left:6 ~top:0) v4 in
    (table,(s1,s2,s3,s4))
  in
  match label with
  | None   -> snd (maker ?packing ())
  | Some label_text ->
      let (table,(s1,s2,s3,s4)) = maker ?packing:None () in
      let _ = wrap_with_label ?tooltip ?packing ?labelpos label_text table in
      (s1,s2,s3,s4)

(** Four spins for asking for an ipv4 address, and a fifth for
    the netmask (in CIDR notation).  *)
let spin_ipv4_address_with_cidr_netmask
  ?tooltip ?byte_tooltips ?label ?labelpos ?packing v1 v2 v3 v4 v5
  =
  let byte_tooltips = match byte_tooltips with
  | None   -> byte_tooltips_default_array
  | Some a -> a
  in
  let (tooltip_s1, tooltip_s2, tooltip_s3, tooltip_s4, tooltip_s5) =
   let a = byte_tooltips in
   (a.(0), a.(1), a.(2), a.(3), a.(4))
  in
  let maker ?packing () =
    let table = GPack.table ~rows:1 ~columns:9 ~homogeneous:false ?packing () in
    let dot ~left = GMisc.label ~packing:(table#attach ~left ~top:0) ~width:15 ~markup:"<b>.</b>" () in
    let s1 = spin_byte ~tooltip:tooltip_s1 ~packing:(table#attach ~left:0 ~top:0) v1 in
    let _1 = dot ~left:1 in
    let s2 = spin_byte ~tooltip:tooltip_s2 ~packing:(table#attach ~left:2 ~top:0) v2 in
    let _2 = dot ~left:3 in
    let s3 = spin_byte ~tooltip:tooltip_s3 ~packing:(table#attach ~left:4 ~top:0) v3 in
    let _3 = dot ~left:5 in
    let s4 = spin_byte ~tooltip:tooltip_s4 ~packing:(table#attach ~left:6 ~top:0) v4 in
    let _slash = GMisc.label ~packing:(table#attach ~left:7 ~top:0) ~width:15 ~markup:"<b>/</b>" () in
    let s5 = spin_byte ~tooltip:tooltip_s5 ~packing:(table#attach ~left:8 ~top:0) v5 in
    (table,(s1,s2,s3,s4,s5))
  in
  match label with
  | None   -> snd (maker ?packing ())
  | Some label_text ->
      let (table,(s1,s2,s3,s4,s5)) = maker ?packing:None () in
      let _ = wrap_with_label ?tooltip ?packing ?labelpos label_text table in
      (s1,s2,s3,s4,s5)


let add_help_button_if_necessary window = function
| None   -> (fun () -> ())
| Some f -> (window#add_button_stock `HELP `HELP; f)


module Ok_callback = struct

let check_name name old_name name_exists t =
  if not (StrExtra.wellFormedName name)
  then begin
    Simple_dialogs.error
      (s_ "Ill-formed name" )
      ("Admissible characters are letters and underscores." ) ();
    None   (* refused *)
  end else
  if (name <> old_name) && name_exists name
  then begin
    Simple_dialogs.error
      (s_ "Name conflict" )
      (Printf.sprintf(f_ "The name '%s' is already used in the virtual network. The names of virtual network elements must be unique." ) name)
      ();
    None   (* refused *)
  end else
  Some t (* accepted *)

end (* module Ok_callback *)


(** Wrappers for the method [run] of a dialog window. *)
module Dialog_run = struct

(** Wrapper for the method [run] of a dialog window.
    The function [get_widget_data] must extract the values from the dialog.
    The function [ok_callback] must check these values: if it consider that
    are incorrect, it returns [None] in order to continue the loop.
    Otherwise it builds the result [Some something] of the loop.
    If the [?help_callback] is not provided, the help button is not built.  *)
let ok_or_cancel
    (w:[ `CANCEL | `DELETE_EVENT | `HELP | `OK ] GWindow.dialog)
    ~(get_widget_data:unit -> 'a)
    ~(ok_callback:'a -> 'b option)
    ?help_callback () =
  begin
  let help_callback = add_help_button_if_necessary w help_callback in
  w#add_button_stock `CANCEL `CANCEL;
  w#add_button_stock `OK `OK;
  w#set_default_response `OK;
  w#set_response_sensitive `OK true;
  let result = ref None in
  let rec loop () =
    match w#run () with
    | `DELETE_EVENT | `CANCEL -> ()
    | `HELP -> (help_callback ()); loop ()
    | `OK ->
        (match ok_callback (get_widget_data ()) with
	| None   -> loop ()
	| Some d -> result := Some d
        )
  in
  (* The enter key has the same effect than pressing the OK button: *)
  let f_enter () = match ok_callback (get_widget_data ()) with
   | None   -> ()
   | Some d -> (result := Some d; ignore (w#event#send (GdkEvent.create `DELETE)))
  in
  let _ = w#event#connect#key_press ~callback:
    begin fun ev ->
      (if GdkEvent.Key.keyval ev = GdkKeysyms._Return then f_enter ());
      false
    end
  in
  loop ();
  w#destroy ();
  !result
  end


let set_key_meaning_to window key result value =
  let f_key () =
    (result := value; ignore (window#event#send (GdkEvent.create `DELETE)))
  in
  ignore (window#event#connect#key_press ~callback:
    begin fun ev ->
      (if GdkEvent.Key.keyval ev = key then f_key ());
      false
    end)


let yes_or_cancel
    (w:[ `CANCEL | `DELETE_EVENT | `HELP | `YES  ] GWindow.dialog)
    ?help_callback
    ~(context:'a)
    () : 'a option =
  begin
  let help_callback = add_help_button_if_necessary w help_callback in
  w#add_button_stock `CANCEL `CANCEL;
  w#add_button_stock `YES `YES;
  w#set_default_response `YES;
  w#set_response_sensitive `YES true;
  let result = ref None in
  let rec loop () =
    match w#run () with
    | `DELETE_EVENT | `CANCEL -> ()
    | `HELP -> (help_callback ()); loop ()
    | `YES -> result := Some context
  in
  (* The enter key has the same effect than pressing the YES button: *)
  set_key_meaning_to w GdkKeysyms._Return result (Some context);
  loop ();
  w#destroy ();
  !result
  end

(* Example: do you want to save the project before quitting? *)
let yes_no_or_cancel
    (w:[ `CANCEL | `DELETE_EVENT | `HELP | `NO | `YES  ] GWindow.dialog)
    ?help_callback
    ~(context:'a)
    () : ('a * bool) option =
  begin
  let help_callback = add_help_button_if_necessary w help_callback in
  w#add_button_stock `CANCEL `CANCEL;
  w#add_button_stock `NO `NO;
  w#add_button_stock `YES `YES;
  w#set_default_response `YES;
  w#set_response_sensitive `YES true;
  let result = ref None in
  let rec loop () =
    match w#run () with
    | `DELETE_EVENT | `CANCEL -> ()
    | `HELP -> (help_callback ()); loop ()
    | `YES -> result := Some (context,true)
    | `NO  -> result := Some (context,false)
  in
  (* The enter key has the same effect than pressing the YES button: *)
  set_key_meaning_to w GdkKeysyms._Return result (Some (context,true));
  loop ();
  w#destroy ();
  !result
  end

end (* module Dialog_run *)


let set_marionnet_icon window =
  let icon =
    let icon_file = Initialization.Path.images^"marionnet-launcher.png" in
    GdkPixbuf.from_file icon_file
  in
  (window#set_icon (Some icon))


module Dialog = struct

let make_a_window_for_a_question
 ?(title="Question")
 ?(image_filename=Initialization.Path.images^"ico.question-2.orig.png")
 ?markup
 ?text
 ()
 =
 let w = GWindow.dialog ~destroy_with_parent:true ~title ~modal:true ~resizable:false ~position:`CENTER () in
 set_marionnet_icon w;
 let hbox = GPack.hbox ~homogeneous:false ~border_width:20 ~spacing:10 ~packing:w#vbox#add () in
 let _image = GMisc.image ~file:image_filename ~xalign:0.5 ~packing:hbox#add () in
 let _label = GMisc.label ?markup ?text ~justify:`CENTER ~xalign:0.5 ~xpad:10 ~ypad:10 ~packing:hbox#add () in
 w


let yes_or_cancel_question ?title ?help_callback ?image_filename ?markup ?text
 ~(context:'a)
 () : 'a option
 =
  let w = make_a_window_for_a_question ?title ?image_filename ?markup ?text () in
  Dialog_run.yes_or_cancel w ?help_callback ~context ()


let yes_no_or_cancel_question ?title ?help_callback ?image_filename ?markup ?text
 ~(context:'a)
 () : ('a * bool) option
 =
  let w = make_a_window_for_a_question ?title ?image_filename ?markup ?text () in
  Dialog_run.yes_no_or_cancel w ?help_callback ~context ()

end (* module Dialog *)


type packing_function = GObj.widget -> unit

let make_combo_boxes_of_vm_installations
  ?distribution
  ?variant
  ?kernel
  ?updating
  ~packing
  vm_installations
  =
  (* Convert updating as boolean: *)
  let updating = (updating<>None) in
  (* Resolve the initial choice for distribution: *)
  let distribution = match distribution with
   | None   -> Option.extract vm_installations#filesystems#get_default_epithet
   | Some x -> x
   in
  (* Resolve the initial choice for variant: *)
  let variant = match variant with
   | None   -> "none"
   | Some x -> x
  in
  let (packing_distribution, packing_variant, packing_kernel) = packing in
  let distribution_widget =
     Widget.ComboTextTree.fromListWithSlave
      ~masterCallback:None
      ~masterPacking:(Some packing_distribution)
      (* The user can't change filesystem and variant any more once the device has been created:*)
      (match updating with
      | false -> (vm_installations#filesystems#get_epithet_list)
      | true  -> [distribution])
      ~slaveCallback: None
      ~slavePacking: (Some packing_variant)
      (fun epithet ->
        match updating with
        | false ->
            "none"::(vm_installations#variants_of epithet)#get_epithet_list
        | true -> [variant]
        )
  in
  let initial_variant_widget = distribution_widget#slave
  in
  (* Resolve the initial choice for kernel: *)
  let kernel = match kernel with
   | None -> Option.extract vm_installations#kernels#get_default_epithet
   | Some x -> x
  in
  let kernel_widget =
    Widget.ComboTextTree.fromList
      ~callback:None
      ~packing:(Some packing_kernel)
      (vm_installations#kernels#get_epithet_list)
  in
  (* Setting active values: *)
  distribution_widget#set_active_value distribution;
  initial_variant_widget#set_active_value variant;
  kernel_widget#set_active_value kernel;
  (* Blocking changes updating: *)
  if updating then begin
    distribution_widget#box#misc#set_sensitive false;
    initial_variant_widget#box#misc#set_sensitive false;
  end else ();

  (* The result: *)
  (distribution_widget, kernel_widget)


module Dialog_add_or_update = struct

let make_window_image_name_and_label
  ~title
  ~image_file
  ~image_tooltip
  ~name
  ~name_tooltip
  ?label
  ?label_tooltip
  ()
  =
  let w = GWindow.dialog ~destroy_with_parent:true ~title ~modal:true ~position:`CENTER () in
  set_marionnet_icon w;
  let tooltips = make_tooltips_for_container w in
  let hbox = GPack.hbox ~homogeneous:true ~border_width:20 ~spacing:10 ~packing:w#vbox#add () in
  let image = GMisc.image ~file:image_file ~xalign:0.5 ~packing:hbox#add () in
  tooltips image#coerce image_tooltip;
  let vbox = GPack.vbox ~spacing:10 ~packing:hbox#add () in
  let name  = entry_with_label ~tooltip:name_tooltip ~packing:vbox#add ~entry_text:name  (s_ "Name") in
  let label =
    let tooltip = match label_tooltip with
    | None -> (s_ "Label to be written in the network sketch, next to the element icon." )
    | Some x -> x
    in
    entry_with_label ~tooltip ~packing:vbox#add ?entry_text:label (s_ "Label")
  in
  ignore (GMisc.separator `HORIZONTAL ~packing:w#vbox#add ());
  (w,image,name,label)

end (* module Dialog_add_or_update *)

#load "chip_parser_p4.cmo"
;;
module Reactive_widget = struct

  class combo_box_text
    ?name ?parent
    ?system
    ~(strings:string list) ?active
    ?width
    ?height
    ~packing
    () =
  let system = Option.extract system ~fallback:Chip.get_or_initialize_current_system in
  let name = match name with None -> Chip.fresh_wire_name "combo_box_text" | Some x -> x in
  let make_widget ?(active=0) strings =
    let w = GEdit.combo_box_text ~strings ~use_markup:true ~active ~packing ?width ?height () in
    ignore ((fst w)#connect#changed (fun _ -> ignore system#stabilize));
    w
  in
  object (self)
    inherit [string list * int option, string option] Chip.wire ~name ?parent system
    val mutable content = make_widget ?active strings
    val mutable strings_content = strings
    method get_alone   = GEdit.text_combo_get_active content
    method set_alone (strings, active) =
      let active = match active with
      | Some i -> Some i
      | None ->
          (* The old selected string, if exists, is recovered: *)
          let active_string = GEdit.text_combo_get_active content in
          Option.bind active_string (fun x -> ListExtra.indexOf x strings)
      in
      (match strings = strings_content with
      | true -> Option.iter ((fst content)#set_active) active
      | false -> 
         (fst content)#destroy ();
         content <- make_widget ?active strings;
	 strings_content <- strings;
      )
    method destroy = (fst content)#destroy
  end

  let partition ?(loopback=true) xys n0 p0 n1 p1 =
   let nodes_of xys = ListExtra.uniq (List.map fst xys) in
   let substract (n0,p0) xys = match n0,p0 with
   | (Some n0, Some p0) -> List.filter ((<>)(n0,p0)) xys
   | (Some n0, None)    -> List.filter (fun (n,p)->n<>n0) xys
   | _                  -> xys
   in
   let (>>=) = Option.bind in
   let index_of n xs =
     n >>= (fun n -> (Option.map fst (ListExtra.searchi ((=)n) xs)))
   in
   let ports_of n xys = match n with
   | None -> []
   | Some x -> ListExtra.filter_map (fun (n,p)-> if n=x then Some p else None) xys
   in
   let xs0 = nodes_of xys in
   let xs0_active = index_of n0 xs0 in
   let ys0 = ports_of n0 xys in
   let ys0_active = index_of p0 ys0 in
   let (xs1, ys1) =
     let xys' = match loopback with
     | true  -> substract (n0,p0)   xys
     | false -> substract (n0,None) xys
     in
     (nodes_of xys', ports_of n1 xys')
   in
   let xs1_active = index_of n1 xs1 in
   let ys1_active = index_of p1 ys1
   in
   let w1 = (xs0, xs0_active) in
   let w2 = (ys0, ys0_active) in
   let w3 = (xs1, xs1_active) in
   let w4 = (ys1, ys1_active) in
   (w1,w2,w3,w4)
   ;;

 let guess_humanly_speaking_enpoints xys n0 p0 n1 p1 =
  let ((xs0,_),(ys0,_),(xs1,_),(ys1,_)) = partition ~loopback:false xys n0 p0 n1 p1 in
  let x0 = Option.catch List.hd xs0 in
  let y0 = Option.catch List.hd ys0 in
  let x1 = Option.catch List.hd xs1 in
  let y1 = Option.catch List.hd ys1 in
  ((x0,y0),(x1,y1))
 
 chip partition_chip (xys:(string * string) list) : (x0,y0,x1,y1) -> (w1,w2,w3,w4)
    = (partition xys x0 y0 x1 y1) ;;

 class cable_input_widget
   ?n0 ?p0 ?n1 ?p1
   ?width
   ?height
   ~packing_n0 ~packing_p0 ~packing_n1 ~packing_p1
   ~free_node_port_list
   ()
   =
   let (w1,w2,w3,w4) = partition free_node_port_list n0 p0 n1 p1 in
   let (xs0, xs0_active) = w1 in
   let (ys0, ys0_active) = w2 in
   let (xs1, xs1_active) = w3 in
   let (ys1, ys1_active) = w4 in
   let system = new Chip.system () in
   let n0_widget =
     let packing = packing_n0 in
     new combo_box_text ~name:"n0" ~system ~strings:xs0 ?active:xs0_active ?width ?height ~packing ()
   in
   let p0_widget =
     let packing = packing_p0 in
     new combo_box_text ~name:"p0" ~system ~strings:ys0 ?active:ys0_active ?width ?height ~packing ()
   in
   let n1_widget =
     let packing = packing_n1 in
     new combo_box_text ~name:"n1" ~system ~strings:xs1 ?active:xs1_active ?width ?height ~packing ()
   in
   let p1_widget =
     let packing = packing_p1 in
     new combo_box_text ~name:"p1" ~system ~strings:ys1 ?active:ys1_active ?width ?height ~packing ()
   in
   let _partition_chip =
     new partition_chip free_node_port_list
       ~system
       ~x0:n0_widget
       ~y0:p0_widget
       ~x1:n1_widget
       ~y1:p1_widget
       ~w1:n0_widget
       ~w2:p0_widget
       ~w3:n1_widget
       ~w4:p1_widget
       ()
   in
   object (self)
     method get_widget_data =
       ((n0_widget#get, p0_widget#get), (n1_widget#get, p1_widget#get))
       
     method system = system
     method destroy = 
       List.iter
         (fun w->w#destroy ())
         [n0_widget; p0_widget; n1_widget; p1_widget]

    initializer
       ignore system#stabilize;

   end
   
end (* Reactive_widget *)

let test () = Dialog.yes_or_cancel_question ~markup:"prova <b>bold</b>" ~context:'a' ()
