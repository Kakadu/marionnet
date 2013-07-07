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

open Gettext;;

(** User-level component "machine" implementation. *)

(* The module containing the add/update dialog is defined later,
   using the syntax extension "where" *)
#load "where_p4.cmo"
;;

(* Machine related constants: *)
(* TODO: make it configurable! *)
module Const = struct
 let port_no_default = 1
 let port_no_min = 1
 let port_no_max = 8

 let memory_default = 48
 let memory_min = 8
 (* let memory_max = 256 *)
 (* In order to test with selinux: *)
 let memory_max = 1024
end

(* The type of data returned by the dialog: *)
module Data = struct
type t = {
  name               : string;
  label              : string;
  memory             : int;
  port_no            : int;
  distribution       : string;          (* epithet *)
  variant            : string option;
  kernel             : string;          (* epithet *)
  console_no         : int;
  terminal           : string;
  old_name           : string;
  }

let to_string t = "<obj>" (* TODO? *)
end (* Data *)

module Make_menus (Params : sig
  val st      : State.globalState
  val packing : [ `toolbar of GButton.toolbar | `menu_parent of Menu_factory.menu_parent ]
 end) = struct

  open Params

  module Toolbar_entry = struct
   let imagefile = "ico.machine.palette.png"
   let tooltip   = (s_ "Machine")
   let packing   = Params.packing
  end

  module Add = struct
    include Data

    let key = Some GdkKeysyms._M

    let ok_callback t = Gui_bricks.Ok_callback.check_name t.name t.old_name st#network#name_exists t

    let dialog () =
      let name = st#network#suggestedName "m" in
      Dialog_add_or_update.make
        ~title:(s_ "Add machine") ~name ~ok_callback ()

    let reaction {
         name = name;
         label = label;
         memory = memory;
         port_no = port_no;
         distribution = distribution;
         variant = variant;
	 kernel = kernel;
	 console_no = console_no;
         terminal = terminal;
         old_name = _ ;
         }
      =
      let action () = ignore (
        new User_level_machine.machine (* defined later with WHERE *)
          ~network:st#network
          ~name
          ~label
	  ~memory
	  ~port_no
          ~epithet:distribution
          ?variant:variant
          ~kernel
          ~console_no
 	  ~terminal
          ())
      in
      st#network_change action ();

  end (* Add *)

  module Properties = struct
    include Data
    let dynlist () = st#network#get_nodes_that_can_startup ~devkind:`Machine ()

    let dialog name () =
     let m = (st#network#get_node_by_name name) in
     let m = ((Obj.magic m):> User_level_machine.machine) in
     let title = (s_ "Modify machine")^" "^name in
     let label = m#get_label in
     let memory = m#get_memory in
     let port_no = m#get_port_no in
     let distribution = m#get_epithet in
     let variant = m#get_variant in
     let kernel = m#get_kernel in
     let console_no = m#get_console_no in
     let terminal = m#get_terminal in
     (* The user cannot remove receptacles used by a cable. *)
     let port_no_min = st#network#port_no_lower_of (m :> User_level.node)
     in
     Dialog_add_or_update.make
       ~title ~name ~label
       ~memory ~port_no ~port_no_min
       ~distribution ?variant
       ~kernel
       ~console_no
       ~terminal
       ~updating:() (* the user cannot change the distrib & variant *)
       ~ok_callback:Add.ok_callback  ()


    let reaction {
         name = name;
         label = label;
         memory = memory;
         port_no = port_no;
         distribution = distribution;
         variant = variant;
	 kernel = kernel;
	 console_no = console_no;
         terminal = terminal;
         old_name = old_name;
         }
      =
      let d = (st#network#get_node_by_name old_name) in
      let m = ((Obj.magic d):> User_level_machine.machine) in
      let action () =
        m#update_machine_with
          ~name ~label
          ~memory ~port_no
	  ~kernel
	  ~console_no ~terminal
      in
      st#network_change action ();

  end (* Properties *)

  module Remove = struct
    type t = string (* just the name *)
    let to_string = (Printf.sprintf "name = %s\n")

    let dynlist = Properties.dynlist

    let dialog name () =
      Gui_bricks.Dialog.yes_or_cancel_question
        ~title:(s_ "Remove")
        ~markup:(Printf.sprintf (f_ "Are you sure that you want to remove %s\nand all the cables connected to this %s?") name (s_ "machine"))
        ~context:name
        ()

    let reaction name =
      let d = (st#network#get_node_by_name name) in
      let r = ((Obj.magic d):> User_level_machine.machine) in
      let action () = r#destroy in
      st#network_change action ();

  end

  module Startup = struct
    type t = string (* just the name *)
    let to_string = (Printf.sprintf "name = %s\n")
    let dynlist    = Properties.dynlist
    let dialog     = Menu_factory.no_dialog_but_simply_return_name
    let reaction name = (st#network#get_node_by_name name)#startup

  end

  module Stop = struct
    type t = string (* just the name *)
    let to_string = (Printf.sprintf "name = %s\n")
    let dynlist () = st#network#get_nodes_that_can_gracefully_shutdown ~devkind:`Machine ()
    let dialog = Menu_factory.no_dialog_but_simply_return_name
    let reaction name = (st#network#get_node_by_name name)#gracefully_shutdown

  end

  module Suspend = struct
    type t = string (* just the name *)
    let to_string = (Printf.sprintf "name = %s\n")
    let dynlist () = st#network#get_nodes_that_can_suspend ~devkind:`Machine ()
    let dialog = Menu_factory.no_dialog_but_simply_return_name
    let reaction name = (st#network#get_node_by_name name)#suspend

  end

  module Resume = struct
    type t = string (* just the name *)
    let to_string = (Printf.sprintf "name = %s\n")
    let dynlist () = st#network#get_nodes_that_can_resume ~devkind:`Machine ()
    let dialog = Menu_factory.no_dialog_but_simply_return_name
    let reaction name = (st#network#get_node_by_name name)#resume

  end

 module Create_entries =
  Gui_toolbar_COMPONENTS_layouts.Layout_for_network_node (Params) (Toolbar_entry) (Add) (Properties) (Remove) (Startup) (Stop) (Suspend) (Resume)

 (* Subscribe this kind of component to the network club: *)
 st#network#subscribe_a_try_to_add_procedure Eval_forest_child.try_to_add_machine;

end

(*-----*)
  WHERE
(*-----*)

module Dialog_add_or_update = struct

(* This function may be useful for testing the widget creation without
   recompiling the whole project. *)
let make
 ?(title="Add a machine")
 ?(name="")
 ?label
 ?(memory=Const.memory_default)
 ?(memory_min=Const.memory_min)
 ?(memory_max=Const.memory_max)
 ?(port_no=Const.port_no_default)
 ?(port_no_min=Const.port_no_min)
 ?(port_no_max=Const.port_no_max)
 ?distribution
 ?variant
 ?kernel
 ?(updating:unit option)
 ?(console_no=1)
 ?terminal
 ?(help_callback=help_callback) (* defined backward with "WHERE" *)
 ?(ok_callback=(fun data -> Some data))
 ?(dialog_image_file=Initialization.Path.images^"ico.machine.dialog.png")
 () :'result option =
  let old_name = name in
  let vm_installations =  Disk.get_machine_installations () in
  let (w,_,name,label) =
    Gui_bricks.Dialog_add_or_update.make_window_image_name_and_label
      ~title
      ~image_file:dialog_image_file
      ~image_tooltip:(s_ "Virtual machine")
      ~name
      ~name_tooltip:(s_ "Virtual machine name. This name must be unique in the virtual network.")
      ?label
      ()
  in
  let (memory, port_no, distribution_variant_kernel, console_no, terminal) =
    let vbox = GPack.vbox ~homogeneous:false ~border_width:20 ~spacing:10 ~packing:w#vbox#add () in
    let form =
      Gui_bricks.make_form_with_labels
        ~packing:vbox#add
        [(s_ "Memory <tt>(Mb)</tt>");
         (s_ "Ethernet cards");
         (s_ "Distribution");
         (s_ "Variant");
         (s_ "Kernel");
         (s_ "Consoles");
         (s_ "Terminal");
         ]
    in
    form#add_section ~no_line:() "Hardware";
    let memory =
      Gui_bricks.spin_byte ~lower:memory_min ~upper:memory_max ~step_incr:8
      ~packing:(form#add_with_tooltip (s_ "Amount of RAM to be reserved for this machine.")) memory
    in
    let port_no =
      Gui_bricks.spin_byte ~lower:port_no_min ~upper:port_no_max ~step_incr:1
      ~packing:(form#add_with_tooltip (s_ "Number of ethernet cards (eth0, eth1 ...) of the virtual machine")) port_no
    in
    (* Ugly hack: the callback will be correctly defined later: *)
    let on_distrib_change = ref [] (* a list of callbacks *)
    in
    form#add_section "Software";
    let (distribution_variant_kernel) =
      let packing_distribution =
        form#add_with_tooltip
          (s_ "GNU/Linux distribution installed on the virtual machine.")
      in
      let packing_variant      =
        form#add_with_tooltip
          (s_ "Initial hard disk state. The virtual machine will start by default with this variant of the chosen distribution.")
      in
      let packing_kernel =
        form#add_with_tooltip
          (s_ "Linux kernel version used for this virtual machine.")
      in
      let packing = (packing_distribution, packing_variant, packing_kernel) in
      Gui_bricks.make_combo_boxes_of_vm_installations
        ~on_distrib_change:(fun distrib -> List.iter (fun f -> f distrib) !on_distrib_change)
        ?distribution ?variant ?kernel ?updating
        ~packing
        vm_installations
    in
    form#add_section "Access";
    let console_no =
      Gui_bricks.spin_byte ~lower:1 ~upper:8 ~step_incr:1
      ~packing:(form#add_with_tooltip (s_ "Number of consoles (tty0, tty1 ...) of the virtual machine")) console_no
    in
    let console_no_related_action_on_distrib_change d =
      let sensitive = (vm_installations#multiple_consoles_supported_by d) in
      console_no#misc#set_sensitive (sensitive);
      (if not sensitive then console_no#set_value 1.);
    in
    let () =
      on_distrib_change := (console_no_related_action_on_distrib_change)::!on_distrib_change;
      let current = distribution_variant_kernel#selected in
      console_no_related_action_on_distrib_change (current)
    in
    let terminal =
      let tooltip = (s_ "Type of terminal to use to control the virtual machine. Possible choices are: X HOST terminal (providing the possibility to launch graphical applications on the host X server) and X NEST (an independent graphic server displaying all the X windows of a virtual machines).")
      in
      let result =
        Widget.ComboTextTree.fromList
          ~callback:None
          ~packing:(Some (form#add_with_tooltip tooltip))
          ((vm_installations#terminal_manager_of "unused epithet")#get_choice_list)
      in
      Option.iter (fun v -> result#set_active_value v) terminal;
      result
    in
    (memory, port_no, distribution_variant_kernel, console_no, terminal)
  in
  (* TODO: to be fully implemented or removed: *)
  terminal#box#misc#set_sensitive false;
  let get_widget_data () :'result =
    let name = name#text in
    let label = label#text in
    let memory = int_of_float memory#value in
    let port_no = int_of_float port_no#value in
    let distribution  = distribution_variant_kernel#selected in
    let variant       = distribution_variant_kernel#slave0#selected in
    let kernel        = distribution_variant_kernel#slave1#selected in
    let variant = match variant with
    | "none" -> None
    | x      -> Some x
    in
    let console_no = int_of_float console_no#value in
    let terminal = terminal#selected in
      { Data.name = name;
        Data.label = label;
	Data.memory = memory;
        Data.port_no = port_no;
        Data.distribution = distribution;
        Data.variant = variant;
        Data.kernel = kernel;
        Data.console_no = console_no;
        Data.terminal = terminal;
        Data.old_name = old_name;
        }

  in
  (* The result of make is the result of the dialog loop (of type 'result option): *)
  Gui_bricks.Dialog_run.ok_or_cancel w ~ok_callback ~help_callback ~get_widget_data ()


(*-----*)
  WHERE
(*-----*)

let help_callback =
   let title = (s_ "ADD OR MODIFY A VIRTUAL MACHINE") in
   let msg   = (s_ "\
In this dialog window you can define the name of the virtual \
machine and set several hardware and software parameters.\n\n\
SECTION 'Hardware'\n\
- Memory: amount of memory (RAM) that will be reserved on \
the host for this virtual machine (default 48 Mb)\n\n\
- Ethernet Card: number of Ethernet cards (defalut 1)\n\n\
SECTION 'Software':\n\n\
- Distribution: the GNU/Linux distribution (Debian, Mandriva, Gentoo,..), \
chosen among those available in the filesystem directory\n\n\
- Variant: a variant (or patch) of the given distribution; a variant is a \
COW (Copy On Write) file that represents a small update of the used distribution.\
Available variants are in the variants/ subdirectory of the filesystem directory. \
You can make your own variants by exporting any virtual machine state in the 'Disks' \
tab.\n\n\
- Kernel: the Linux kernel version, chosen among the ones available in the kernels/ \
subdirectory\n\n\
SECTION 'UML':\n\n\
- Terminal: the possible choices are 'X HOST' and 'X NEST'; the first one \
allows the user to run graphic applications from a text terminal where the user \
can operate the virtual machine (with user 'root' and password 'root'); \
the second allows the user to have a real graphic server reserved for the virtual \
machine, with independent windows manager and desktops environments.")
   in Simple_dialogs.help title msg ;;

end

(*-----*)
  WHERE
(*-----*)

module Eval_forest_child = struct

 let try_to_add_machine (network:User_level.network) ((root,children):Xforest.tree) =
  try
   (match root with
    | ("machine", attrs) ->
    	let name  = List.assoc "name" attrs in
	(* The key "eth" is also tried for backward-compatibility: *)
	let port_no = int_of_string (ListExtra.Assoc.find_first ["port_no"; "eth"] attrs) in
        Log.printf "Importing machine \"%s\" with %d ethernet cards...\n" name port_no;
	let x = new User_level_machine.machine ~network ~name ~port_no () in
	x#from_tree ("machine", attrs) children;
        Log.printf "Machine \"%s\" successfully imported.\n" name;
        true
   | _ -> false
   )
  with _ -> false
end (* module Eval_forest_child *)

(*-----*)
  WHERE
(*-----*)


module User_level_machine = struct

class machine
  ~(network:User_level.network)
  ~name
  ?label
  ?(memory=Const.memory_default)
  ?epithet
  ?variant
  ?kernel
  ?(console_no=1)
  ?terminal
  ~port_no
  ()
  =
  let vm_installations = Disk.get_machine_installations () in
  let network_alias = network in

  object (self) inherit OoExtra.destroy_methods ()

  inherit User_level.node_with_defects
    ~network
    ~name
    ?label
    ~devkind:`Machine
    ~port_no
    ~port_no_min:Const.port_no_min
    ~port_no_max:Const.port_no_max
    ~port_prefix:"eth"
    ~user_port_offset:0
    ()
    as self_as_node_with_defects

  inherit User_level.virtual_machine_with_history_and_ifconfig
    ~network:network_alias
    ?epithet ?variant ?kernel ?terminal
    ~history_icon:"machine"
    ~ifconfig_device_type:"machine"
    ~vm_installations
    ()
    as self_as_virtual_machine_with_history_and_ifconfig

  method polarity = User_level.MDI
  method string_of_devkind = "machine"

  (* Redefinition: *)
  method dot_fontsize_statement = ""

  (** Get the full host pathname to the directory containing the guest hostfs filesystem: *)
  method hostfs_directory_pathname =
    (Option.extract !simulated_device)#hostfs_directory_pathname

  (** A machine will be started with a certain amount of memory *)
  val mutable memory : int = memory
  initializer ignore (self#check_memory memory)
  method get_memory = memory
  method set_memory x = memory <- self#check_memory x
  method private check_memory x =
    match (x>=Const.memory_min) && (x<=Const.memory_max) with
    | true  -> x
    | false ->
        self#failwith "value %d not in the memory range [%d,%d]" x Const.memory_min Const.memory_max

  val mutable console_no : int = console_no
  initializer ignore (self#check_console_no console_no)
  method get_console_no = console_no
  method set_console_no x = console_no <- self#check_console_no x
  method private check_console_no x =
    match (x>=1) && (x<=8) with
    | true  -> x
    | false ->
        self#failwith "value %d not in the console no. range [%d,%d]" x 1 8

  (** Show for debugging *)
  method show = name

  method defects_device_type = "machine"

  method dotImg iconsize =
   let imgDir = Initialization.Path.images in
   (imgDir^"ico.machine."^(self#string_of_simulated_device_state)^"."^iconsize^".png")

  method to_tree =
   Forest.tree_of_leaf ("machine", [
      ("name"     ,  self#get_name );
      ("label"    ,  self#get_label );
      ("memory"   ,  (string_of_int self#get_memory));
      ("distrib"  ,  self#get_epithet  );
      ("variant"  ,  self#get_variant_as_string);
      ("kernel"   ,  self#get_kernel   );
      ("console_no", (string_of_int self#get_console_no));
      ("terminal" ,  self#get_terminal );
      ("port_no"  ,  (string_of_int self#get_port_no))  ;
      ])

 (** A machine has just attributes (no children) in this version. *)
 method eval_forest_attribute = function
  | ("name"     , x ) -> self#set_name x
  | ("label"    , x ) -> self#set_label x
  | ("memory"   , x ) -> self#set_memory (int_of_string x)
  | ("distrib"  , x ) -> self#set_epithet x
  | ("variant"  , "aucune" ) -> self#set_variant None (* backward-compatibility *)
  | ("variant"  , "" )-> self#set_variant None
  | ("variant"  , x ) -> self#set_variant (Some x)
  | ("kernel"   , x ) -> self#set_kernel x
  | ("console_no" , x ) -> self#set_console_no (int_of_string x)
  | ("terminal" , x ) -> self#set_terminal x
  | ("eth"      , x ) (* backward-compatibility *)
  | ("port_no"  , x ) -> self#set_port_no  (int_of_string x)
  | _ -> () (* Forward-comp. *)


 (** Create the simulated device *)
 method private make_simulated_device =
    let id = self#id in
    let cow_file_name, dynamically_get_the_cow_file_name_source =
      self#create_cow_file_name_and_thunk_to_get_the_source
    in
    let () =
     Log.printf
       "About to start the machine %s\n  with filesystem: %s\n  cow file: %s\n  kernel: %s\n  xnest: %b\n"
       self#name
       self#get_filesystem_file_name
       cow_file_name
       self#get_kernel_file_name
       self#is_xnest_enabled
    in
    new Simulation_level.machine
      ~parent:self
      ~kernel_file_name:self#get_kernel_file_name
      ?kernel_console_arguments:self#get_kernel_console_arguments
      ~filesystem_file_name:self#get_filesystem_file_name
      ~dynamically_get_the_cow_file_name_source
      ~cow_file_name
      ~states_directory:(self#get_states_directory)
      ~ethernet_interface_no:self#get_port_no
      ~memory:self#get_memory
      ~console_no:self#get_console_no
      ~umid:self#get_name
      ~id
      ~xnest:self#is_xnest_enabled
      ~unexpected_death_callback:self#destroy_because_of_unexpected_death
      ()

 (** Here we also have to manage cow files... *)
 method private gracefully_shutdown_right_now =
    Log.printf "Calling hostfs_directory_pathname on %s...\n" self#name;
    let hostfs_directory_pathname = self#hostfs_directory_pathname in
    Log.printf "Ok, we're still alive\n";
    (* Do as usual... *)
    self_as_node_with_defects#gracefully_shutdown_right_now;
    (* If we're in exam mode then make the report available in the texts treeview: *)
    (if Initialization.are_we_in_exam_mode then begin
      let treeview_documents = Treeview_documents.extract () in
      Log.printf "Adding the report on %s to the texts interface\n" self#name;
      treeview_documents#import_report
        ~machine_or_router_name:self#name
        ~pathname:(hostfs_directory_pathname ^ "/report.html")
        ();
      Log.printf "Added the report on %s to the texts interface\n" self#name;
      Log.printf "Adding the history on %s to the texts interface\n" self#name;
      treeview_documents#import_history
        ~machine_or_router_name:self#name
        ~pathname:(hostfs_directory_pathname ^ "/bash_history.text")
        ();
      Log.printf "Added the history on %s to the texts interface\n" self#name;
    end);
    (* ...And destroy, so that the next time we have to re-create the process command line
       can use a new cow file (see the make_simulated_device method) *)
    self#destroy_right_now

 (** Here we also have to manage cow files... *)
 method private poweroff_right_now =
    (* Do as usual... *)
    self_as_node_with_defects#poweroff_right_now;
    (* ...And destroy, so that the next time we have to re-create the process command line
       can use a new cow file (see the make_simulated_device method) *)
    self#destroy_right_now

 method update_machine_with ~name ~label ~memory ~port_no ~kernel ~console_no ~terminal =
   (* first action: *)
   self_as_virtual_machine_with_history_and_ifconfig#update_virtual_machine_with ~name ~port_no kernel;
   (* then we can set the object property "name" (read by #get_name): *)
   self_as_node_with_defects#update_with ~name ~label ~port_no;
   self#set_memory memory;
   self#set_console_no console_no;
   self#set_terminal terminal;

end;;

end (* module User_level_machine *)

(*-----*)
  WHERE
(*-----*)

module Simulation_level = struct
(** A machine: just a [machine_or_router] with [router = false] *)
class ['parent] machine =
  fun ~(parent:'parent)
      ~(filesystem_file_name)
      ~(kernel_file_name)
      ?(kernel_console_arguments)
      ~dynamically_get_the_cow_file_name_source
      ~(cow_file_name)
      ~states_directory
      ~(ethernet_interface_no)
      ?(memory=40) (* in megabytes *)
      ?umid
      ?(xnest=false)
      ?(console_no=1)
      ~id
      ~unexpected_death_callback
      () ->
object(self)
  inherit ['parent] Simulation_level.machine_or_router
      ~parent
      ~router:false
      ~filesystem_file_name
      ~dynamically_get_the_cow_file_name_source
      ~cow_file_name
      ~states_directory
      ~kernel_file_name
      ?kernel_console_arguments
      ~ethernet_interface_no
      ~memory
      ?umid
      ~console:"xterm"
      ~console_no
      ~id
      ~xnest
      ~unexpected_death_callback
      ()
      as super
  method device_type = "computer"
end;;

end (* module Simulation_level *)


(** Just for testing: *)
let test = Dialog_add_or_update.make
