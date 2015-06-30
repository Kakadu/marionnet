
type devkind = [ `Machine | `Hub | `Switch | `Router | `World_gateway | `World_bridge | `Cloud ] ;;
type nodename = string
type receptname = string
type name = string
type label = string
type iconsize = string

type simulated_device_automaton_state = NoDevice | DeviceOff | DeviceOn | DeviceSleeping

exception ForbiddenTransition
val raise_forbidden_transition : string -> 'a

module Recursive_mutex : MutexExtra.Extended_signature 
  with type t = MutexExtra.Recursive.t

class virtual ['a] simulated_device :
  unit ->
  object
    (* --- *)
    constraint 'a = < get_name : string; .. >
    method virtual get_name : string
    method virtual make_simulated_device : 'a Simulation_level.device
    method private virtual add_destroy_callback : unit lazy_t -> unit
    (* --- *)
    val mutex : Recursive_mutex.t
    method private destroy_because_of_unexpected_death : unit -> unit
    method automaton_state_as_string : string
    (* --- *)
    method can_gracefully_shutdown : bool
    method can_poweroff : bool
    method can_resume : bool
    method can_startup : bool
    method can_suspend : bool
    (* --- *)
    method create : unit
    method create_right_now : unit
    method destroy_my_simulated_device : unit
    method destroy_right_now : unit
    method get_hublet_process_of_port : int -> Simulation_level.hublet_process
    method gracefully_restart : unit
    method gracefully_shutdown : unit
    method gracefully_shutdown_right_now : unit
    method has_hublet_processes : bool
    method is_correct : bool
    method next_simulated_device_state : simulated_device_automaton_state option
    method poweroff : unit
    method poweroff_right_now : unit
    method resume : unit
    method resume_right_now : unit
    method set_next_simulated_device_state : simulated_device_automaton_state option -> unit
    method simulated_device_state : simulated_device_automaton_state
    method startup : unit
    method startup_right_now : unit
    method string_of_simulated_device_state : string
    method suspend : unit
    method suspend_right_now : unit
  end

class id_name_label :
  ?name:string ->
  ?label:string ->
  unit ->
  object
    method id        : int
    method name      : string
    method get_label : string
    method set_label : string -> unit
    method get_name  : string
    method set_name  : string -> unit
  end

class virtual component :
  network:(< .. > as 'a) ->
  ?name:string ->
  ?label:string ->
  unit ->
  object
    val network : 'a
    (* --- *)
    inherit id_name_label
    (* --- *)
    method virtual suspend : unit
    method virtual resume : unit
    method virtual can_resume : bool
    method virtual can_suspend : bool
    method virtual to_tree : Xforest.tree
    (* --- *)
    method eval_forest_attribute : Xforest.attribute -> unit
    method eval_forest_child     : Xforest.tree -> unit
    method from_tree             : Xforest.node -> Xforest.forest -> unit
    method to_forest             : Xforest.forest
  end

class port :
  port_prefix:string ->
  internal_index:int ->
  user_port_offset:int ->
  unit ->
  object
    method internal_index : int
    method user_index : int
    method user_name : string
  end

type defects = < duplication: float;  flip: float;  loss: float;  max_delay: float;  min_delay: float >
  
class ['a] ports_card :
  network:< defects : Treeview_defects.t; .. > ->
  parent:'a ->
  port_no:int ->
  port_prefix:string ->
  ?user_port_offset:int ->
  unit ->
  object
    constraint 'a = < get_name : string; .. >
    method get_my_inward_defects_by_index  : int -> defects
    method get_my_outward_defects_by_index : int -> defects
    method internal_index_of_user_port_name : string -> int
    method port_no : int
    method port_prefix : string
    method user_port_index_of_internal_index : int -> int
    method user_port_index_of_user_port_name : string -> int
    method user_port_name_list : string list
    method user_port_name_of_internal_index : int -> string
    method user_port_offset : int
  end

type polarity = MDI | MDI_X | MDI_Auto

class virtual node_with_ports_card :
  network:(< (*defects : Treeview_defects.t;*)
	     defects : < get_port_attribute_of :  device_name:string ->
						  port_prefix:string ->
						  port_index:int ->
						  user_port_offset:int ->
						  port_direction:Treeview_defects.port_direction ->
						  column_header:string ->
						  unit -> float; .. >;
             get_cables_involved_by_node_name : string ->
                                                (< decrement_alive_endpoint_no : 'd;
                                                   increment_alive_endpoint_no : 'e;
                                                   show : string -> string;
                                                   .. > as 'c) list;
             .. >
           as 'b) ->
  name:string ->
  ?label:string ->
  devkind:devkind ->
  port_no:int ->
  port_prefix:string ->
  port_no_min:int ->
  port_no_max:int ->
  ?user_port_offset:int ->
  ?has_ledgrid:bool ->
  unit ->
  object ('a)
    val automaton_state : simulated_device_automaton_state ref
    val id : int
    val mutable label : string
    val mutex : Recursive_mutex.t
    val mutable name : string
    val network : 'b
    val next_automaton_state : simulated_device_automaton_state option ref
    val mutable ports_card : 'a ports_card option
    val simulated_device : node_with_ports_card Simulation_level.device option ref
    method private virtual add_destroy_callback : unit lazy_t -> unit
    method automaton_state_as_string : string
    method can_gracefully_shutdown : bool
    method can_poweroff : bool
    method can_resume : bool
    method can_startup : bool
    method can_suspend : bool
    method create : unit
    method create_right_now : unit
    method virtual destroy : unit
    method private destroy_because_of_unexpected_death : unit -> unit
    method destroy_my_simulated_device : unit
    method destroy_right_now : unit
    method devkind : devkind
    method virtual dotImg : iconsize -> string
    method dotLabelForEdges : string -> string
    method dotPortForEdges : string -> string
    method dotTrad : ?nodeoptions:string -> iconsize -> string
    method dot_fontsize_statement : string
    method private enqueue_task_with_progress_bar : string -> (unit -> unit) -> unit
    method eval_forest_attribute : Xforest.attribute -> unit
    method eval_forest_child : Xforest.tree -> unit
    method from_tree : Xforest.node -> Xforest.forest -> unit
    method get_hublet_process_of_port : int -> Simulation_level.hublet_process
    method get_label : string
    method get_name : string
    method get_port_no : int
    method gracefully_restart : unit
    method gracefully_shutdown : unit
    method gracefully_shutdown_right_now : unit
    method has_hublet_processes : bool
    method has_ledgrid : bool
    method id : int
    method is_correct : bool
    method label_for_dot : string
    method leds_relative_subdir : string
    method virtual make_simulated_device : node_with_ports_card Simulation_level.device
    method name : string
    method next_simulated_device_state : simulated_device_automaton_state option
    method virtual polarity : polarity
    method port_no_max : int
    method port_no_min : int
    method port_prefix : string
    method ports_card : 'a ports_card
    method poweroff : unit
    method poweroff_right_now : unit
    method resume : unit
    method resume_right_now : unit
    method set_label : string -> unit
    method set_name : string -> unit
    method set_next_simulated_device_state : simulated_device_automaton_state option -> unit
    method set_port_no : int -> unit
    method simulated_device_state : simulated_device_automaton_state
    method startup : unit
    method startup_right_now : unit
    method virtual string_of_devkind : string
    method string_of_simulated_device_state : string
    method suspend : unit
    method suspend_right_now : unit
    method to_forest : Xforest.forest
    method virtual to_tree : Xforest.tree
    method user_port_offset : int
  end
  
class type virtual node = node_with_ports_card

class virtual node_with_defects_zone :
  network:< defects : Treeview_defects.t; .. > ->
  unit ->
  object
    method virtual add_destroy_callback : unit Lazy.t -> unit
    method private add_my_defects : unit
    method virtual defects_device_type : string
    method private defects_update_port_no : int -> unit
    method private destroy_my_defects : unit
    method virtual get_name : string
    method virtual get_port_no : int
    method virtual port_prefix : string
    method virtual user_port_offset : int
  end

class virtual node_with_defects :
  network:(< add_node : node -> 'c; defects : Treeview_defects.t;
             del_node_by_name : string -> unit;
             get_cables_involved_by_node_name : string ->
                                              < decrement_alive_endpoint_no : 'd;
                                                increment_alive_endpoint_no : 'e;
                                                show : string -> string; .. >
                                              list;
             .. >
           as 'b) ->
  name:string ->
  ?label:string ->
  devkind:devkind ->
  port_no:int ->
  port_no_min:int ->
  port_no_max:int ->
  ?user_port_offset:int ->
  port_prefix:string ->
  unit ->
  object ('a)
    val automaton_state : simulated_device_automaton_state ref
    val id : int
    val mutable label : string
    val mutex : Recursive_mutex.t
    val mutable name : string
    val network : 'b
    val next_automaton_state : simulated_device_automaton_state option ref
    val mutable ports_card : 'a ports_card option
    val simulated_device : node_with_ports_card Simulation_level.device option ref
    method virtual add_destroy_callback : unit Lazy.t -> unit
    method private add_my_defects : unit
    method automaton_state_as_string : string
    method can_gracefully_shutdown : bool
    method can_poweroff : bool
    method can_resume : bool
    method can_startup : bool
    method can_suspend : bool
    method create : unit
    method create_right_now : unit
    method virtual defects_device_type : string
    method private defects_update_port_no : int -> unit
    method virtual destroy : unit
    method private destroy_because_of_unexpected_death : unit -> unit
    method private destroy_my_defects : unit
    method destroy_my_simulated_device : unit
    method destroy_right_now : unit
    method devkind : devkind
    method virtual dotImg : iconsize -> string
    method dotLabelForEdges : string -> string
    method dotPortForEdges : string -> string
    method dotTrad : ?nodeoptions:string -> iconsize -> string
    method dot_fontsize_statement : string
    method private enqueue_task_with_progress_bar : string -> (unit -> unit) -> unit
    method eval_forest_attribute : Xforest.attribute -> unit
    method eval_forest_child : Xforest.tree -> unit
    method from_tree : Xforest.node -> Xforest.forest -> unit
    method get_hublet_process_of_port : int -> Simulation_level.hublet_process
    method get_label : string
    method get_name : string
    method get_port_no : int
    method gracefully_restart : unit
    method gracefully_shutdown : unit
    method gracefully_shutdown_right_now : unit
    method has_hublet_processes : bool
    method has_ledgrid : bool
    method id : int
    method is_correct : bool
    method label_for_dot : string
    method leds_relative_subdir : string
    method virtual make_simulated_device : node_with_ports_card Simulation_level.device
    method name : string
    method next_simulated_device_state : simulated_device_automaton_state option
    method virtual polarity : polarity
    method port_no_max : int
    method port_no_min : int
    method port_prefix : string
    method ports_card : 'a ports_card
    method poweroff : unit
    method poweroff_right_now : unit
    method resume : unit
    method resume_right_now : unit
    method set_label : string -> unit
    method set_name : string -> unit
    method set_next_simulated_device_state : simulated_device_automaton_state option -> unit
    method set_port_no : int -> unit
    method simulated_device_state : simulated_device_automaton_state
    method startup : unit
    method startup_right_now : unit
    method virtual string_of_devkind : string
    method string_of_simulated_device_state : string
    method suspend : unit
    method suspend_right_now : unit
    method to_forest : Xforest.forest
    method virtual to_tree : Xforest.tree
    method update_with :
      name:string ->
      label:string -> port_no:int -> unit
    method user_port_offset : int
  end

class virtual node_with_ledgrid_and_defects :
  network:(< add_node : node -> 'c;
             busy_port_indexes_of_node : node_with_ports_card -> int list;
             defects : Treeview_defects.t;
             del_node_by_name : string -> unit;
             get_cables_involved_by_node_name : string ->
                                              < decrement_alive_endpoint_no : 'd;
                                                increment_alive_endpoint_no : 'e;
                                                show : string -> string; .. >
                                              list;
             ledgrid_manager : Ledgrid_manager.ledgrid_manager; .. >
           as 'b) ->
  name:string ->
  ?label:string ->
  devkind:devkind ->
  port_no:int ->
  port_no_min:int ->
  port_no_max:int ->
  ?user_port_offset:int ->
  port_prefix:string ->
  unit ->
  object ('a)
    val automaton_state : simulated_device_automaton_state ref
    val id : int
    val mutable label : string
    val mutex : Recursive_mutex.t
    val mutable name : string
    val network : 'b
    val next_automaton_state : simulated_device_automaton_state option ref
    val mutable ports_card : 'a ports_card option
    val simulated_device : node_with_ports_card Simulation_level.device option ref
    method virtual add_destroy_callback : unit Lazy.t -> unit
    method private add_my_defects : unit
    method add_my_ledgrid : unit
    method automaton_state_as_string : string
    method can_gracefully_shutdown : bool
    method can_poweroff : bool
    method can_resume : bool
    method can_startup : bool
    method can_suspend : bool
    method create : unit
    method create_right_now : unit
    method virtual defects_device_type : string
    method private defects_update_port_no : int -> unit
    method virtual destroy : unit
    method private destroy_because_of_unexpected_death : unit -> unit
    method private destroy_my_defects : unit
    method destroy_my_ledgrid : unit
    method destroy_my_simulated_device : unit
    method destroy_right_now : unit
    method devkind : devkind
    method virtual dotImg : iconsize -> string
    method dotLabelForEdges : string -> string
    method dotPortForEdges : string -> string
    method dotTrad : ?nodeoptions:string -> iconsize -> string
    method dot_fontsize_statement : string
    method private enqueue_task_with_progress_bar : string -> (unit -> unit) -> unit
    method eval_forest_attribute : Xforest.attribute -> unit
    method eval_forest_child : Xforest.tree -> unit
    method from_tree : Xforest.node -> Xforest.forest -> unit
    method get_hublet_process_of_port : int -> Simulation_level.hublet_process
    method get_label : string
    method get_name : string
    method get_port_no : int
    method gracefully_restart : unit
    method gracefully_shutdown : unit
    method gracefully_shutdown_right_now : unit
    method has_hublet_processes : bool
    method has_ledgrid : bool
    method id : int
    method is_correct : bool
    method label_for_dot : string
    method ledgrid_image_directory : string
    method virtual ledgrid_label : string
    method ledgrid_title : string
    method leds_relative_subdir : string
    method virtual make_simulated_device : node_with_ports_card Simulation_level.device
    method name : string
    method next_simulated_device_state : simulated_device_automaton_state option
    method virtual polarity : polarity
    method port_no_max : int
    method port_no_min : int
    method port_prefix : string
    method ports_card : 'a ports_card
    method poweroff : unit
    method poweroff_right_now : unit
    method resume : unit
    method resume_right_now : unit
    method set_label : string -> unit
    method set_name : string -> unit
    method set_next_simulated_device_state : simulated_device_automaton_state option -> unit
    method set_port_no : int -> unit
    method simulated_device_state : simulated_device_automaton_state
    method startup : unit
    method startup_right_now : unit
    method virtual string_of_devkind : string
    method string_of_simulated_device_state : string
    method suspend : unit
    method suspend_right_now : unit
    method to_forest : Xforest.forest
    method virtual to_tree : Xforest.tree
    method update_with :
      name:string ->
      label:string -> port_no:int -> unit
    method user_port_offset : int
  end

class virtual virtual_machine_with_history_and_ifconfig :
  network:< history : Treeview_history.t; ifconfig : Treeview_ifconfig.t;
            .. > ->
  ?epithet:[ `distrib ] Disk.epithet ->
  ?variant:string ->
  ?kernel:[ `kernel ] Disk.epithet ->
  ?terminal:string ->
  history_icon:string ->
  ifconfig_device_type:string ->
  ?ifconfig_port_row_completions:Treeview_ifconfig.port_row_completions ->
  vm_installations:Disk.virtual_machine_installations ->
  unit ->
  object
    val mutable epithet : [ `distrib ] Disk.epithet
    val mutable kernel : [ `kernel ] Disk.epithet
    val mutable terminal : string
    val mutable variant : [ `variant ] Disk.epithet option
    method private virtual add_destroy_callback : unit lazy_t -> unit
    method add_my_history : unit
    method add_my_ifconfig : ?port_row_completions:Treeview_ifconfig.port_row_completions -> int -> unit
    method private banner : string
    method private check_epithet  : [ `distrib ] Disk.epithet -> [ `distrib ] Disk.epithet
    method private check_kernel   : [ `kernel ] Disk.epithet -> [ `kernel ] Disk.epithet
    method private check_terminal : string -> string
    method private check_variant  : [ `variant ] Disk.epithet -> [ `variant ] Disk.epithet
    method create_cow_file_name_and_thunk_to_get_the_source : string * (unit -> Disk.realpath option)
    method destroy_my_history : unit
    method destroy_my_ifconfig : unit
    method failwith : ('a, unit, string, string) format4 -> 'b
    method get_epithet : [ `distrib ] Disk.epithet
    method get_filesystem_file_name : Disk.realpath
    method get_filesystem_relay_script : Disk.filename option
    method get_kernel : [ `kernel ] Disk.epithet
    method get_kernel_console_arguments : string option
    method get_kernel_file_name : Disk.realpath
    method private virtual get_name : string
    method private virtual get_port_no : int
    method get_states_directory : string
    method get_terminal : string
    method get_variant : [ `variant ] Disk.epithet option
    method get_variant_as_string : [ `variant ] Disk.epithet
    method get_variant_realpath : Disk.realpath option
    method history_icon : Treeview.Row_item.Icon_prj_inj.a
    method ifconfig_device_type : string
    method is_xnest_enabled : bool
    method private prefixed_epithet : string
    method set_epithet : [ `distrib ] Disk.epithet -> unit
    method set_kernel : [ `kernel ] Disk.epithet -> unit
    method set_terminal : string -> unit
    method set_variant : string option -> unit
    method sprintf : ('a, unit, string, string) format4 -> 'a
    method update_virtual_machine_with :
      name:string ->
      port_no:int -> [ `kernel ] Disk.epithet -> unit
  end

class type endpoint =
  object
    method involved_node_and_port_index : node * int
    method node : node
    method port_index : int
    method user_port_index : int
    method user_port_name : string
  end

class type virtual cable =
  object
    val automaton_state : simulated_device_automaton_state ref
    val id : int
    val mutable label : string
    val mutex : Recursive_mutex.t
    val mutable name : string
    val network : < .. >
    val next_automaton_state : simulated_device_automaton_state option ref
    val simulated_device : component Simulation_level.device option ref
    method private virtual add_destroy_callback : unit lazy_t -> unit
    method automaton_state_as_string : string
    method can_gracefully_shutdown : bool
    method can_poweroff : bool
    method can_resume : bool
    method can_startup : bool
    method can_suspend : bool
    method create : unit
    method create_right_now : unit
    method crossover : bool
    method decrement_alive_endpoint_no : unit
    method destroy : unit
    method private destroy_because_of_unexpected_death : unit -> unit
    method destroy_my_simulated_device : unit
    method destroy_right_now : unit
    method dot_traduction : curved_lines:bool -> labeldistance:float -> string
    method private enqueue_task_with_progress_bar : string -> (unit -> unit) -> unit
    method eval_forest_attribute : Xforest.attribute -> unit
    method eval_forest_child : Xforest.tree -> unit
    method from_tree : Xforest.node -> Xforest.forest -> unit
    method get_hublet_process_of_port : int -> Simulation_level.hublet_process
    method get_label : string
    method get_left : endpoint
    method get_name : string
    method get_right : endpoint
    method gracefully_restart : unit
    method gracefully_shutdown : unit
    method gracefully_shutdown_right_now : unit
    method has_hublet_processes : bool
    method id : int
    method increment_alive_endpoint_no : unit
    method involved_node_and_port_index_list : (node * int) list
    method is_connected : bool
    method is_correct : bool
    method is_node_involved : string -> bool
    method is_reversed : bool
    method virtual make_simulated_device : component Simulation_level.device
    method name : string
    method next_simulated_device_state : simulated_device_automaton_state option
    method poweroff : unit
    method poweroff_right_now : unit
    method resume : unit
    method resume_right_now : unit
    method set_label : string -> unit
    method set_name : string -> unit
    method set_next_simulated_device_state : simulated_device_automaton_state option -> unit
    method set_reversed : bool -> unit
    method show : string -> string
    method simulated_device_state : simulated_device_automaton_state
    method startup : unit
    method startup_right_now : unit
    method string_of_simulated_device_state : string
    method suspend : unit
    method suspend_right_now : unit
    method to_forest : Xforest.forest
    method virtual to_tree : Xforest.tree
  end

class network :
  unit ->
  object ('a)
    method ifconfig        : Treeview_ifconfig.t
    method defects         : Treeview_defects.t
    method history         : Treeview_history.t
    method motherboard     : Motherboard.t
    method ledgrid_manager : Ledgrid_manager.ledgrid_manager
    method dotoptions      : Sketch.tuning
    (* --- *)
    method nodes  : node  Queue.t Cortex.t
    method cables : cable Queue.t Cortex.t
    (* --- *)
    method add_node  : node  -> unit
    method add_cable : cable -> unit
    (* --- *)
    method names : string list
    method name_exists : string -> bool
    method components : component list
    method components_of_kind : ?kind:[ `Cable | `Node ] -> unit -> component list
    method get_component_by_name : ?kind:[ `Cable | `Node ] -> string -> component
    method get_component_names_that_can_suspend_or_resume : unit -> (string * [ `Cable | `Node ] * bool) list
    method disjoint_union_of_nodes_and_cables : (component * [ `Cable | `Node ]) list
    (* --- *)
    method node_exists : string -> bool
    method get_node_by_name : string -> node
    method get_node_list : node list
    method set_node_list : node list -> unit
    method is_node_list_empty : bool
    method del_node_by_name : string -> unit
    method get_node_names : string list
    method get_node_names_that_can_gracefully_shutdown : ?devkind:devkind -> unit -> string list
    method get_node_names_that_can_resume              : ?devkind:devkind -> unit -> string list
    method get_node_names_that_can_startup             : ?devkind:devkind -> unit -> string list
    method get_node_names_that_can_suspend             : ?devkind:devkind -> unit -> string list
    method get_nodes_such_that                         : ?devkind:devkind -> (node -> bool) -> node list
    method get_nodes_that_can_gracefully_shutdown      : ?devkind:devkind -> unit -> node list
    method get_nodes_that_can_resume                   : ?devkind:devkind -> unit -> node list
    method get_nodes_that_can_startup                  : ?devkind:devkind -> unit -> node list
    method get_nodes_that_can_suspend                  : ?devkind:devkind -> unit -> node list
    method change_node_name : string -> string -> unit
    method involved_node_and_port_index_list : (node * int) list
    method busy_port_indexes_of_node   : node -> int list
    method max_busy_port_index_of_node : node -> int
    method port_no_lower_of            : node -> int
    (* --- *)
    method cable_exists : string -> bool
    method get_cable_by_name : string -> cable
    method get_cable_list : cable list
    method set_cable_list : cable list -> unit
    method is_cable_list_empty : bool
    method del_cable_by_name : string -> unit
    method get_cables_involved_by_node_name : string -> cable list
    method get_crossover_cable_names : string list
    method get_crossover_cables : cable list
    method get_direct_cable_names : string list
    method get_direct_cables : cable list
    method reversed_cable_set : bool -> string -> unit
    method reversed_cables : string list
    method are_there_almost_2_free_endpoints : bool
    (* --- *)
    method destroy_process_before_quitting : unit -> unit
    method dotTrad : unit -> string
    method eval_forest_attribute : Xforest.attribute -> unit
    method eval_forest_child : Xforest.tree -> unit
    method free_endpoint_list_humanly_speaking : ?force_to_be_included:(string * string) list -> (string * string) list
    method free_port_indexes_of_node           : ?force_to_be_included:int list -> node -> int list
    method free_user_port_names_of_node        : ?force_to_be_included:string list -> node -> string list
    method from_tree : Xforest.node -> Xforest.forest -> unit
    method reset : ?scheduled:bool -> unit -> unit
    method restore_from_buffers : unit
    method save_to_buffers : unit
    method show : unit
    method subscribe_a_try_to_add_procedure : ('a -> Xforest.tree -> bool) -> unit
    method suggestedName : string -> string
    method to_forest : Xforest.forest
    method to_tree : Xforest.tree
  end

module Xml :
  sig
    val network_marshaller : Xforest.t Oomarshal.marshaller
    val load_network       : project_version:[ `v0 | `v1 | `v2 ] -> network -> string -> unit
    val save_network       : network -> string -> unit
  end
