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

exception ProcessIsntInTheRightState of string

type process_name = Death_monitor.process_name (* string *)
type pid = Death_monitor.Map.key (* int *)

class virtual process :
  process_name ->
  process_name list ->
  ?stdin:Unix.file_descr ->
  ?stdout:Unix.file_descr ->
  ?stderr:Unix.file_descr ->
  unexpected_death_callback:(int -> string -> unit) ->
  unit ->
  object
    method append_arguments : process_name list -> unit
    method continue : unit
    method get_pid : pid
    method get_pid_option : pid option
    method gracefully_terminate : unit
    method is_alive : bool
    method spawn : unit
    method stop : unit
    method stop_monitoring : unit
    method terminate : unit
  end

class xnest_process :
  ?host_name_as_client:string ->
  ?display_as_client:string ->
  ?screen_as_client:string ->
  ?display_number_as_server:process_name ->
  unexpected_death_callback:(int -> process_name -> unit) ->
  title:'a ->
  unit ->
  object
    method append_arguments : process_name list -> unit
    method continue : unit
    method display_number_as_server : process_name
    method display_string_as_client : string
    method get_pid : pid
    method get_pid_option : pid option
    method gracefully_terminate : unit
    method is_alive : bool
    method spawn : unit
    method stop : unit
    method stop_monitoring : unit
    method terminate : unit
  end

class virtual process_which_creates_a_socket_at_spawning_time :
  process_name ->
  process_name list ->
  ?stdin:Unix.file_descr ->
  ?stdout:Unix.file_descr ->
  ?stderr:Unix.file_descr ->
  ?socket_name_prefix:string ->
  ?management_socket:unit ->
  unexpected_death_callback:(int -> process_name -> unit) ->
  unit ->
  object
    method append_arguments : process_name list -> unit
    method continue : unit
    method get_pid : pid
    method get_pid_option : pid option
    method get_socket_name : string
    method get_management_socket_name : string option
    method gracefully_terminate : unit
    method is_alive : bool
    method spawn : unit
    method stop : unit
    method stop_monitoring : unit
    method terminate : unit
  end

class hub_or_switch_process :
  ?hub:bool ->
  ?port_no:int ->
  ?tap_name:process_name ->
  ?socket_name_prefix:string ->
  ?management_socket:unit ->
  unexpected_death_callback:(int -> process_name -> unit) ->
  unit ->
  object
    method append_arguments : process_name list -> unit
    method continue : unit
    method get_pid : pid
    method get_pid_option : pid option
    method get_socket_name : process_name
    method get_management_socket_name : string option
    method gracefully_terminate : unit
    method is_alive : bool
    method spawn : unit
    method stop : unit
    method stop_monitoring : unit
    method terminate : unit
  end

class switch_process :
  port_no:int ->
  ?socket_name_prefix:string ->
  ?management_socket:unit ->
  unexpected_death_callback:(int -> process_name -> unit) ->
  unit ->
  object
    method append_arguments : process_name list -> unit
    method continue : unit
    method get_pid : pid
    method get_pid_option : pid option
    method get_socket_name : process_name
    method get_management_socket_name : string option
    method gracefully_terminate : unit
    method is_alive : bool
    method spawn : unit
    method stop : unit
    method stop_monitoring : unit
    method terminate : unit
  end

class hub_process :
  port_no:int ->
  ?socket_name_prefix:string ->
  ?management_socket:unit ->
  unexpected_death_callback:(int -> process_name -> unit) ->
  unit ->
  object
    method append_arguments : process_name list -> unit
    method continue : unit
    method get_pid : pid
    method get_pid_option : pid option
    method get_socket_name : process_name
    method get_management_socket_name : string option
    method gracefully_terminate : unit
    method is_alive : bool
    method spawn : unit
    method stop : unit
    method stop_monitoring : unit
    method terminate : unit
  end

class hublet_process :
  ?index:int ->
  unexpected_death_callback:(int -> process_name -> unit) ->
  unit ->
  object
    method append_arguments : process_name list -> unit
    method continue : unit
    method get_pid : pid
    method get_pid_option : pid option
    method get_socket_name : process_name
    method get_management_socket_name : string option
    method gracefully_terminate : unit
    method is_alive : bool
    method spawn : unit
    method stop : unit
    method stop_monitoring : unit
    method terminate : unit
  end

class slirpvde_process :
  ?network:process_name ->
  ?dhcp:unit ->
  existing_socket_name:process_name ->
  unexpected_death_callback:(int -> process_name -> unit) ->
  unit ->
  object
    method append_arguments : process_name list -> unit
    method continue : unit
    method get_pid : pid
    method get_pid_option : pid option
    method gracefully_terminate : unit
    method is_alive : bool
    method spawn : unit
    method stop : unit
    method stop_monitoring : unit
    method terminate : unit
  end

class unixterm_process :
  ?xterm_title:string ->
  management_socket_name:string ->
  unexpected_death_callback:(int -> process_name -> unit) ->
  unit ->
  object
    method append_arguments : process_name list -> unit
    method continue : unit
    method get_pid : pid
    method get_pid_option : pid option
    method gracefully_terminate : unit
    method is_alive : bool
    method spawn : unit
    method stop : unit
    method stop_monitoring : unit
    method terminate : unit
  end

val defects_to_command_line_options :
  ?rightward_loss:float ->
  ?rightward_duplication:float ->
  ?rightward_flip:float ->
  ?rightward_min_delay:float ->
  ?rightward_max_delay:float ->
  ?leftward_loss:float ->
  ?leftward_duplication:float ->
  ?leftward_flip:float ->
  ?leftward_min_delay:float ->
  ?leftward_max_delay:float -> unit -> string list

class ethernet_cable_process :
  left_end:< get_socket_name : string; .. > ->
  right_end:< get_socket_name : string; .. > ->
  ?blinker_thread_socket_file_name:process_name option ->
  ?left_blink_command:string option ->
  ?right_blink_command:string option ->
  ?rightward_loss:float ->
  ?rightward_duplication:float ->
  ?rightward_flip:float ->
  ?rightward_min_delay:float ->
  ?rightward_max_delay:float ->
  ?leftward_loss:float ->
  ?leftward_duplication:float ->
  ?leftward_flip:float ->
  ?leftward_min_delay:float ->
  ?leftward_max_delay:float ->
  unexpected_death_callback:(int -> process_name -> unit) ->
  unit ->
  object
    method append_arguments : process_name list -> unit
    method continue : unit
    method get_pid : pid
    method get_pid_option : pid option
    method gracefully_terminate : unit
    method is_alive : bool
    method spawn : unit
    method stop : unit
    method stop_monitoring : unit
    method terminate : unit
  end

type defects_object =
  < duplication : float;
    flip        : float;
    loss        : float;
    max_delay   : float;
    min_delay   : float >

val make_ethernet_cable_process :
  left_end:< get_socket_name : string; .. > ->
  right_end:< get_socket_name : string; .. > ->
  ?blinker_thread_socket_file_name:process_name option ->
  ?left_blink_command:string option ->
  ?right_blink_command:string option ->
  leftward_defects: defects_object ->
  rightward_defects: defects_object ->
  unexpected_death_callback:(int -> process_name -> unit) ->
  unit -> ethernet_cable_process

val ethernet_interface_to_boot_parameters_bindings :
  string -> int -> 'a -> (string * string) list

val ethernet_interface_to_uml_command_line_argument :
  string -> int -> < get_socket_name : string; .. > -> string

val random_ghost_mac_address : unit -> string

class uml_process :
  kernel_file_name:process_name ->
  filesystem_file_name:string ->
  cow_file_name:string ->
  ?swap_file_name:string ->
  ethernet_interface_no:int ->
  hublet_processes:< get_socket_name : string; .. > list ->
  memory:int ->
  console:string ->
  ?umid:string ->
  id:int ->
  ?show_unix_terminal:bool ->
  ?xnest_display_number:string ->
  unexpected_death_callback:(int -> process_name -> unit) ->
  unit ->
  object
    method append_arguments : process_name list -> unit
    method continue : unit
    method create_swap_file : unit
    method delete_swap_file : unit
    method get_pid : pid
    method get_pid_option : pid option
    method gracefully_terminate : unit
    method hostfs_directory_pathname : string
    method is_alive : bool
    method remove_hostfs_directory : unit
    method spawn : unit
    method stop : unit
    method stop_monitoring : unit
    method swap_file_name : string
    method terminate : unit
  end

type device_state = Off | On | Sleeping | Destroyed
val device_state_to_string : device_state -> string
exception CantGoFromStateToState of device_state * device_state

(* Provokes a Fatal error: exception Assert_failure("typing/ctype.ml", 261, 23)
   at compilation time: *)
(* type 'a user_level_parent_type = < get_name : string; .. > as 'a *)
(* using the constraint *)
(*    constraint 'parent = _ user_level_parent *)
(* almost 2 times *)

type user_level_parent = <
 get_name : string;
 ports_card : <
    get_my_inward_defects_by_index  : int -> defects_object;
    get_my_outward_defects_by_index : int -> defects_object;
    >
 >

class virtual ['parent] device :
  parent:'parent ->
  hublet_no:int ->
  unexpected_death_callback:(unit -> unit) ->
  unit ->
  object
    constraint 'parent = < get_name : string; .. > as 'b
    method virtual device_type : string
    method virtual spawn_processes : unit
    method virtual stop_processes : unit
    method virtual continue_processes : unit
    method virtual terminate_processes : unit

    method get_hublet_process_of_port : int -> hublet_process
    method get_hublet_process_list  : hublet_process list
    method get_hublet_no : int

    method get_state : device_state
    method gracefully_shutdown : unit
    method gracefully_terminate_processes : unit
    method hostfs_directory_pathname : string
    method startup : unit
    method suspend : unit
    method resume : unit
    method shutdown : unit
    method destroy : unit
    method execute_the_unexpected_death_callback : int -> string -> unit
  end

class virtual ['parent] main_process_with_n_hublets_and_cables_and_accessory_processes :
  parent:'parent ->
  hublet_no:int ->
  ?last_user_visible_port_index:int ->
  unexpected_death_callback:(unit -> unit) ->
  unit ->
  object
    constraint 'parent = <
      get_name : string;
      ports_card : <
	get_my_inward_defects_by_index  : int -> defects_object;
	get_my_outward_defects_by_index : int -> defects_object;
        .. >;
      .. >
    method continue_processes : unit
    method destroy : unit
    method virtual device_type : string
    method get_hublet_process_of_port : int -> hublet_process
    method get_hublet_process_list : hublet_process list
    method get_hublet_no : int
    method get_state : device_state
    method gracefully_shutdown : unit
    method gracefully_terminate_processes : unit
    method hostfs_directory_pathname : string
    method resume : unit
    method shutdown : unit
    method spawn_processes : unit
    method startup : unit
    method stop_processes : unit
    method suspend : unit
    method terminate_processes : unit
    method add_accessory_process : process -> unit
    method execute_the_unexpected_death_callback : int -> string -> unit
  end

class virtual ['parent] hub_or_switch :
  parent:'parent ->
  hublet_no:int ->
  ?last_user_visible_port_index:int ->
  hub:bool ->
  ?management_socket:unit ->
  unexpected_death_callback:(unit -> unit) ->
  unit ->
  object
    constraint 'parent = <
      get_name : string;
      ports_card : <
	get_my_inward_defects_by_index  : int -> defects_object;
	get_my_outward_defects_by_index : int -> defects_object;
        .. >;
      .. >
    method continue_processes : unit
    method destroy : unit
    method virtual device_type : string
    method get_hublet_process_of_port : int -> hublet_process
    method get_hublet_process_list : hublet_process list
    method get_hublet_no : int
    method get_state : device_state
    method gracefully_shutdown : unit
    method gracefully_terminate_processes : unit
    method hostfs_directory_pathname : string
    method resume : unit
    method shutdown : unit
    method spawn_processes : unit
    method startup : unit
    method stop_processes : unit
    method suspend : unit
    method terminate_processes : unit
    method add_accessory_process : process -> unit
    method get_management_socket_name : string option
    method execute_the_unexpected_death_callback : int -> string -> unit
  end


class virtual ['parent] machine_or_router :
  parent:'parent ->
  router:bool ->
  kernel_file_name:process_name ->
  filesystem_file_name:string ->
  cow_file_name:string ->
  ethernet_interface_no:int ->
  memory:int ->
  console:string ->
  xnest:bool ->
  ?umid:string ->
  id:int ->
  ?show_unix_terminal:bool ->
  unexpected_death_callback:(unit -> unit) ->
  unit ->
  object
    constraint 'parent = <
      get_name : string;
      ports_card : <
	get_my_inward_defects_by_index  : int -> defects_object;
	get_my_outward_defects_by_index : int -> defects_object;
        .. >;
      .. >
    method continue_processes : unit
    method destroy : unit
    method virtual device_type : string
    method get_hublet_process_of_port : int -> hublet_process
    method get_hublet_process_list : hublet_process list
    method get_hublet_no : int
    method get_state : device_state
    method gracefully_shutdown : unit
    method gracefully_terminate_processes : unit
    method hostfs_directory_pathname : string
    method resume : unit
    method shutdown : unit
    method spawn_processes : unit
    method startup : unit
    method stop_processes : unit
    method suspend : unit
    method terminate_processes : unit
    method execute_the_unexpected_death_callback : int -> string -> unit
  end

