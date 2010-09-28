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
  unexpected_death_callback:(int -> process_name -> unit) ->
  unit ->
  object
    method append_arguments : process_name list -> unit
    method continue : unit
    method get_pid : pid
    method get_pid_option : pid option
    method get_socket_name : string
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
  unexpected_death_callback:(int -> process_name -> unit) ->
  unit ->
  object
    method append_arguments : process_name list -> unit
    method continue : unit
    method get_pid : pid
    method get_pid_option : pid option
    method get_socket_name : process_name
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
  unexpected_death_callback:(int -> process_name -> unit) ->
  unit ->
  object
    method append_arguments : process_name list -> unit
    method continue : unit
    method get_pid : pid
    method get_pid_option : pid option
    method get_socket_name : process_name
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
  unexpected_death_callback:(int -> process_name -> unit) ->
  unit ->
  object
    method append_arguments : process_name list -> unit
    method continue : unit
    method get_pid : pid
    method get_pid_option : pid option
    method get_socket_name : process_name
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
    method gracefully_terminate : unit
    method is_alive : bool
    method spawn : unit
    method stop : unit
    method stop_monitoring : unit
    method terminate : unit
  end

class world_bridge_hub_process :
  tap_name:process_name ->
  unexpected_death_callback:(int -> process_name -> unit) ->
  unit ->
  object
    method append_arguments : process_name list -> unit
    method continue : unit
    method get_pid : pid
    method get_pid_option : pid option
    method get_socket_name : process_name
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

val ethernet_interface_to_boot_parameters_bindings :
  string -> int -> 'a -> (string * string) list

val ethernet_interface_to_uml_command_line_argument :
  string -> int -> < get_socket_name : string; .. > -> string

val random_ghost_mac_address : unit -> string

val create_swap_file_name : unit -> string

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

class virtual device :
  name:string ->
  hublet_no:int ->
  unexpected_death_callback:(unit -> unit) ->
  unit ->
  object
    method virtual continue_processes : unit
    method destroy : unit
    method virtual device_type : string
    method get_ethernet_port : int -> hublet_process
    method get_ethernet_port_by_name : string -> hublet_process
    method get_hublet_process : int -> hublet_process
    method get_hublet_processes : hublet_process list
    method get_hublet_no : int
    method get_state : device_state
    method gracefully_shutdown : unit
    method gracefully_terminate_processes : unit
    method hostfs_directory_pathname : string
    method resume : unit
    method set_hublet_process_no : int -> unit
    method shutdown : unit
    method virtual spawn_processes : unit
    method startup : unit
    method virtual stop_processes : unit
    method suspend : unit
    method virtual terminate_processes : unit
    method execute_the_unexpected_death_callback : int -> string -> unit

  end

class ethernet_cable :
  name:string ->
  left_end:< get_socket_name : string; .. > ->
  right_end:< get_socket_name : string; .. > ->
  ?blinker_thread_socket_file_name:process_name option ->
  ?left_blink_command:string option ->
  ?right_blink_command:string option ->
  unexpected_death_callback:(unit -> unit) ->
  unit ->
  object
    method continue_processes : unit
    method destroy : unit
    method device_type : string
    method get_ethernet_port : int -> hublet_process
    method get_ethernet_port_by_name : string -> hublet_process
    method get_hublet_process : int -> hublet_process
    method get_hublet_processes : hublet_process list
    method get_hublet_no : int
    method get_state : device_state
    method gracefully_shutdown : unit
    method gracefully_terminate_processes : unit
    method hostfs_directory_pathname : string
    method resume : unit
    method set_hublet_process_no : int -> unit
    method shutdown : unit
    method spawn_processes : unit
    method startup : unit
    method stop_processes : unit
    method suspend : unit
    method terminate_processes : unit
    method execute_the_unexpected_death_callback : int -> string -> unit
  end

class virtual main_process_with_n_hublets_and_cables :
  name:string ->
  hublet_no:int ->
  ?last_user_visible_port_index:int ->
  unexpected_death_callback:(unit -> unit) ->
  unit ->
  object
    method continue_processes : unit
    method destroy : unit
    method virtual device_type : string
    method get_ethernet_port : int -> hublet_process
    method get_ethernet_port_by_name : string -> hublet_process
    method get_hublet_process : int -> hublet_process
    method get_hublet_processes : hublet_process list
    method get_hublet_no : int
    method get_state : device_state
    method gracefully_shutdown : unit
    method gracefully_terminate_processes : unit
    method hostfs_directory_pathname : string
    method resume : unit
    method set_hublet_process_no : int -> unit
    method shutdown : unit
    method spawn_processes : unit
    method startup : unit
    method stop_processes : unit
    method suspend : unit
    method terminate_processes : unit
    method execute_the_unexpected_death_callback : int -> string -> unit
  end

class virtual hub_or_switch :
  name:string ->
  hublet_no:int ->
  ?last_user_visible_port_index:int ->
  hub:bool ->
  unexpected_death_callback:(unit -> unit) ->
  unit ->
  object
    method continue_processes : unit
    method destroy : unit
    method virtual device_type : string
    method get_ethernet_port : int -> hublet_process
    method get_ethernet_port_by_name : string -> hublet_process
    method get_hublet_process : int -> hublet_process
    method get_hublet_processes : hublet_process list
    method get_hublet_no : int
    method get_state : device_state
    method gracefully_shutdown : unit
    method gracefully_terminate_processes : unit
    method hostfs_directory_pathname : string
    method resume : unit
    method set_hublet_process_no : int -> unit
    method shutdown : unit
    method spawn_processes : unit
    method startup : unit
    method stop_processes : unit
    method suspend : unit
    method terminate_processes : unit
    method execute_the_unexpected_death_callback : int -> string -> unit
  end

class hub :
  name:string ->
  hublet_no:int ->
  ?last_user_visible_port_index:int ->
  unexpected_death_callback:(unit -> unit) ->
  unit ->
  object
    method continue_processes : unit
    method destroy : unit
    method device_type : string
    method get_ethernet_port : int -> hublet_process
    method get_ethernet_port_by_name : string -> hublet_process
    method get_hublet_process : int -> hublet_process
    method get_hublet_processes : hublet_process list
    method get_hublet_no : int
    method get_state : device_state
    method gracefully_shutdown : unit
    method gracefully_terminate_processes : unit
    method hostfs_directory_pathname : string
    method resume : unit
    method set_hublet_process_no : int -> unit
    method shutdown : unit
    method spawn_processes : unit
    method startup : unit
    method stop_processes : unit
    method suspend : unit
    method terminate_processes : unit
    method execute_the_unexpected_death_callback : int -> string -> unit
  end

class switch :
  name:string ->
  hublet_no:int ->
  ?last_user_visible_port_index:int ->
  unexpected_death_callback:(unit -> unit) ->
  unit ->
  object
    method continue_processes : unit
    method destroy : unit
    method device_type : string
    method get_ethernet_port : int -> hublet_process
    method get_ethernet_port_by_name : string -> hublet_process
    method get_hublet_process : int -> hublet_process
    method get_hublet_processes : hublet_process list
    method get_hublet_no : int
    method get_state : device_state
    method gracefully_shutdown : unit
    method gracefully_terminate_processes : unit
    method hostfs_directory_pathname : string
    method resume : unit
    method set_hublet_process_no : int -> unit
    method shutdown : unit
    method spawn_processes : unit
    method startup : unit
    method stop_processes : unit
    method suspend : unit
    method terminate_processes : unit
    method execute_the_unexpected_death_callback : int -> string -> unit
  end

(*class world_gateway :
  name:string ->
  user_port_no:int ->
  network_address:process_name ->
  dhcp_enabled:bool ->
  unexpected_death_callback:(unit -> unit) ->
  unit ->
  object
    method continue_processes : unit
    method destroy : unit
    method device_type : string
    method get_ethernet_port : int -> hublet_process
    method get_ethernet_port_by_name : string -> hublet_process
    method get_hublet_process : int -> hublet_process
    method get_hublet_processes : hublet_process list
    method get_hublet_no : int
    method get_state : device_state
    method gracefully_shutdown : unit
    method gracefully_terminate_processes : unit
    method hostfs_directory_pathname : string
    method resume : unit
    method set_hublet_process_no : int -> unit
    method shutdown : unit
    method spawn_processes : unit
    method startup : unit
    method stop_processes : unit
    method suspend : unit
    method terminate_processes : unit
    method execute_the_unexpected_death_callback : int -> string -> unit
  end*)

class virtual machine_or_router :
  name:string ->
  router:'a ->
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
    method continue_processes : unit
    method destroy : unit
    method virtual device_type : string
    method get_ethernet_port : int -> hublet_process
    method get_ethernet_port_by_name : string -> hublet_process
    method get_hublet_process : int -> hublet_process
    method get_hublet_processes : hublet_process list
    method get_hublet_no : int
    method get_state : device_state
    method gracefully_shutdown : unit
    method gracefully_terminate_processes : unit
    method hostfs_directory_pathname : string
    method resume : unit
    method set_hublet_process_no : int -> unit
    method shutdown : unit
    method spawn_processes : unit
    method startup : unit
    method stop_processes : unit
    method suspend : unit
    method terminate_processes : unit
    method execute_the_unexpected_death_callback : int -> string -> unit
  end

class machine :
  name:string ->
  filesystem_file_name:string ->
  kernel_file_name:process_name ->
  cow_file_name:string ->
  ethernet_interface_no:int ->
  ?memory:int ->
  ?umid:string ->
  ?xnest:bool ->
  id:int ->
  unexpected_death_callback:(unit -> unit) ->
  unit ->
  object
    method continue_processes : unit
    method destroy : unit
    method device_type : string
    method get_ethernet_port : int -> hublet_process
    method get_ethernet_port_by_name : string -> hublet_process
    method get_hublet_process : int -> hublet_process
    method get_hublet_processes : hublet_process list
    method get_hublet_no : int
    method get_state : device_state
    method gracefully_shutdown : unit
    method gracefully_terminate_processes : unit
    method hostfs_directory_pathname : string
    method resume : unit
    method set_hublet_process_no : int -> unit
    method shutdown : unit
    method spawn_processes : unit
    method startup : unit
    method stop_processes : unit
    method suspend : unit
    method terminate_processes : unit
    method execute_the_unexpected_death_callback : int -> string -> unit
  end

(*class router :
  name:string ->
  cow_file_name:string ->
  kernel_file_name:process_name ->
  filesystem_file_name:string ->
  ethernet_interface_no:int ->
  ?umid:string ->
  id:int ->
  show_unix_terminal:bool ->
  unexpected_death_callback:(unit -> unit) ->
  unit ->
  object
    method continue_processes : unit
    method destroy : unit
    method device_type : string
    method get_ethernet_port : int -> hublet_process
    method get_ethernet_port_by_name : string -> hublet_process
    method get_hublet_process : int -> hublet_process
    method get_hublet_processes : hublet_process list
    method get_hublet_no : int
    method get_state : device_state
    method gracefully_shutdown : unit
    method gracefully_terminate_processes : unit
    method hostfs_directory_pathname : string
    method resume : unit
    method set_hublet_process_no : int -> unit
    method shutdown : unit
    method spawn_processes : unit
    method startup : unit
    method stop_processes : unit
    method suspend : unit
    method terminate_processes : unit
    method execute_the_unexpected_death_callback : int -> string -> unit
  end*)

class world_bridge :
  name:string ->
  bridge_name:Daemon_language.bridge_name ->
  unexpected_death_callback:(unit -> unit) ->
  unit ->
  object
    method continue_processes : unit
    method destroy : unit
    method device_type : string
    method get_ethernet_port : int -> hublet_process
    method get_ethernet_port_by_name : string -> hublet_process
    method get_hublet_process : int -> hublet_process
    method get_hublet_processes : hublet_process list
    method get_hublet_no : int
    method get_state : device_state
    method gracefully_shutdown : unit
    method gracefully_terminate_processes : unit
    method hostfs_directory_pathname : string
    method resume : unit
    method set_hublet_process_no : int -> unit
    method shutdown : unit
    method spawn_processes : unit
    method startup : unit
    method stop_processes : unit
    method suspend : unit
    method terminate_processes : unit
    method execute_the_unexpected_death_callback : int -> string -> unit
  end

class cloud :
  name:string ->
  unexpected_death_callback:(unit -> unit) ->
  unit ->
  object
    method continue_processes : unit
    method destroy : unit
    method device_type : string
    method get_ethernet_port : int -> hublet_process
    method get_ethernet_port_by_name : string -> hublet_process
    method get_hublet_process : int -> hublet_process
    method get_hublet_processes : hublet_process list
    method get_hublet_no : int
    method get_state : device_state
    method gracefully_shutdown : unit
    method gracefully_terminate_processes : unit
    method hostfs_directory_pathname : string
    method resume : unit
    method set_hublet_process_no : int -> unit
    method shutdown : unit
    method spawn_processes : unit
    method startup : unit
    method stop_processes : unit
    method suspend : unit
    method terminate_processes : unit
    method execute_the_unexpected_death_callback : int -> string -> unit
  end
