(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007, 2008  Luca Saiu

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

open PreludeExtra.Prelude;; (* We want synchronous terminal output *)
open Sugar;;
open ListExtra;;
open StringExtra;;
open SysExtra;;
open StrExtra;;
open UnixExtra;;
open Hashmap;;
open Ledgrid;;
open Unix;;
open UnixExtra;;
open Gtk.Tags;;

let blinker_thread_socket_file_name =
  let parent =
    try
      Some (Global_options.get_project_working_directory ())
    with _ ->
      None in
  Unix.temp_file
    ?parent
    ~prefix:".marionnet-blinker-server-socket-"
    ();;

class ledgrid_manager =
object (self)
  (** Synchornization is automatically managed by methods, thus making 
      ledgrid_manager a monitor *)
  val mutex =
    Mutex.create ()
  method private lock =
    Mutex.lock mutex
  method private unlock =
    Mutex.unlock mutex
      
  val id_to_data =
    Hashmap.make ()
  
  method blinker_thread_socket_file_name =
    blinker_thread_socket_file_name

  (** Return a tuple (window, device, name, connected_port_indices). This is {e unlocked}! *)
  method private lookup (id : int) =
    try
      Hashmap.lookup id_to_data id
    with _ -> begin
      Log.print_string ("Warning: id_to_device: No device has id " ^ (string_of_int id) ^ "\n");
      failwith ("id_to_device: No device has id " ^ (string_of_int id))
    end

  (** This is {e unlocked}! *)
  method private id_to_device (id : int) =
    let _, device, _, _ = self#lookup id in
    device

  (** This is {e unlocked}! *)
  method private id_to_window (id : int) =
    let window, _, _, _ = self#lookup id in
    window

  (** This is {e unlocked}! *)
  method private id_to_name (id : int) =
    let _, _, name, _ = self#lookup id in
    name

  (** This is {e unlocked}! *)
  method private id_to_connected_ports (id : int) =
    let _, _, _, connected_ports = self#lookup id in
    connected_ports
  
  method get_connected_ports ~id () =
    self#lock;
    let result = self#id_to_connected_ports id in
    self#unlock;
    result
  
  (** This is {e unlocked}! *)
  method private update_connected_ports (id : int) new_connected_ports =
    let window, device, name, _ = self#lookup id in
    Hashmap.replace id_to_data id (window, device, name, new_connected_ports)
  
  (** Make the given ledgrid window always on top, and visible (this is a harmless side
      effect of the implementation; we always need the window to be visible anyway when
      calling this method) *)
  method private set_always_on_top id value : unit =
    let window = self#id_to_window id in
(*     window#misc#set_property "keep-above" (`BOOL true); *)
    let is_window_visible = true (*window#misc#hidden*) in
    (if is_window_visible then
      window#misc#hide ());
(*     window#misc#set_property "keep-above" (`BOOL true); *)
    window#set_type_hint
      (if value then `DIALOG else `NORMAL);
    window#set_position `MOUSE;
    (if is_window_visible then
      window#misc#show ());

  (** This is {e unlocked}! *)
  method private make_widget ~id ~ports_no ~title ~label ~image_directory () =
    let window =
      GWindow.window
        ~icon:Icon.icon_pixbuf
        ~title
        ~border_width:0
        ~resizable:false
        () in
    let frame = GBin.frame ~label (* ~shadow_type:`ETCHED_OUT *) ~packing:window#add () in
(*     let box = GPack.box `HORIZONTAL ~packing:frame#add () in  *)
    let vbox = GPack.box `VERTICAL ~packing:frame#add () in 
    let box = GPack.box `HORIZONTAL ~packing:vbox#add () in 
    let always_on_top_box = GPack.box `HORIZONTAL ~packing:vbox#add () in     
    let check_button =
      GButton.check_button (*~stock:`CUT*) ~label:"Always on top" ~packing:always_on_top_box#add () in
    ignore (check_button#connect#clicked
              ~callback:(fun () ->
                let state = check_button#active in
                self#set_always_on_top id state));
    (* Make a label which we don't need to name: *)
    ignore (GMisc.label ~text:"Activity" ~packing:box#add ());
    ignore (window#event#connect#delete
             ~callback:(fun _ -> Log.print_string "Sorry, no, you can't\n"; true));
    let device =
      new device_led_grid
        ~packing:box#add ~ports:ports_no ~show_100_mbs:false
        ~lines:(if ports_no > 8 then 2 else 1)
        ~angle:(if ports_no > 8 then 90.0 else 0.0)
        ~off_xpm_file_name:(image_directory^"/off.xpm")
        ~on_xpm_file_name:(image_directory^"/on.xpm")
        ~nothing_xpm_file_name:(image_directory^"/nothing.xpm")
        ()
    in
      (* Note how the window is {e not} shown by default: it's appropriate
         to show it only when the device is started up. *)
      window, device
  
  method make_device_ledgrid ~id ~title ~label ~ports_no ~image_directory 
      ?connected_ports:(connected_ports=[])() =
    self#lock;
    Log.print_string ("Making a ledgrid with title '" ^ title ^ "' (the device has id " ^
                  (string_of_int id) ^ "), with " ^ (string_of_int ports_no) ^
                  " ports\n");
    let ledgrid_widget, window_widget = 
      self#make_widget ~id ~ports_no ~title ~label ~image_directory () in
    Hashmap.add id_to_data id (ledgrid_widget, window_widget, title, connected_ports);
    ignore (List.map 
              (fun port -> self#set_port_connection_state ~id ~port ~value:true ())
              connected_ports);
    Log.print_string "Ok, done.\n";
    Log.print_string ("Testing (1): is id "^(string_of_int id)^" present in the table?...\n");
    (try
      let _ = self#id_to_device id in
      Log.print_string "Ok, passed.\n";
    with _ ->
      Log.print_string "FAILED.\n");
    Log.print_string ("Testing (2): is id "^(string_of_int id)^" present in the table?...\n");
    (try
      let _ = self#lookup id in
      Log.print_string "Ok, passed.\n";
    with _ ->
      Log.print_string "FAILED.\n");
    self#unlock

  method show_device_ledgrid ~id () =
    self#lock;
    (try
      (self#id_to_window id)#show ();
    with _ ->
      Log.print_string ("Warning: id "^(string_of_int id)^" unknown in show_device_ledgrid\n"));
    self#unlock

  method hide_device_ledgrid ~id () =
    self#lock;
    (try
      (self#id_to_window id)#misc#hide ();
    with _ ->
      Log.print_string ("Warning: id "^(string_of_int id)^" unknown in show_device_ledgrid\n"));
    self#unlock

  method destroy_device_ledgrid ~id () =
    self#lock;
    Log.print_string ("Destroying the ledgrid with id " ^
                  (string_of_int id) ^ "\n");
    (try
      (self#id_to_window id)#misc#hide ();
      (self#id_to_window id)#destroy ();
      Hashmap.remove id_to_data id
     with _ ->
       (print_string "WARNING: failed in destroy_device_ledgrid\n";
        Log.print_string ("  (id is "^(string_of_int id)^",\n")));
    self#unlock

  method set_port_connection_state ~id ~port ~value () = 
    self#lock;
    Log.print_string ("Making the port " ^ (string_of_int port) ^ " of device " ^
                  (string_of_int id) ^ (if value then " connected" else " disconnected") ^
                  "\n");
    (try
      (self#id_to_device id)#set port value;
      let new_connected_ports =
        if value then
          port :: (self#id_to_connected_ports id)
        else
          List.filter (fun p -> p != port) (self#id_to_connected_ports id) in
      self#update_connected_ports id new_connected_ports;
    with _ ->
      (print_string "WARNING: failed in set_port_connection_state\n";
       Log.print_string ("  (id is "^(string_of_int id)^",\n");
       Log.print_string ("   port is "^(string_of_int port)^")\n")));
    self#unlock
      
  method flash ~id ~port () = 
    self#lock;
    (try
(*       Log.print_string ("Flashing port " ^ (string_of_int port) ^ " of device " ^ *)
(*                     (self#id_to_name id) ^ "\n"); *)
      (self#id_to_device id)#flash port;
     with _ ->
       (print_string "WARNING: failed in flash\n";
        Log.print_string ("  (id is "^(string_of_int id)^",\n");
        Log.print_string ("   port is "^(string_of_int port)^")\n")));
    self#unlock

  (** Destroy all currently existing widgets and their data, so that we can start
      afresh with a new network: *)
  method reset =
    (* Log.print_string "\n\n*************** LEDgrid_manager: reset was called.\n\n"; *)
    let hashmap_as_alist = Hashmap.to_list id_to_data in
    ignore (List.map
              (fun (id, _) ->
                self#destroy_device_ledgrid ~id ();
                Hashmap.remove id_to_data id)
              hashmap_as_alist);

  val blinker_thread = ref None;

  method blinker_thread =
    match !blinker_thread with
      (Some blinker_thread) -> blinker_thread
    | None -> assert false

  method private make_blinker_thread =
    Log.print_string ("\n\n################# Making a blinker thread\n\n");
    Thread.create
      (fun () ->
        Log.print_string ("Making the socket\n");
        let socket = socket PF_UNIX SOCK_DGRAM 0 in
        let _ = try Unix.unlink blinker_thread_socket_file_name with _ -> () in
        Log.print_string ("Binding the socket\n");
        let _ = bind socket (ADDR_UNIX blinker_thread_socket_file_name) in
        Log.print_string ("Still alive\n");
        let maximum_message_size = 1000 in
        let buffer = String.create maximum_message_size in
        Log.print_string ("Ok, entering the thread main loop\n");
        while true; do
(*           Log.print_string ("\nWaiting for a string...\n"); *)
          let length = 
            try
              let (length, _) = recvfrom socket buffer 0 maximum_message_size [] in length
            with _ ->
              Log.print_string "SSSSSS recvfrom was interrupted by a signal.\n"; flush_all ();
              0 in
          let message = String.sub buffer 0 length in
(*           Log.print_string ("\n*** Received: >"^message^"<\n\n"); *)
          try
            let (id, port) = 
              Scanf.sscanf message "%i %i" (fun id port -> (id, port))
            in
            self#flash ~id ~port ();
          with _ ->
            try 
              let _ = 
                Scanf.sscanf message "please-die" (fun x -> x)
              in
              Log.print_string ("Exiting the LEDgrid manager blinker thread\n");
              Unix.close socket;
              let _ = try Unix.unlink blinker_thread_socket_file_name with _ -> () in
              Thread.exit ();
              Log.print_string ("!!! This should never be reached !!!\n");
              flush_all ();
            with _ ->
              Log.print_string ("Warning: can't understand the message >" ^ message ^ "<\n");
        done)
      ()
  
  initializer
    blinker_thread := Some self#make_blinker_thread

  (** This should be called before termination *)
  method kill_blinker_thread =
    let client_socket =
      Unix.socket PF_UNIX SOCK_DGRAM 0 in
    let client_socket_file_name =
      Filename.temp_file "blinker-killer-client-socket-" "" in
    (try Unix.unlink client_socket_file_name with _ -> ());
    Unix.bind client_socket (ADDR_UNIX client_socket_file_name);
    Log.printf "Sending the message \"please-die\" to the blinker thread...\n";
    flush_all ();
    let message = "please-die" in
    (try
      ignore (Unix.sendto
                client_socket
                message
                0
                ((String.length message))
                []
                (ADDR_UNIX blinker_thread_socket_file_name));
    with _ -> begin
      Log.print_string "!!!!!!!!!!! VERY SERIOUS !!!!!!!!!!\n"; flush_all ();
      Log.print_string "sending the message \"please-die\" to the blinker thread failed.\n"; flush_all ();
    end);
    Log.printf "  Ok.\n";
    flush_all ();
    (* Make sure this arrives right now: *)
(*     flush_all (); *)
(*     Thread.join (self#blinker_thread); *)
    Log.print_string "Ok, the blinker thread has exited now.\n";
    (try Unix.unlink client_socket_file_name with _ -> ());
    (try Unix.unlink blinker_thread_socket_file_name with _ -> ());
(*     Thread.kill self#blinker_thread *)
end;;

(** There must be exactly one instance of ledgrid_manager: *)
let the_one_and_only_ledgrid_manager = 
  new ledgrid_manager;;
