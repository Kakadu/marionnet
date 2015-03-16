(* This file is part of our reusable OCaml BRICKS library
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

(** Ledgrid widgets. *)

(** {2 Constants}
    Some global constant definitions, for fine-tuning. *)

(** The duration of a LED light "flash", in milliseconds: *)
let flash_duration = 80 (* 125 *)

(** The duration of a LED light "blink", in milliseconds. The time is
    measured from the first to the last state change: *)
let blink_duration = 250

(** How many times a LED light changes state during a blink. This
    includes both on->off and off->on transitions: *)
let blink_toggles_no = 8 (* 4 times on + 4 times off *)

(** {2 Exception}
    The ways this brick can fail. *)

(** An exception raised whenever the user refers a non-existing LED light:
    in a LED grid *)
exception Non_existing_led_light of int * int

(** An exception raised whenever the user refers a non-existing port in a
    device LED grid: *)
exception Non_existing_port of int

(** {2 Utility stuff} *)

(** Make a pixmap data structure (not a widget) from the given file: *)
let make_pixmap_from_xpm_file ~file_name =
  GDraw.pixmap_from_xpm ~file:file_name ()

(** {2 A single LED light}
    Gtk+ simulation of just {e one} LED light. Particularly useful when arranged in a
    grid. *)

(** A LED light is a widget mimicking a single physical LED light, whose state
    at any given moment can be on or off: its state is represented as a boolean
    value, and by convention 'true' means 'on'. A LED light keeps its default
    state until its state is explitly changed by the user. The user can simply set
    the object's state, or can set its state *also changing the default*. As soon
    as the current state changes the widget's appearance on screen is updated.
    A LED light can be also 'flashed', i.e. set to its non-default value for a
    short time, after which it automatically reverts to its default state, or
    'blinked', i.e. ordered to repeatedly toggle its state very fast for a short
    time, before automatically reverting to its default state.
    Flashing and blinking are *asynchronous* operations: when the user requests
    them they are scheduled to be executed in background, and the user is immediately
    given back control. This allows us to use concurrency in an extremely simple way,
    without even exposing a thread interface.
    LED lights can be used in isolation, but they are mainly intended to be arranged
    within a grid, allowing for more complex behaviour.
    Note that already initialized Gtk+ pixmap objects of type GDraw.pixmap (and
    *not* widgets) must be explicitly supplied at construction time. Pixmaps can
    and should be shared among differnet LED lights. *)
class led_light ?default:(default=false) ?x:(x= -1) ?y:(y= -1) ~off_pixmap ~on_pixmap ~packing () =
object(self)
  (** A notebook with hidden tabs and border is the main widget: it contains two pages with
      the 'on' and 'off' pixmaps, and can easily change state by 'going' to a different
      page: *)
  val notebook =
    let notebook = GPack.notebook ~tab_pos:`TOP ~packing ~show_border:false ~show_tabs:false () in
    let _ = (* "on" pixmap widget *)
      GMisc.pixmap
        off_pixmap
        ~packing:(fun widget -> ignore (notebook#insert_page ~pos:0 widget))
        () in
    let _ = (* "on" pixmap widget *)
      GMisc.pixmap
        on_pixmap
        ~packing:(fun widget -> ignore (notebook#insert_page ~pos:1 widget))
        () in
      notebook

  (** Default state and current state; see above: *)
  val default = ref(default)
  val state = ref(false)

  (** Return the current default state: *)
  method get_default = !default

  (** Update the default state *and also the current state*; this changes the widget's
      appearance if the new value is different from the current state: *)
  method set_default value = default := value;
                             self#set value;
                             ()

  (** Return the current state: *)
  method get = !state

  (** Update the current state, possibly changing the widget's appearance: *)
  method set value = state := value;
                     notebook#goto_page (if value then 1 else 0);
                     ()

  (** Set the widget current state to be equal to its default. This may change the
      widget's appearance: *)
  method reset = self#set(!default); ()

  (** Set the widget current state to be on if it's currently off, or vice-versa.
      This always changes the widget's appearance: *)
  method toggle = self#set(not self#get); ()

  (** Return the widget position as it was set at creation time, or (-1, -1) if
      it was not set: *)
  method get_position = x, y

  (** Return the main Gtk+ widget making up the LED light: *)
  method get_widget = notebook

  (** Order the LED light to flash (see above) for the established time, and
      return immediately: *)
  method flash = self#set (not !default);
                 ignore (GMain.Timeout.add
                           flash_duration
                           (function () -> self#reset; false))

  (** Schedule the LED light to blink 'times' times, then to reset itself. This
      is internally used to implement blinking: *)
  method private blink_this_number_of_times times =
  if times = 0 then
    self#set(!default)
  else begin
    self#toggle;
    ignore (GMain.Timeout.add
              (blink_duration / blink_toggles_no)
              (fun () -> self#blink_this_number_of_times (times - 1); false));
  end

  (** Order the LED light to blink (see above) for the established time, and
      return immediately: *)
  method blink = self#blink_this_number_of_times blink_toggles_no; ()

  (** This just assures that the default state reflects what is visually
      displayed at creation time: *)
  initializer self#set !default
end

(** These variables are just used as parameters to Array.make so that types can be
    correctly inferred. useless_label's widget is never displayed: *)
let useless_array_of_led_light_options = Array.make 0 None
let useless_label = GMisc.label ()


(** {2 LED grid}
    Gtk+ simulation of a {e grid} of LED lights. *)

(** A LED grid visually represents a matrix of LED lights, where each light is
    independently controllable. A light is identified by its 0-based coordinates,
    where the origin is top-left.
    The optional parameter no_leds_at represents a list of coordinates (such as
    [(0, 1); (3, 4)]) where *no* lights should be placed.
    Each end of each row and column contains an optional, user-settable text
    label. Vertical labels can be rotated, to allow for denser writing in
    vertical.
    The constructor expects three file names identifying the XPM images to use
    for the 'on' state, the 'off' state, and for representing the absence of a
    light. All three pixmaps should have the same size. *)
class led_grid ?default:(default=false)
               ~on_xpm_file_name ~off_xpm_file_name ~nothing_xpm_file_name
               ~columns ~rows ~packing ?angle:(angle=90.0)
               ?no_leds_at:(no_leds_at=[]) () = object(self)
  (** The pixmap objects made from user-supplied files. Notice how the same three
      pixmaps are shared among all the lights (and 'holes'): *)
  val off_pixmap = make_pixmap_from_xpm_file ~file_name:off_xpm_file_name
  val on_pixmap = make_pixmap_from_xpm_file ~file_name:on_xpm_file_name
  val nothing_pixmap = make_pixmap_from_xpm_file ~file_name:nothing_xpm_file_name

  (** A two-dimensional matrix of led_light option: *)
  val led_lights_matrix = Array.make columns useless_array_of_led_light_options

  (** Arrays holding the label widgets decorating each end of rows and columns: *)
  val left_labels = Array.make rows useless_label
  val right_labels = Array.make rows useless_label
  val top_labels = Array.make columns useless_label
  val bottom_labels = Array.make columns useless_label

  (** The Gtk+ widget holding the whole grid: *)
  val table_widget = GPack.table ~columns:(columns + 2) ~rows:(rows + 2) ~row_spacings:0 ~col_spacings:0
                                 ~border_width:0 ~packing ()
  (* To do: use Jean's sets instead of this ugly hash: *)
  (** A set of positions which should be left empty. This structure must be
      accessed associatively at initialization time, and is more efficient
      than a list: *)
  val no_leds_at = let hash = Hashtbl.create (columns * rows) in
                     List.iter (fun x_y -> Hashtbl.add hash x_y ()) no_leds_at;
                     hash

  (** Initialize the complex state of the grid: *)
  initializer
  for x = 0 to columns - 1 do
    Array.set led_lights_matrix x (Array.make rows None);
    for y = 0 to rows - 1 do
      if Hashtbl.mem no_leds_at (x, y) then begin
        let _ = GMisc.pixmap
                  nothing_pixmap
                  ~packing:(table_widget#attach ~left:(x + 1) ~top:(y + 1) ~expand:`BOTH)
                  () in
        Array.set (Array.get led_lights_matrix x) y None
      end else
        let new_led_light =
          new led_light ~packing:(table_widget#attach ~left:(x + 1) ~top:(y + 1) ~expand:`BOTH)
                      ~off_pixmap ~on_pixmap ~default ~x ~y ()
        in
          Array.set (Array.get led_lights_matrix x) y (Some new_led_light)
    done;
  done;
  for y = 0 to rows - 1 do
     let left_label = GMisc.label ~packing:(table_widget#attach ~left:0 ~top:(y + 1)) () in
     let right_label = GMisc.label ~packing:(table_widget#attach ~left:(columns + 1) ~top:(y + 1)) () in
     Array.set left_labels y left_label;
     Array.set right_labels y right_label;
  done;
  for x = 0 to columns - 1 do
    let top_label = GMisc.label ~packing:(table_widget#attach ~left:(x + 1) ~top:0) () in
    let bottom_label = GMisc.label ~packing:(table_widget#attach ~left:(x + 1) ~top:(rows + 1)) () in
    top_label#set_angle angle;
    bottom_label#set_angle angle;
    Array.set top_labels x top_label;
    Array.set bottom_labels x bottom_label;
  done

  (** Return the LED light identified by (x, y), or throw an exception if no light is
      present at that position: *)
  method get_led_light x y =
    match Array.get (Array.get led_lights_matrix x) y with
      None -> raise (Non_existing_led_light(x, y))
    | Some(led_light) -> led_light
  method get = self#get_led_light

  (** Return a random LED light belonging to the grid, if it exists, or loop forever.
      This is useful for debugging (and for demos :-)): *)
  method get_random_led_light =
    let x, y = (Random.int columns, Random.int rows) in
    try
      self#get_led_light x y
    with Non_existing_led_light(_) ->
      self#get_random_led_light

  (** Get and set the text of each label. Notice that all arrays are 0-based: *)
  method get_top_label x = (Array.get top_labels x)#text
  method set_top_label x text = (Array.get top_labels x)#set_text text
  method get_bottom_label x = (Array.get bottom_labels x)#text
  method set_bottom_label x text = (Array.get bottom_labels x)#set_text text
  method get_left_label y = (Array.get left_labels y)#text
  method set_left_label y text = (Array.get left_labels y)#set_text text
  method get_right_label y = (Array.get right_labels y)#text
  method set_right_label y text = (Array.get right_labels y)#set_text text

  (* Set the rotation angle, (90.0 degrees by default) for column labels: *)
  method set_top_labels_angle alpha = for x = 0 to columns - 1 do
                                        (Array.get top_labels x)#set_angle alpha;
                                      done
  method set_bottom_labels_angle alpha = for x = 0 to columns - 1 do
                                           (Array.get bottom_labels x)#set_angle alpha;
                                         done

  (** Return the Gtk+ widget holding the whole grid: *)
  method get_widget = table_widget
end

(** To do: recycle this from Jean's library *)
let rec range a b =
  if a > b then [] else a :: (range (a + 1) b)

(** {2 Device LED Grid}
    A matrix of LED lights simulating the control panel of a phisical network
    device such as a switch or a router. *)

(** A 'device LED grid' is a LED grid specialized as a realistic simulation of
    the control panel of a physical device such as a switch, a hub or a router.
    A device LED's appearance can be customized at creation time, and this class
    allows us to control each _port_, abstracting from the position of the
    light or lights representing the port state.
    Port information can be displayed in either one or two lines, and an optional
    "100Mb/s" array of lights can also be shown. The number of ports must be even
    when two lines are requested. Three pixmap file names are required at creation
    time, as for the LED grid. Labels are automatically set.
    Reflecting the interface of common network devices, it can be said that a port
    is either in 'connected' or 'disconnected' state, meaning that its associated
    lights are 'on' or 'off' (and discounting flashes and blinks).
    Notice that, as in most real-world switch and hubs, port numeration is
    1-based. *)
class device_led_grid
  ~on_xpm_file_name
  ~off_xpm_file_name
  ~nothing_xpm_file_name
  ?(show_100_mbs=true)
  ~ports
  ?(port_labelling_offset=0)
  ~packing
  ?(angle=90.0)
  ?(lines=1)
  () =
(* Let's prevent stupid errors... *)
let _ = assert(ports > 1) in
let _ = assert(((ports mod 2) = 0) || (lines = 1)) in
let _ = assert((lines = 1) || (lines = 2))
in
object(self)
  inherit led_grid
    ~default:false
    ~on_xpm_file_name
    ~off_xpm_file_name
    ~nothing_xpm_file_name
    ~columns:(if lines = 1 then ports else ports / 2)
    ~angle
    ~rows:(match lines, show_100_mbs with
           | 1, false -> 1
           | 1, true  -> 2
           | 2, false -> 3
           | 2, true  -> 5
           | _ -> assert false)
    ~no_leds_at:(match lines, show_100_mbs with
           | 1, _     -> []
           | 2, false -> List.map (function x -> x, 1) (range 0 (ports - 1))
           | 2, true  -> List.map (function x -> x, 2) (range 0 (ports / 2 - 1))
           | _ -> assert false)
   ~packing () as super

  (** Initialize the complex state of this object: *)
  initializer
    for x = 0 to (if lines = 1 then ports - 1 else ports / 2 - 1) do
(*       self#set_top_label x (string_of_int (x + 1)); *)
      self#set_top_label x (string_of_int (x+port_labelling_offset)); (* 0-based numbering *)
    done;
    if lines = 2 then
    for x = ports / 2 to ports - 1 do
      self#set_bottom_label (x - ports / 2) (string_of_int (x+port_labelling_offset)); (* 0-based numbering *)
    done;
    self#set_right_label 0 "TX/RX";
    match lines, show_100_mbs with
      1, false -> ()
    | 2, false -> self#set_right_label 2 "TX/RX"
    | 1, true ->  self#set_right_label 1 "100Mb/s"
    | 2, true ->  self#set_right_label 1 "100Mb/s";
                  self#set_right_label 3 "100Mb/s";
                  self#set_right_label 4 "TX/RX"
    | _ -> assert false;

  (** Given a port number, return a list of pairs of coordinates identifying the
      inolved lights: *)
  method private port_to_positions port =
    let port = port + 1 in (* kludge to implement 0-based numbering... *)
    match lines, show_100_mbs, port <= (ports / 2) with
      1, false, _     -> [ port - 1, 0 ]
    | 2, false, true  -> [ port - 1, 0 ]
    | 2, false, false -> [ port - (ports / 2) - 1, 2 ]
    | 1, true, _      -> [ port - 1, 0; port - 1, 1 ]
    | 2, true, true   -> [ port - 1, 0; port - 1, 1 ]
    | 2, true, false  -> [ port - (ports / 2) - 1, 3; port - (ports / 2) - 1, 4 ]
    | _               -> assert false

  (** Print the port->coordinates mapping before returning the result of calling
      port_to_positions: *)
  method private port_to_positions_ port =
    let positions = self#port_to_positions port in
    (*List.iter (function x, y ->
      print_int port; print_string " -> ("; print_int x; print_string ", ";
                   print_int y; print_string ")\n")
      positions;*)
    positions

  (** Given a port number, return the list of LED lights representing it: *)
  method private port_to_led_lights port =
    let positions = self#port_to_positions port in
      List.map (function x, y -> super#get x y) positions

  (** For each LED light representing the given port, call the given function and
      return the list of results: *)
  method private for_each_led_light (f : led_light -> 'a) (port : int) : 'a list
      = List.map (function x, y -> f (super#get x y))
                 (self#port_to_positions port);

  (** Ask every LED light representing the given port to (asynchronously) flash: *)
  method flash port =
    ignore (self#for_each_led_light (function led -> led#flash) port)

  (** Ask every LED light representing the given port to (asynchronously) blink: *)
  method blink port =
    ignore (self#for_each_led_light (function led -> led#blink) port)

  (** Set the state of all LED lights representing a port, updating their default
      state: this is a good way to indicate a cable connection or disconnection: *)
  method set port value =
    ignore (self#for_each_led_light (function led -> led#set_default value) port)
  method connect port = self#set port true
  method disconnect port = self#set port false

  (** Return the number of a random port currently in the 'On' state, or loop forever
      if no such port exists. This is useful for debugging and demos :-) *)
  method random_connected_port = (* This does not terminate if there are no connected ports! *)
(*     let port = (Random.int ports) + 1 in *)
    let port = (Random.int ports) in (* 0-based numbering *)
      if self#is_connected port then
        port
      else
        self#random_connected_port

  (** Return true iff the given port is in connected state: *)
  method is_connected port = List.hd (List.map (function led -> led#get_default)
                                               (self#port_to_led_lights port))
end

(** {3 Example}
    A trivial usage example.

{[let main ports () =
  let window = GWindow.window ~title:"Switch n.2" ~border_width:0 () in
  window#connect#destroy ~callback:GMain.Main.quit;
  let grid =
    new device_led_grid ~packing:window#add ~ports ~show_100_mbs:true ~lines:2
      ~off_xpm_file_name:"sample-files/off.xpm"
      ~on_xpm_file_name:"sample-files/on.xpm"
      ~nothing_xpm_file_name:"sample-files/nothing.xpm"
      () in
  for i = 1 to ports / 3 do
    grid#connect ((Random.int ports) + 1);
  done;

  (** Simulate a distinct communication between two ports every 50 milliseconds: *)
  GMain.Timeout.add 50 (function () -> grid#blink (grid#random_connected_port);
                                       grid#blink (grid#random_connected_port);
                                       true);
  window#show ();
  Main.main ()
let _ = main 64 ()]} *)

(* To do: should any out-of-bounds access raise a non-existing-led-light exception? *)

