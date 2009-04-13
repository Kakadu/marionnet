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


open Gettext;;

(** Layouts for component-related menus. See the file gui_machine.ml for an example of application. *)

(** Function for insert or append entries to the toolbar_COMPONENTS *)
module Toolbar = struct

 (* Note that ~label:"" is very important in the call of GMenu.image_menu_item. Actually, it is a workaround
    of something that resemble to a bug in lablgtk: if not present, another external function is internally
    called by this function and the result is a menu entry with an horizontal line in background... *)
 let insert_image_menu pos (toolbar:GButton.toolbar) filename tooltip =
  let item = GButton.tool_item () in
  let menubar = GMenu.menu_bar ~border_width:0 ~width:0 ~height:60 ~packing:(item#add) () in
  let image_menu_item =
    let image = GMisc.image ~xalign:0.5 ~yalign:0.5 ~xpad:0 ~ypad:0 ~file:(Initialization.marionnet_home_images^filename) () in
    GMenu.image_menu_item ~label:"" ~image ~packing:menubar#add () in
  (* The call to insert_widget generates a GTK warning "Mixing deprecated and non-deprecated GtkToolbar API is not allowed".
     We consider it harmless. *)
  let () = toolbar#insert_widget ~tooltip ~pos item#coerce 
  in image_menu_item

 let append_image_menu =
  let pos = ref 0 in
  fun toolbar filename tooltip ->
   let result = insert_image_menu !pos toolbar filename tooltip in
   let () = incr pos in
   result

end (* module Toolbar *)

module type Toolbar_entry =
 sig
  val imagefile : string
  val tooltip   : string
 end

module type State = sig val st:State.globalState end

module Layout_for_network_component
 (State         : sig val st:State.globalState end)
 (Toolbar_entry : Toolbar_entry)
 (Add           : Menu_factory.Entry_callbacks)
 (Properties    : Menu_factory.Entry_with_children_callbacks)
 (Remove        : Menu_factory.Entry_with_children_callbacks)
 = struct

  let toolbar = State.st#mainwin#toolbar_COMPONENTS
  let image_menu_item = Toolbar.append_image_menu toolbar Toolbar_entry.imagefile Toolbar_entry.tooltip

  module F = Menu_factory.Make (struct
    let parent = Menu_factory.Menuitem (image_menu_item :> GMenu.menu_item_skel)
    let window = State.st#mainwin#window_MARIONNET
  end)

 module Add' = struct
   include Add
   let text  = (s_ "Add")
   let stock = `ADD
   end

 module Properties' = struct
   include Properties
   let text  = (s_ "Modify")
   let stock = `PROPERTIES
   end

 module Remove' = struct
   include Remove
   let text  = (s_ "Remove")
   let stock= `REMOVE
   end

 module Created_Add        = Menu_factory.Make_entry               (Add')        (F)
 module Created_Properties = Menu_factory.Make_entry_with_children (Properties') (F)
 module Created_Remove     = Menu_factory.Make_entry_with_children (Remove')     (F)

end


module Layout_for_network_node
 (State         : sig val st:State.globalState end)
 (Toolbar_entry : Toolbar_entry)
 (Add        : Menu_factory.Entry_callbacks)
 (Properties : Menu_factory.Entry_with_children_callbacks)
 (Remove     : Menu_factory.Entry_with_children_callbacks)
 (Startup    : Menu_factory.Entry_with_children_callbacks)
 (Stop       : Menu_factory.Entry_with_children_callbacks)
 (Suspend    : Menu_factory.Entry_with_children_callbacks)
 (Resume     : Menu_factory.Entry_with_children_callbacks)
 = struct
 
 module Startup' = struct
   include Startup
   let text  = (s_ "Start")
   let stock = `EXECUTE
   end

 module Stop' = struct
   include Stop
   let text  = (s_ "Stop")
   let stock = `MEDIA_STOP
   end

 module Suspend' = struct
   include Suspend
   let text  = (s_ "Suspend")
   let stock = `MEDIA_PAUSE
   end

 module Resume' = struct
   include Resume
   let text  = (s_ "Resume")
   let stock = `MEDIA_PLAY
   end

 module Created_entries_for_network_component = Layout_for_network_component (State) (Toolbar_entry) (Add) (Properties) (Remove)
 module F = Created_entries_for_network_component.F
 let () = F.add_separator ()
 module Created_Startup = Menu_factory.Make_entry_with_children (Startup') (F)
 module Created_Stop    = Menu_factory.Make_entry_with_children (Stop')    (F)
 let () = F.add_separator ()
 module Created_Suspend = Menu_factory.Make_entry_with_children (Suspend') (F)
 module Created_Resume  = Menu_factory.Make_entry_with_children (Resume')  (F)

end

module Layout_for_network_node_with_state
 (State             : sig val st:State.globalState end)
 (Toolbar_entry     : Toolbar_entry)
 (Add               : Menu_factory.Entry_callbacks)
 (Properties        : Menu_factory.Entry_with_children_callbacks)
 (Remove            : Menu_factory.Entry_with_children_callbacks)
 (Startup           : Menu_factory.Entry_with_children_callbacks)
 (Stop              : Menu_factory.Entry_with_children_callbacks)
 (Suspend           : Menu_factory.Entry_with_children_callbacks)
 (Resume            : Menu_factory.Entry_with_children_callbacks)
 (Ungracefully_stop : Menu_factory.Entry_with_children_callbacks)
 = struct
 
 module Ungracefully_stop' = struct
   include Ungracefully_stop
   let text  = (s_ "Shutdown")
   let stock = `DISCONNECT
   end

 module Created_entries_for_network_node = Layout_for_network_node (State) (Toolbar_entry) (Add) (Properties) (Remove) (Startup) (Stop) (Suspend) (Resume)
 module F = Created_entries_for_network_node.F
 let () = F.add_separator ()
 module Created_Ungracefully_stop = Menu_factory.Make_entry_with_children (Ungracefully_stop') (F)

end


module Layout_for_network_edge
 (State             : sig val st:State.globalState end)
 (Toolbar_entry     : Toolbar_entry)
 (Add        : Menu_factory.Entry_callbacks)
 (Properties : Menu_factory.Entry_with_children_callbacks)
 (Remove     : Menu_factory.Entry_with_children_callbacks)
 (Disconnect : Menu_factory.Entry_with_children_callbacks)
 (Reconnect  : Menu_factory.Entry_with_children_callbacks)
 = struct
 
 module Disconnect' = struct
   include Disconnect
   let text  = (s_ "Disconnect")
   let stock = `DISCONNECT
   end

 module Reconnect' = struct
   include Reconnect
   let text  = (s_ "Re-connect")
   let stock = `CONNECT
   end

 module Created_entries_for_network_component = Layout_for_network_component (State) (Toolbar_entry) (Add) (Properties) (Remove)
 module F = Created_entries_for_network_component.F
 let () = F.add_separator ()
 module Created_Disconnect = Menu_factory.Make_entry_with_children (Disconnect') (F)
 module Created_Reconnect  = Menu_factory.Make_entry_with_children (Reconnect') (F)

 (* Cable sensitiveness *)
 module Created_Add = Created_entries_for_network_component.Created_Add
 let () = State.st#add_sensitive_cable Created_Add.item#coerce

end

