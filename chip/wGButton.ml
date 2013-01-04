
class button_wire_tuple ~owner ~system =
 let clicked  = Chip.wcounter ~name:(owner^"#clicked") ~system () in
 let enter    = Chip.wcounter ~name:(owner^"#enter")   ~system () in
 let leave    = Chip.wcounter ~name:(owner^"#leave")   ~system () in
 let pressed  = Chip.wcounter ~name:(owner^"#pressed") ~system () in
 let released = Chip.wcounter ~name:(owner^"#clicked") ~system () in
object
 method clicked  = clicked
 method enter    = enter
 method leave    = leave
 method pressed  = pressed
 method released = released

 method destroy () =
  clicked#destroy ();
  enter#destroy ();
  leave#destroy ();
  pressed#destroy ();
  released#destroy ()
end

type button_signal = Clicked | Enter | Leave | Pressed | Released

class button
 ?name
 ?parent
 (system:Chip.system)
 obj =
 let name = match name with None -> Chip.fresh_wire_name "button" | Some x -> x in
 let wire = new button_wire_tuple ~owner:name ~system in
object (self)
 inherit GButton.button (obj : Gtk.button Gtk.obj) as self_as_gbutton
 method wire = wire

 inherit [ button_signal, (int * int * int * int * int)] Chip.wire ~name ?parent system as self_as_wire

 method destroy () =
  wire#destroy ();
  self_as_wire#destroy ();
  self_as_gbutton#destroy ()

 method reset () = assert false (* unused! *) 
  
 method set_alone = function
 | Clicked  -> wire#clicked#set ()
 | Enter    -> wire#enter#set ()
 | Leave    -> wire#leave#set ()
 | Pressed  -> wire#pressed#set ()
 | Released -> wire#released#set ()

 method get_alone =
  (wire#clicked#get,
   wire#enter#get,
   wire#leave#get,
   wire#pressed#get,
   wire#released#get)

  method to_dot =
   Printf.sprintf "%d [shape=record, label=\"%s (%d) | {|{%s}|}\"];"
     self#id
     self#name
     self#id
     (Chip.dot_record_of_port_names
         [wire#clicked#name;
	  wire#enter#name;
	  wire#leave#name;
	  wire#pressed#name;
	  wire#released#name ])

initializer
 ignore (self#connect#clicked  ~callback:(wire#clicked#set));
 ignore (self#connect#enter    ~callback:(wire#enter#set));
 ignore (self#connect#leave    ~callback:(wire#leave#set));
 ignore (self#connect#pressed  ~callback:(wire#pressed#set));
 ignore (self#connect#released ~callback:(wire#released#set));
 let p = (Some self#as_common) in
  wire#clicked#set_parent  p;
  wire#enter#set_parent    p;
  wire#leave#set_parent    p;
  wire#pressed#set_parent  p;
  wire#released#set_parent p;
  ()
end

let pack_return create p ?packing ?show () =
 GObj.pack_return (create p) ~packing ~show

let button ?name ?parent (system:Chip.system) ?label =
  GtkButton.Button.make_params [] ?label ~cont:(
  pack_return (fun p -> new button ?name ?parent system (GtkButton.Button.create p)))
