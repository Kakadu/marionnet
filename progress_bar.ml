(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007  Luca Saiu

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

(* open PreludeExtra.Prelude;; (\* We want synchronous terminal output *\) *)

let progress_bars = ref [];;

let rec remove_from_list x xs =
  match xs with
  | [] -> []
  | (y, _) :: ys when x = y ->
      remove_from_list x ys
  | y :: ys ->
      y :: (remove_from_list x ys);;

let update_interval = 200;; (* in milliseconds *)

let destroy_progress_bar_dialog window =
  match !window with
    Some the_window -> begin
      print_string "A progress bar dialog window was destroyed.\n";
      flush_all ();
      the_window#destroy ();
      window := None;
      progress_bars := remove_from_list window !progress_bars;
    end
  | None -> begin
      print_string "A progress bar dialog window was destroyed\n";
      print_string "*more than once*. Doing nothing more.\n";
      flush_all ();
    end;;

let make_progress_bar_dialog
    ?title:(title="Une opération lente est en cours")
    ?text_on_bar:(text_on_bar="Patientez s'il vous plaît...")
    () =
  let window = GWindow.window ~title ~border_width:10 ~resizable:false () in
  window#set_icon (Some Icon.icon_pixbuf);
  let box = GPack.box `VERTICAL ~packing:window#add () in
  ignore (GMisc.label ~text:"Une opération lente est en cours" ()
            ~packing:box#add);
  let progress_bar = GRange.progress_bar ~pulse_step:0.1 ()
      ~packing:box#add in
  let the_ref = ref (Some window) in
  progress_bar#set_text text_on_bar;
(*  ignore (GMain.Timeout.add
            ~ms:update_interval
            ~callback:(fun () -> progress_bar#pulse(); true)); *)
  let destroy_callback : unit -> unit =
    fun () -> destroy_progress_bar_dialog the_ref in
  let hbox = GPack.box `HORIZONTAL ~border_width:0 ~packing:box#add () in
  let button =
    GButton.button ~label:"Cacher cette fenêtre" ~packing:hbox#add () in
  ignore (button#connect#clicked ~callback:destroy_callback);
  ignore (window#connect#destroy ~callback:destroy_callback);
  window#show ();
  progress_bars := (the_ref, progress_bar) :: !progress_bars;
  the_ref;;

let _ =
  GMain.Timeout.add
    ~ms:update_interval
    ~callback:(fun () ->
      List.iter
        (fun (_, progress_bar) -> progress_bar#pulse ())
        !progress_bars;
      true);; (* call this again at the next interval *)

(* let _ = make_progress_bar_dialog ~title:"one" () in *)
(* let _ = make_progress_bar_dialog ~title:"two" () in *)
(* let _ = make_progress_bar_dialog ~title:"three" () in *)
(* ();; *)

(* GMain.main ();; *)
