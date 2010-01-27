(* This file is part of Marionnet, a virtual network laboratory
   Copyright (C) 2007, 2008  Luca Saiu
   Updated in 2009 by Luca Saiu

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

open Treeview;;
open Initialization;;
open Sugar;;
open Row_item;;
open Gettext;;

class texts_interface =
fun ~packing
(*    ~after_user_edit_callback *)
    () -> 
object(self)
  inherit
    treeview
      ~packing
      ~hide_reserved_fields:true
      ()
  as super
      
  (** Return the full pathname of the current working directory. Fail if it isn't set: *)
  method private get_working_directory =
    Filename.dirname self#file_name

  (** Display the document at the given row, in an asynchronous process: *)
  method private display row_id =
    let format = item_to_string (self#get_row_item row_id "Format") in
    let reader = self#format_to_reader format in
    let file_name = item_to_string (self#get_row_item row_id "FileName") in
    let command_line =
      Printf.sprintf "%s '%s/%s'&" reader self#get_working_directory file_name in
    ignore (Unix.system command_line)

  val error_message =
    (s_ "You should select an existing document in PDF, Postscript, DVI, HTML or text format.")

  (** Ask the user to choose a file, and return its pathname. Fail if the user doesn't
      choose a file or cancels: *)
  method private ask_file =
    let dialog = GWindow.file_chooser_dialog 
        ~icon:Icon.icon_pixbuf
        ~action:`OPEN 
        ~title:((*utf8*)(s_ "Choose the document to import"))
        ~modal:true () in
    dialog#add_button_stock `CANCEL `CANCEL;
    dialog#add_button_stock `OK `OK;
    dialog#unselect_all;
    dialog#add_filter
      (GFile.filter
         ~name:(s_ "Texts (PDF, PostScript, DVI, HTML, text)")
         ~patterns:["*.pdf"; "*.ps"; "*.dvi"; "*.text"; "*.txt"; "*.html"; "*.htm"; "README";
                    (s_ "README") (* it's nice to also support something like LISEZMOI... *)]
         ());
    dialog#set_default_response `OK;
    (match dialog#run () with
      `OK ->
        (match dialog#filename with
          Some result ->
            dialog#destroy ();
            Log.printf "* Ok: \"%s\"\n" result; flush_all ();
            result
        | None -> begin
            dialog#destroy ();
            failwith "No document was selected"
          end)
    | _ ->
        dialog#destroy ();
        Log.printf "* Cancel\n"; flush_all ();
        failwith "You cancelled");


  method private file_to_format pathname =
    if Filename.check_suffix pathname ".html" or
      Filename.check_suffix pathname ".htm" or
      Filename.check_suffix pathname ".HTML" or
      Filename.check_suffix pathname ".HTM" then
      "html"
    else if Filename.check_suffix pathname ".text" or
      Filename.check_suffix pathname ".txt" or
      Filename.check_suffix pathname "readme" or
      Filename.check_suffix pathname "lisezmoi" or
      Filename.check_suffix pathname ".TEXT" or
      Filename.check_suffix pathname ".TXT" or
      Filename.check_suffix pathname "README" or
      Filename.check_suffix pathname "LISEZMOI" then
      "text"
    else if Filename.check_suffix pathname ".ps" or
      Filename.check_suffix pathname ".eps" or
      Filename.check_suffix pathname ".PS" or
      Filename.check_suffix pathname ".EPS" then
      "ps"
    else if Filename.check_suffix pathname ".dvi" or
      Filename.check_suffix pathname ".DVI" then
      "dvi"
    else if Filename.check_suffix pathname ".pdf" or
      Filename.check_suffix pathname ".PDF" then
      "pdf"
    else
      failwith ("I cannot recognize the file type of " ^ pathname);
    
  method private format_to_reader format =
    match format with
    | "pdf" ->
        Initialization.configuration#string "MARIONNET_PDF_READER"
    | "ps" ->
        Initialization.configuration#string "MARIONNET_POSTSCRIPT_READER"
    | "dvi" ->
        Initialization.configuration#string "MARIONNET_DVI_READER"
    | "html" -> (* 'file' may recognize (X)HTML as XML... *)
        Initialization.configuration#string "MARIONNET_HTML_READER"
    | "text" ->
        Initialization.configuration#string "MARIONNET_TEXT_EDITOR"
    | "auto" -> (* the file type in unknown: web browsers can open most everything... *)
        Initialization.configuration#string "MARIONNET_HTML_READER"
    | _ ->
      failwith ("The format \"" ^ format ^ "\" is not supported");

  (** Import the given file, copying it into the appropriate directory with a fresh name;
      return the fresh name (just the file name, not a complete pathname) and the name
      of an application suitable to read it, as a pair. In case of failure show an error
      message and raise an exception. If ~move is true then the file is moved instead of
      copied. *)
  method private import_file ?(move=false) pathname =
    try
      let format =
        self#file_to_format pathname in
      let fresh_pathname =
        UnixExtra.temp_file ~parent:self#get_working_directory ~prefix:"document-" () in
      let fresh_name = 
        Filename.basename fresh_pathname in
      let redirection =
        Global_options.debug_mode_redirection () in
      let program =
        if move then "mv" else "cp -a" in
      let command_line =
          Printf.sprintf
            "%s '%s' '%s' %s && chmod a-w '%s'" program pathname fresh_pathname redirection fresh_pathname
      in
      (match Unix.system command_line with
        Unix.WEXITED 0 ->
          fresh_name, format
      | _ -> begin
          (* Copying failed: remove any partial copy which may have been created: *)
          let rm_command_line =
            Printf.sprintf "rm -f '%s' %s" fresh_pathname redirection in
          ignore (Unix.system rm_command_line);
          failwith ("The copy of \n\"" ^ pathname ^ "\"\n in \n\""^ fresh_pathname ^"\"\nfailed")
        end)
    with (Failure title) as e -> begin
      Simple_dialogs.error title error_message ();
      raise e (* Re-raise *)
    end

  method import_report ~machine_or_router_name ~pathname () =
    let title = (s_ "Report on ") ^ machine_or_router_name in
    let row_id = self#import_document ~move:true pathname in
    self#set_row_item row_id "Title" (String title);
    self#set_row_item row_id "Author" (String "-");
    self#set_row_item row_id "Type" (String (s_ "Report"));
    self#set_row_item row_id "Comment" (String ((s_ "created on ") ^ (Timestamp.current_timestamp_as_string ())));

  method import_history ~machine_or_router_name ~pathname () =
    let title = (s_ "History of ") ^ machine_or_router_name in
    let row_id = self#import_document ~move:true pathname in
    self#set_row_item row_id "Title" (String title);
    self#set_row_item row_id "Author" (String "-");
    self#set_row_item row_id "Type" (String (s_ "History"));
    self#set_row_item row_id "Comment" (String ((s_ "created on ") ^ (Timestamp.current_timestamp_as_string ())));

  method import_document ?(move=false) user_path_name =
    let internal_file_name, format = self#import_file user_path_name in
    let row_id =
      self#add_row
        [ "FileName", String internal_file_name;
          "Format", String format ] in
    self#save;
    row_id

  initializer
    let _ =
      self#add_icon_column
        ~shown_header:(s_ "Icon")
        ~header:"Icon"
        ~strings_and_pixbufs:[ "text", marionnet_home_images^"treeview-icons/text.xpm"; ]
        ~default:(fun () -> Icon "text")
        () in
    let _ =
      self#add_editable_string_column
        ~shown_header:(s_ "Title")
        ~header:"Title"
        ~italic:true
        ~default:(fun () -> String "Please edit this")
        () in
    let _ =
      self#add_editable_string_column
        ~shown_header:(s_ "Author")
        ~header:"Author"
        ~italic:false
        ~default:(fun () -> String "Please edit this")
        () in
    let _ =
      self#add_editable_string_column
        ~shown_header:(s_ "Type")
        ~header:"Type"
        ~italic:false
        ~default:(fun () -> String "Please edit this")
        () in
    let _ =
      self#add_editable_string_column
        ~shown_header:(s_ "Comment")
        ~header:"Comment"
        ~italic:true
        ~default:(fun () -> String "Please edit this")
        () in
    let _ =
      self#add_string_column
        ~header:"FileName"
        ~hidden:true
        () in
    let _ =
      self#add_string_column
        ~header:"Format"
        ~default:(fun () -> String "auto") (* unknown format; this is usefule for
                                              backward-compatibility, as this column
                                              didn't exist in older Marionnet versions *)
        ~hidden:true
        () in
    (* Make internal data structures: no more columns can be added now: *)
    self#create_store_and_view;

    (* Setup the contextual menu: *)
    self#set_contextual_menu_title "Texts operations";
    let get = raise_when_none in (* just a convenient alias *)
    self#add_menu_item
      (s_ "Import a document")
      (fun _ -> true)
      (fun _ ->
        ignore (self#import_document self#ask_file));

    self#add_menu_item
      (s_ "Display this document")
      is_some
      (fun selected_rowid_if_any ->
        let row_id = get selected_rowid_if_any in
        self#display row_id);
    self#set_double_click_on_row_callback (fun row_id -> self#display row_id);

    self#add_menu_item
      (s_ "Remove this document")
      is_some
      (fun selected_rowid_if_any ->
        let row_id = get selected_rowid_if_any in
        let file_name = item_to_string (self#get_row_item row_id "FileName") in
        let command_line =
          Printf.sprintf "rm -f \"%s/%s\"&" self#get_working_directory file_name in
        ignore (Unix.system command_line);
        self#remove_row row_id;
        self#save);
end;;

(** Ugly kludge to make a single global instance visible from all modules
    linked *after* this one. Not having mutually-recursive inter-compilation-unit
    modules is a real pain. *)
let the_texts_interface =
  ref None;;
let get_texts_interface () =
  match !the_texts_interface with
    None -> failwith "No texts interface exists"
  | Some the_texts_interface -> the_texts_interface;;
let make_texts_interface ~packing () =
  let result = new texts_interface ~packing () in
  the_texts_interface := Some result;
  result;;
