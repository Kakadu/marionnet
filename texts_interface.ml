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

open PreludeExtra.Prelude;; (* We want synchronous terminal output *)
open Treeview;;
open Pathnames;;
open ListExtra;;
open UnixExtra;;
open Sugar;;
open Row_item;;

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
    let reader = item_to_string (self#get_row_item row_id "Reader") in
    let file_name = item_to_string (self#get_row_item row_id "FileName") in
    let command_line =
      Printf.sprintf "%s '%s/%s'&" reader self#get_working_directory file_name in
    ignore (Unix.system command_line)

  val error_message =
    "Vous devez sélectionner un document existant et lisible au format PDF, Postscript, DVI ou texte."

  (** Ask the user to choose a file, and return its pathname. Fail if the user doesn't
      choose a file or cancels: *)
  method private ask_file =
    let dialog = GWindow.file_chooser_dialog 
        ~icon:Icon.icon_pixbuf
        ~action:`OPEN 
        ~title:((*utf8*)"Choisissez le document à importer")
        ~modal:true () in
    dialog#add_button_stock `CANCEL `CANCEL;
    dialog#add_button_stock `OK `OK;
    dialog#unselect_all;
    dialog#add_filter
      (GFile.filter
         ~name:"Textes (PDF, PostScript, DVI, HTML, text)"
         ~patterns:["*.pdf"; "*.ps"; "*.dvi"; "*.text"; "*.txt"; "*.html"; "*.htm"; "README"; "LISEZMOI"]
         ());
    (match dialog#run () with
      `OK ->
        (match dialog#filename with
          Some result ->
            dialog#destroy ();
            Printf.printf "* Ok: \"%s\"\n" result; flush_all ();
            result
        | None -> begin
            dialog#destroy ();
            failwith "No document was selected"
          end)
    | _ ->
        dialog#destroy ();
        Printf.printf "* Cancel\n"; flush_all ();
        failwith "You cancelled");

  (** Return true iff the given file exists: *)
  method private does_file_exists pathname =
    let command_line =
      Printf.sprintf "file '%s' &> /dev/null" pathname in
    match Unix.system command_line with
      Unix.WEXITED 0 -> true
    | _ -> false

  (** Return true iff the given file exists and has the given type, as returned within
      the output of file(1): *)
  method private has_file_type pathname file_type =
    match file_type with
    | "HTML" | "XML" ->
      (Filename.check_suffix pathname ".html") or 
      (Filename.check_suffix pathname ".htm")
    | "text" ->
      (Filename.check_suffix pathname ".text") or
      (Filename.check_suffix pathname ".txt") or
      (Filename.check_suffix pathname "README")
    | "PostScript" ->
      (Filename.check_suffix pathname ".ps")
    | "DVI" ->
      (Filename.check_suffix pathname ".dvi")
    | "PDF" ->
      (Filename.check_suffix pathname ".pdf")
    | _ ->
      false
      
(*
    let command_line =
      Printf.sprintf
        "file '%s' | grep '%s' &> /dev/null"
        pathname
        file_type in
    match Unix.system command_line with
      Unix.WEXITED 0 -> true
    | _ -> false
*)
    
  (** Import the given file, copying it into the appropriate directory with a fresh name;
      return the fresh name (just the file name, not a complete pathname) and the name
      of an application suitable to read it, as a pair. In case of failure show an error
      message and raise an exception. If ~move is true then the file is moved instead of
      copied. *)
  method private import_file ?(move=false) pathname =
    try
      let reader =
        if not (self#does_file_exists pathname) then
          failwith ("The file \"" ^ (Filename.basename pathname) ^ "\" does not exist or is not readable")
(*        else if self#has_file_type pathname "PDF" or
                self#has_file_type pathname "PostScript" or
                self#has_file_type pathname "DVI" then
          "evince" *)
        else if self#has_file_type pathname "PDF" then
          "kpdf"
        else if self#has_file_type pathname "PostScript" then
          "kghostview"
        else if self#has_file_type pathname "DVI" then
          "kdvi"
        else if (self#has_file_type pathname "HTML") or
                (self#has_file_type pathname "XML") then (* 'file' may recognize (X)HTML as XML... *)
          "konqueror"
        else if self#has_file_type pathname "text" then
          "kedit"
        else
          failwith ("The file \"" ^ (Filename.basename pathname) ^ "\" has an unsupported format") in
      let fresh_pathname =
        Unix.temp_file ~parent:self#get_working_directory ~prefix:"document-" () in
      let fresh_name = 
        Filename.basename fresh_pathname in
      let command_line =
        if move then
          Printf.sprintf
            "mv '%s' '%s' &> /dev/null && chmod a-w '%s'" pathname fresh_pathname fresh_pathname
        else
          Printf.sprintf
            "cp -a '%s' '%s' &> /dev/null && chmod a-w '%s'" pathname fresh_pathname fresh_pathname in
      (match Unix.system command_line with
        Unix.WEXITED 0 ->
          fresh_name, reader
      | _ -> begin
          (* Copying failed: remove any partial copy which may have been created: *)
          let rm_command_line =
            Printf.sprintf "rm -f '%s' &> /dev/null" fresh_pathname in
          ignore (Unix.system rm_command_line);
          failwith ("Copying or moving \"" ^ pathname ^ "\" into \""^ fresh_pathname ^"\"failed")
        end)
    with (Failure title) as e -> begin
      Simple_dialogs.error title error_message ();
      raise e (* Re-raise *)
    end

  method import_report ~machine_or_router_name ~pathname () =
    let title = "Rapport sur " ^ machine_or_router_name in
    let row_id = self#import_document ~move:true pathname in
    self#set_row_item row_id "Title" (String title);
    self#set_row_item row_id "Author" (String "-");
    self#set_row_item row_id "Type" (String "Rapport");
    self#set_row_item row_id "Comment" (String ("généré le " ^ (Timestamp.current_timestamp_as_string ())));

  method import_history ~machine_or_router_name ~pathname () =
    let title = "Historique de " ^ machine_or_router_name in
    let row_id = self#import_document ~move:true pathname in
    self#set_row_item row_id "Title" (String title);
    self#set_row_item row_id "Author" (String "-");
    self#set_row_item row_id "Type" (String "Historique");
    self#set_row_item row_id "Comment" (String ("généré le " ^ (Timestamp.current_timestamp_as_string ())));

  method import_document ?(move=false) user_path_name =
    let internal_file_name, reader = self#import_file user_path_name in
    let row_id =
      self#add_row
        [ "FileName", String internal_file_name;
          "Reader", String reader ] in
    self#save;
    row_id

  initializer
    let _ =
      self#add_icon_column
        ~shown_header:"Icone"
        ~header:"Icon"
        ~strings_and_pixbufs:[ "text", marionnet_home_images^"treeview-icons/text.xpm"; ]
        ~default:(fun () -> Icon "text")
        () in
    let _ =
      self#add_editable_string_column
        ~shown_header:"Titre"
        ~header:"Title"
        ~italic:true
        ~default:(fun () -> String "Please edit this")
        () in
    let _ =
      self#add_editable_string_column
        ~shown_header:"Auteur"
        ~header:"Author"
        ~italic:false
        ~default:(fun () -> String "Please edit this")
        () in
    let _ =
      self#add_editable_string_column
        ~shown_header:"Type"
        ~header:"Type"
        ~italic:false
        ~default:(fun () -> String "Please edit this")
        () in
    let _ =
      self#add_editable_string_column
        ~shown_header:"Commentaire"
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
        ~header:"Reader"
        ~hidden:true
        () in
    (* Make internal data structures: no more columns can be added now: *)
    self#create_store_and_view;

    (* Setup the contextual menu: *)
    self#set_contextual_menu_title "Texts operations";
    let get = raise_when_none in (* just a convenient alias *)
    self#add_menu_item
      "Importer un document"
      (fun _ -> true)
      (fun _ ->
        ignore (self#import_document self#ask_file));

    self#add_menu_item
      "Afficher ce document"
      is_some
      (fun selected_rowid_if_any ->
        let row_id = get selected_rowid_if_any in
        self#display row_id);
    self#set_double_click_on_row_callback (fun row_id -> self#display row_id);

    self#add_menu_item
      "Supprimer ce document"
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
