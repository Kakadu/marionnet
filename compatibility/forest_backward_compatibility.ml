(* The old type definition: *)
type 'a forest  = Empty | NonEmpty of 'a *  ('a forest) *  ('a forest)

(* From old to new format: *)
let rec forest_conversion : 'a forest -> 'a Forest.t = 
  function
  | Empty -> Forest.empty
  | NonEmpty (x,y,z) -> Forest.concat (Forest.of_tree (x,(forest_conversion y))) (forest_conversion z)

let load_from_old_file (file_name) : ('a Forest.t) =
  let m = new Oomarshal.marshaller in
  (* Loading forest in the old format: *)
  let old_forest = m#from_file (file_name) in
  (* Convert to the new format: *)
  forest_conversion (old_forest)

