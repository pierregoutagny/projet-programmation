open Debug
open Cell
open Sheet

(* commandes: ce que l'utilisateur peut saisir dans un fichier.
   - La modification d'une cellule avec une nouvelle formule,
   - l'affichage d'une cellule, 
   - l'affichage de toute la feuille *)
type comm = Upd of cellname * form | Show of cellname | ShowAll

(************ utilitaire ************)

let cell_list_of_formula f = 
  let rec ajoute f l = match f with
  | Cst _ -> l
  | Cell co -> co::l
  | CellRange(p1, q1, p2, q2) -> range_creation p1 q1 p2 q2 l (fun z -> z)
  | Op (_, fl) -> List.fold_right ajoute fl l
  in
  ajoute f []

let dependency_from co used_cell = 
  let rec parcours co' = 
    Hashtbl.fold (fun co'' _ b -> if co'' = used_cell then true else b || parcours co'') (read_cell co').used_in false 
  in
  co = used_cell || parcours co 

let remove_dependencies co = List.iter (fun co' -> remove_used_in_cell co' co)
let add_dependencies co = List.iter (fun co' -> add_used_in_cell co' co)

  
(************ affichage **************)
let show_comm c =
  match c with
  | Upd (c,f) ->
    begin
      ps (cell_name2string c);
      ps"=";
      show_form f
    end
  | Show c ->
    begin
      ps "Show(";
      ps (cell_name2string c);
      ps ")"
    end
  | ShowAll -> ps "ShowAll"

(************ faire tourner les commandes **************)

(* exécuter une commande *)
let run_command c = match c with
  | Show cn ->
    begin
      let co = cellname_to_coord cn in
      eval_p_debug (fun () ->
          "Showing cell "
          ^ cell_name2string cn
        );
      ps (cell_val2string (read_cell co)); (* <- ici ps, et pas p_debug, car on veut afficher au moins cela *)
      print_newline()
    end
  | ShowAll ->
    begin
      eval_p_debug (fun () -> "Show All\n");
      show_sheet ()
    end
  | Upd(cn,f) ->
    let co = cellname_to_coord cn in
    
    let f_list = cell_list_of_formula f in
    
    if List.exists (dependency_from co) f_list then paf_debug () else
    begin
      eval_p_debug (fun () -> "Update cell " ^ cell_name2string cn ^ "\n");
      let l = cell_list_of_formula (read_cell co).formula in
      remove_dependencies co l;

      update_cell_formula co f;

      add_dependencies co f_list;

      recompute_cell (fst co) (snd co);
    end

(* exécuter une liste de commandes *)
let run_script cs = List.iter run_command cs
