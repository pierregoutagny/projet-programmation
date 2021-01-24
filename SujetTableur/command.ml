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
  | Op (_, fl) -> List.fold_right ajoute fl l
  in
  ajoute f []

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
    eval_p_debug (fun () -> "Update cell " ^ cell_name2string cn ^ "\n");

    let l = cell_list_of_formula (read_cell co).formula in
    remove_dependencies co l;

    update_cell_formula co f;

    let l = cell_list_of_formula f in
    add_dependencies co l;
    
    recompute_cell (fst co) (snd co)

(* exécuter une liste de commandes *)
let run_script cs = List.iter run_command cs
