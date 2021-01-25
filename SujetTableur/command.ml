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
  | CellRange _ -> failwith "Pas de CellRange ici"
  in
  ajoute f []

let dependency_from co used_cell = 
  let rec parcours co' = 
    Hashtbl.fold (fun co'' _ b -> if co'' = used_cell then true else b || parcours co'') (read_cell co').used_in false 
  in
  co = used_cell || parcours co 

let remove_dependencies co = List.iter (fun co' -> remove_used_in_cell co' co)
let add_dependencies co = List.iter (fun co' -> add_used_in_cell co' co)

(* Engendre la liste des cellules entre (p1, q1) et (p2, q2). Ordre non spécifié *)
let cell_list_of_range p1 q1 p2 q2 = 
  let cell_list_of_ordered_range p1 q1 p2 q2 =
    let rec add_cell p1 q1 x p2 q2 y l = 
        match (p1 = x, q1 = y) with
        | (true, true) -> Cell(x, y)::l
        | (true, false) -> Cell(x, y)::(add_cell p1 q1 p2 p2 q2 (y-1) l)
        | (false, _) -> Cell(x, y)::(add_cell p1 q1 (x-1) p2 q2 y l) 
    in
    add_cell p1 q1 p2 p2 q2 q2 []
  in
  match p1 <= p2, q1 <= q2 with
  | (true, true) -> cell_list_of_ordered_range p1 q1 p2 q2
  | (true, false) -> cell_list_of_ordered_range p1 q2 p2 q1
  | (false, true) -> cell_list_of_ordered_range p2 q1 p1 q2
  | (false, false) -> cell_list_of_ordered_range p2 q2 p1 q1

(* Etend les range dans une formule. UB si f est une CellRange *)
let expand_ranges f =
  let rec expand_ranges_tolist f =
    match f with
    | Op (_, []) -> [f]
    | Op (o, lf) -> [Op (o, List.flatten (List.map expand_ranges_tolist lf))]
    | CellRange ((x1, y1), (x2, y2)) -> cell_list_of_range x1 y1 x2 y2
    | _ -> [f]
  in
  List.hd (expand_ranges_tolist f)
  
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
          ^ cell_name2string cn ^ "\n"
        );
      ps (cell_val2string (read_cell co)); (* <- ici ps, et pas p_debug, car on veut afficher au moins cela *)
      print_newline()
    end
  | ShowAll ->
    begin
      eval_p_debug (fun () -> "Show All\n");
      show_sheet ()
    end
  | Upd(cn, f') ->
    let co = cellname_to_coord cn in

    let f = expand_ranges f' in
    
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
