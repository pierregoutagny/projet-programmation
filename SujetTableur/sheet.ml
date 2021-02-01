(* tableau de cellules *)
open Cell

let size = (20,10) (* lignes, colonnes *)
let number_of_sheets = 4 (* Nombre de feuilles du tableur *)

(* le tableau que l'on manipule dans le programme ; *)
(* si nécessaire, tapez "fst" et "snd" dans un interprete Caml pour connaître leur type *) 
(* default_cell est défini dans cell.ml (module Cell) *)
let new_sheet () = Array.make_matrix (fst size) (snd size) default_cell
  
let thesheet = Array.make number_of_sheets (new_sheet ())

let current_sheet = ref 0

let read_cell co = thesheet.(!current_sheet).(fst co).(snd co)

let update_cell_formula co f = (read_cell co).formula <- f
let update_cell_value co v = (read_cell co).value <- v
let add_used_in_cell co1 co2 = Hashtbl.replace (read_cell co1).used_in co2 ()
let remove_used_in_cell co1 co2 = Hashtbl.remove (read_cell co1).used_in co2
let iter_used_in f co = Hashtbl.iter f (read_cell co).used_in

(* exécuter une fonction, f, sur tout le tableau *)
let sheet_iter f =
  for i = 0 to (fst size -1) do
    for j = 0 to (snd size -1) do
      f i j
    done;
  done


(* initialisation du tableau : questions un peu subtiles de partage,
 * demandez autour de vous si vous ne comprenez pas pourquoi cela est
 * nécessaire.  
 * Vous pouvez ne pas appeler la fonction ci-dessous,
 * modifier une case du tableau à l'aide de update_cell_formula, et
 * regarder ce que ça donne sur le tableau : cela devrait vous donner
 * une piste *)
let init_sheet () =
  for k = 0 to number_of_sheets - 1 do
    let a_sheet = (new_sheet ()) in
    let init_cell i j =
      let c = { value = None; formula = Cst (I 0); used_in = Hashtbl.create (fst size * snd size) } in
      a_sheet.(i).(j) <- c
    in
    sheet_iter init_cell;
    thesheet.(k) <- a_sheet
    done

    

(* on y va, on initialise *)
let _ = init_sheet ()


(* affichage rudimentaire du tableau *)

let show_sheet () =
  let g i j =
    begin
      (* aller à la ligne en fin de ligne *)
      if j = 0 then print_newline() else ();
      let c = read_cell (i,j) in
      print_string (cell_val2string c);
      print_string " "
    end
  in
  sheet_iter g;
  print_newline()




(********** calculer les valeurs à partir des formules *************)

(* on marque qu'on doit tout recalculer en remplissant le tableau de "None" *)
(* mettre i j a None*)
let invalidate_cell i j =
  update_cell_value (i, j) None
  
(*    à faire : mettre tout le monde à None *)
let invalidate_sheet () = 
  sheet_iter invalidate_cell

(*    à faire : le cœur du programme *)
let number_operation fun_float fun_int a b =
    match a, b with
    | I i, I j -> I (fun_int i j)
    | I i, F j | F j, I i -> F (fun_float (float_of_int i) j)
    | F i, F j -> F (fun_float i j)
let add_numbers = number_operation ( +. ) ( + )
let mult_numbers = number_operation ( *. ) ( * )
let max_numbers = number_operation max max
let div_numbers = number_operation ( /. ) ( / )

let rec eval_form fo = match fo with
  | Cst n -> n
  | Cell (p,q) -> eval_cell p q
  | Op(o,fs) -> eval_op o fs
  | CellRange _ -> failwith "On n'évalue jamais de CellRange"

and eval_op o fs = match o with
  | S -> List.fold_left (fun x f -> add_numbers x (eval_form f)) (I 0) fs 
  | M -> List.fold_left (fun x f -> mult_numbers x (eval_form f)) (I 1) fs
  | A -> div_numbers (eval_op S fs) (F (float_of_int (List.length fs)))
  | X -> List.fold_left (fun x f -> max_numbers x (eval_form f)) (I min_int) fs 

(* ici un "and", car eval_formula et eval_cell sont a priori 
   deux fonctions mutuellement récursives *)
and eval_cell i j =
  let c = read_cell (i, j) in
  match c.value with 
  | None -> let v = eval_form c.formula in update_cell_value (i, j) (Some v); v
  | Some n -> n


(* on recalcule le tableau, en deux étapes *)
let recompute_sheet () =
  invalidate_sheet ();
  sheet_iter eval_cell

(* on recalcule le tableau uniquement par dépendance *)
let rec recompute_cell i j =
  invalidate_cell i j;
  let _ = eval_cell i j in
  iter_used_in (fun co _ -> recompute_cell (fst co) (snd co)) (i, j)
