(* tableau de cellules *)
open Cell

let size = (20,10) (* lignes, colonnes *)

(* le tableau que l'on manipule dans le programme ; *)
(* si nécessaire, tapez "fst" et "snd" dans un interprete Caml pour connaître leur type *) 
(* default_cell est défini dans cell.ml (module Cell) *)
let thesheet = Array.make_matrix (fst size) (snd size) default_cell

let read_cell co = thesheet.(fst co).(snd co)


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
  let init_cell i j =
    let c = { value = None; formula = Cst 0.; used_in = Hashtbl.create (fst size * snd size) } in
    thesheet.(i).(j) <- c
  in
  sheet_iter init_cell

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

let rec range_cell p1 q1 x p2 q2 y l= match (x = p1), (y = q1) with
  |(true, true) -> Cell(x, y)::l
  |(false, true) -> Cell(x, y)::(range_cell p1 q1 (x-1) p2 q2 q2 l)
  |(_, false) -> Cell(x, y)::(range_cell p1 q1 x p2 q2 (y-1) l)

let range_cell_creation p1 q1 p2 q2 l funct = 
  match p1 <= p2, q1 <= q2 with
  |(true, true) -> funct (range_cell p1 q1 p2 p2 q2 q2 [])
  |(true, false) -> funct (range_cell p1 q2 p2 p2 q1 q1 [])
  |(false, true) -> funct (range_cell p2 q1 p1 p1 q2 q2 [])
  |(false, false) -> funct (range_cell p2 q2 p1 p1 q1 q1 [])

(*    à faire : le cœur du programme *)    
let rec eval_form fo = match fo with
| Cst n -> n
| Cell (p,q) -> eval_cell p q 
| Op(o,fs) -> eval_op o fs
| CellRange(co1, co2) -> failwith "on ne doit jamais avoir a évaluer la forme d'un cell de range"

(* Il faudra faire la moyenne qui va être horrible .... *)

and eval_op o fs = match o with
| S -> List.fold_left (eval_range S (fun x f -> x +. eval_form f)) 0. fs 
| M -> List.fold_left (eval_range M (fun x f -> x *. eval_form f)) 1. fs
| A -> (eval_op S fs) /. float_of_int (List.length fs)
| X -> List.fold_left (eval_range X (fun x f -> max x (eval_form f))) neg_infinity fs 

and eval_range fonct o x f = match f with
| Cst _ -> fonct x f
| Cell _ -> fonct x f
| Op(_, _) -> fonct x f
| CellRange((p1, q1), (p2, q2)) -> (range_cell_creation p1 q1 p2 q2 [] eval_op) o x f
      
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
