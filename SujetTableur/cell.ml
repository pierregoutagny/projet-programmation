(* les nombres avec lesquels on calcule *)
type number = F of float | I of int
type coord = int*int
let string_of_number = function
    | F f -> string_of_float f
    | I i -> string_of_int i

let print_number = function
    | F f -> print_float f
    | I i -> print_int i

(* deux coordonnées, p.ex. ("B",7) *)
type cellname = string*int

let rowname_to_row rn =
  let fin = String.length rn in
  let rec convert debut n =
    if debut = fin then n
    else convert (debut+1) (26*n + (int_of_char rn.[debut])- 64)
  in
  convert 0 0

let row_to_rowname r = 
  let rec convert r s =
    let d = r / 26 in
    let m = r mod 26 in
    if d=0 then String.make 1 (char_of_int (r+65)) ^ s else convert (d-1) (String.make 1 (char_of_int (m+65)) ^ s)
  in
  convert r ""

(* les deux fonctions ci-dessous sont a reprendre, un jour ou l'autre :
 * elles ne marchent que pour des noms de colonnes ne comportant qu'un
 * caractère *)
let cellname_to_coord cn =
  let column = rowname_to_row (fst cn) in
    (snd cn -1, column - 1)
let coord_to_cellname co =
  let column_nbr = snd co in
  if column_nbr > 25 then
    failwith "coord_to_cellname : cela ne devrait pas se produire"
  else
    (String.make 1 (char_of_int (column_nbr + 65)), fst co +1)


(* operations que l'on peut utiliser dans les formules *)
type oper = S | M | A | X(* sum, multiply, average, max *)

(* formules : une valeur, la même valeur qu'une autre cellule, une opération et
 * ses arguments *)
type form = Cst of number | Cell of coord | CellRange of coord * coord | Op of oper * form list 

(* cellules *)
(* un type enregistrement
 * "mutable" signifie que l'on pourra modifier le champ
 * pour info, on a  type 'a option = None | Some of 'a (ici, 'a c'est number) 
 * cell est un enregistrement avec deux champs, un champ formula de type form,
 * et un champ value contenant soit Some f (avec f un float), soit None *)
type cell = { mutable formula : form; mutable value : number option; used_in : (coord, unit) Hashtbl.t }

(* cellule par défait : pas de valeur, et la formule correspondante est la constante I 0 *)
let default_cell = { formula = Cst (I 0); value = None; used_in = Hashtbl.create 0 }



(************ affichage **************)
let cell_name2string cn = (fst cn)^(string_of_int (snd cn))

let cell_val2string c = match c.value with
  | None -> "_"
  | Some n -> string_of_number n

let oper2string = function
  | S -> "SUM"
  | M -> "MULT"
  | A -> "AVERAGE"
  | X -> "MAX"

let ps = print_string

let rec list2string f = function
  | [x] -> f x
  | x::xs ->
    begin
      f x ^ ";" ^ list2string f xs
    end
  | _ -> failwith "show_list: the list shouldn't be empty"

let rec show_list f = function
  | [x] -> f x
  | x::xs ->
    begin
      f x;
      ps";";
      show_list f xs
    end
  | _ -> failwith "show_list: the list shouldn't be empty"

(* convertir une formule en une chaîne de caractères *)
let rec form2string = function
  | Cell c -> cell_name2string (coord_to_cellname c)
  | CellRange(co1,co2) -> form2string (Cell co1) ^ ":" ^ form2string (Cell co2)
  | Cst n -> string_of_number n
  | Op(o,fl) ->
    begin
      (oper2string o) ^ "(" ^ list2string form2string fl ^ ")"
    end

let rec show_form f = ps (form2string f)
