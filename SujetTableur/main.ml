open Cell
open Sheet
open Command
open Debug

(* Ajoute l'option -paf en ligne de commande, qui met la ref `paf` à `true` *)
let _ = Arg.parse [("-paf", Set paf , "Affiche PAF s'il y a une boucle")] (fun s -> ()) "usage : ./main.native [-paf]"

(*** début de la partie "incantatoire" ***)
(* stdin désigne l'entrée standard (le clavier) *)
(* lexbuf est un canal ouvert sur stdin *)
let lexbuf = Lexing.from_channel stdin

(* on enchaîne les tuyaux: lexbuf est passé à Lexer.token,
   et le résultat est donné à Parser.main *)

let parse () = Parser.debut Lexer.token lexbuf
(*** fin de la partie "incantatoire" ***)

let spreadsheet () =
      let result = parse () in
      begin
        (* le seul endroit a comprendre (dans un premier temps) :
           appel a la fonction run_script, qui est definie dans command.ml *)
        run_script result;
        flush stdout;
      end
;;


let _ = spreadsheet()

(* let _ = let scr = [ Upd (("C",2), Cst 3.7); ShowAll ] in
 *         begin
 *           print_string "HAHAHA!!!\n";
 *           run_script scr
 *         end *)
