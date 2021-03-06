(* mettre à true si on veut davantage d'affichage (pour debugger) *)
let blabla = false

let paf = ref false

let p_debug s = if blabla then (print_string s; flush stdout) else ()

(* Lazy debugging. This gives the compiler the opportunity to optimise away
 * debugging statements. *)
(* Ne perdez pas trop de temps a comprendre cette fonction, allez
   regarder son utilisation (dans command.ml) lorsque vous en aurez
   besoin. *)
let eval_p_debug f =
  if blabla 
  then (print_string (f ()); flush stdout) else ()

let paf_debug () = if !paf then begin print_string "PAF\n"; exit 1 end else ()
