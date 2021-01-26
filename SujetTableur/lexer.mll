{
  open Parser;;        (* le type "token" est défini dans parser.mli *)
(* ce n'est pas à vous d'écrire ce fichier, il est engendré automatiquement *)
}

rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t' '\n']     { token lexbuf }    (* on saute les blancs et les tabulations *)
 	     	   	           (* token: appel récursif *)
                                   (* lexbuf: argument implicite
                                      associé au tampon où sont
                                      lus les caractères *)
  | eof             { EOF }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | '='             { EQUAL }
  | ';'             { SEMICOL }
  | '.'             { DOT }
  | ':'             { COLON }  (* ligne reservé pour les range *)
  | "Show" { SHOW }
  | "ShowAll" { SHOWALL }
  | "SUM" { SUM }
  | "MULT" { MULT }
  | "AVERAGE" { AVERAGE }
  | "MAX" { MAX }
  | '-'?['0'-'9']+'.'['0'-'9']* as s { NBR (float_of_string s) } 
  | '-'?['0'-'9']+ as s { INT (int_of_string s) } 
  (* Un lexème unique pour les noms de cellules *)
  | (['A'-'Z']+ as s) (['0'-'9']+ as i) { CELL (s, (int_of_string i)) }
