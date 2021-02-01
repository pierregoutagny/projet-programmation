%{
(* --- préambule: ici du code Caml --- *)

open Cell
open Command

%}
/* énumération des lexèmes, ceux-ci sont décrits (par vous) dans lexer.mll */

%token <int> INT       /* le lexème INT a un attribut entier */
%token <float> NBR       /* le lexème NBR a un attribut flottant */
%token <string * int> CELL       /* le lexème CELLROW a un attribut, de type string */
%token <int> SHEET
%token LPAREN RPAREN EQUAL SEMICOL DOT
%token SUM MULT AVERAGE SHOW SHOWALL
%token MAX COLON SWITCHTO
%token EOF 

  /*
%start singlecomm
%type <Command.comm> singlecomm
    */
      
%start debut
%type <Command.comm list> debut

%%
debut:
   | clist EOF { $1 }
  ;
  
clist:
   | singlecomm clist { $1::$2 }
   | singlecomm                { [$1] }
  ;
  
  singlecomm:
   | cell EQUAL formula { Upd($1,$3) }
   | SHOW cell { Show($2) }
   | SHOWALL { ShowAll }
   | SWITCHTO sheet { SwitchTo $2 }
  ;

  sheet:
    | SHEET { $1 }
   ;

  cell:
   | CELL { $1 }
  ;

  operand:
   | SUM { S }
   | MULT { M }
   | AVERAGE { A }
   | MAX { X }
  ;
  
  formula:
   | NBR { Cst (F $1) } 
   | INT { Cst (I $1) } 
   | cell { Cell (Cell.cellname_to_coord $1) }
   | operand LPAREN forlist RPAREN { Op($1,$3) }
  ;

  forlist:
   | formula { [$1] }
   | formula SEMICOL forlist { $1::$3 }
   | cell COLON cell { [ CellRange (Cell.cellname_to_coord $1, Cell.cellname_to_coord $3) ] }
   | cell COLON cell SEMICOL forlist { CellRange (Cell.cellname_to_coord $1, Cell.cellname_to_coord $3) :: $5 }
  ;
  

       
