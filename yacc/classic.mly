/* Classic expression grammar */

%type<tree> expr

%start expr

%token<string> ID
%token LPAR RPAR PLUS TIMES

%%

expr :
    term { $1 }
  | expr PLUS term { Plus ($1, $3) } ;

term :
    factor { $1 }
  | term TIMES factor { Times ($1, $3) } ;

factor :
    ID { Var $1 }
  | LPAR expr RPAR { $2 } ;
