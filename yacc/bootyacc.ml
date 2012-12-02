(* Parser generated by Mike's special yacc *)

open Yyparse;;

type token =
  | EOF
  | SYMBOL of (Grammar.symbol)
  | TAG of (string)
  | NUMBER of (string)
  | SEMACT of (Lexing.position * string)
  | QUOTE
  | TOKEN
  | LEFT
  | RIGHT
  | NONASSOC
  | START
  | TYPE
  | PREC
  | PPERCENT
  | COLON
  | SEMI
  | VBAR
  | DOT
  | BADTOK
  | UNREACHABLE


# 44 "yacc.mly"

open Print
open Grammar

let prec = ref 0
let index = ref 0
let rhsmap = ref []

let dcl_type tag pos sym =
  if sym.x_type = "" then
    sym.x_type <- tag
  else if sym.x_type <> tag then
    Error.syntax pos 
      "'$' is declared with multiple types" [fSym sym]

let maybe_dcl_type tag pos sym =
  match tag with
      Some t -> dcl_type t pos sym
    | None -> ()

let dcl_token assoc tag pos sym =
  begin match assoc with
      Token ->
        sym.x_genuine <- true;
        if sym.x_kind = Nonterm then sym.x_kind <- Token
    | Left | Right | Nonassoc ->
        if sym.x_kind = Nonterm || sym.x_kind = Token then begin
          sym.x_kind <- assoc; sym.x_prec <- !prec
        end else
          Error.syntax pos
            "'$' is declared with multiple precedences" [fSym sym]
    | _ ->
        failwith "dcl_token"
  end;
  maybe_dcl_type tag pos sym

let dcl_start tag pos sym =
  if List.exists (fun x -> same_syms x sym) !start_syms then
    Error.syntax pos 
      "$ is declared as a start symbol more than once" [fSym sym];
  start_syms := !start_syms @ [sym];
  maybe_dcl_type tag pos sym

let pr_sym = ref None

let gencount = ref 0

let gensym () = 
  incr gencount; lookup (sprintf "yyg$" [fNum !gencount])

let fLeft n fmt =
  let f prf =
    let s0 = sprintf "$" [fmt] in
    let n0 = String.length s0 in
    prf "$" [fStr s0];
    for i = n0+1 to n  do prf "$" [fChr ' '] done in
  fExt f

let dollar_num = Str.regexp "\\$\\([0-9]+\\)"

let subst rhs pos s =
  let i = int_of_string (Str.matched_group 1 s) in
  if i > List.length rhs then
    Error.syntax pos
      "$$ is beyond the end of the rule" [fChr '$'; fNum i]
  else begin
    let x = List.nth rhs (i-1) in
    if not (has_value x) then 
      Error.syntax pos
	"$$ refers to $, which has no value" [fChr '$'; fNum i; fSym x]
  end;
  sprintf "_$" [fNum i]

let dollar_name = 
  Str.regexp "\\$\\([A-Za-z_][A-Za-z0-9_]*\\(\\.[0-9]+\\)?\\)"

let subst2 rhs pos s =
  let t = Str.matched_group 1 s in
  match List.filter (fun (x, i) -> x = t) !rhsmap with
      [] -> Error.syntax pos "$$ is not defined" [fChr '$'; fStr t]; ""
    | [(_, i)] -> 
	let x = List.nth rhs (i-1) in
	if not (has_value x) then
	  Error.syntax pos "$$ has no value" [fChr '$'; fStr t];
	sprintf "_$" [fLeft (String.length t) (fNum i)]
    | _ -> Error.syntax pos "$$ is ambiguous" [fChr '$'; fStr t]; ""

let make_action rhs (pos, text) =
  let text' = Str.global_substitute dollar_num (subst rhs pos) text in
  let text'' = Str.global_substitute dollar_name (subst2 rhs pos) text' in
  (pos, text'')

# 122 "yacc.ml"
let yyconst = [|
  0; (* EOF *)
  6; (* QUOTE *)
  7; (* TOKEN *)
  8; (* LEFT *)
  9; (* RIGHT *)
  10; (* NONASSOC *)
  11; (* START *)
  12; (* TYPE *)
  13; (* PREC *)
  14; (* PPERCENT *)
  15; (* COLON *)
  16; (* SEMI *)
  17; (* VBAR *)
  18; (* DOT *)
  19; (* BADTOK *)
  20; (* UNREACHABLE *)
|]

let yyblock = [|
  2; (* SYMBOL *)
  3; (* TAG *)
  4; (* NUMBER *)
  5; (* SEMACT *)
|]

let yylhs = [|
    -1;     1;     2;     2;     2;     2;     2;     4;     4;     4;
     4;     5;     5;     6;     6;     3;     3;     7;     8;     8;
     9;    10;    11;    11;    11;    11;    12;    12;     0;
|]

let yyrlen = [|
     2;     3;     0;     4;     4;     4;     2;     1;     1;     1;
     1;     0;     1;     0;     2;     0;     2;     4;     1;     3;
     3;     0;     0;     2;     3;     2;     1;     3;     2;
|]

let yyaction = [|
    41;     0;     0;     0;    38;     0;     0;     0;     0;     0;
    18;    30;     0;    18;     0;    29;    29;    51;    29;    29;
     0;     0;    19;     0;     0;     0;     0;     7;   -15;     0;
     0;     0;     4;     0;    -2;     2;    20;     0;    10;     0;
     0;
|]

let yydefact = [|
     0;     0;    -2;   -28;     0;    -6;    -7;    -8;    -9;   -10;
     0;     0;   -15;     0;   -12;     0;     0;     0;     0;     0;
    -4;    -5;     0;   -16;    -3;   -14;   -21;     0;     0;   -22;
   -17;   -21;     0;   -19;     0;     0;     0;   -23;     0;   -24;
   -27;
|]

let yygoto = [|
     0;     0;     0;     0;     0;    -5;    -6;     0;   -26;     0;
     0;     0;     0;
|]

let yydefgoto = [|
     1;     3;     4;    17;    13;    15;    20;    23;    27;    28;
    29;    32;    37;
|]

let yytabsize = 54

let yytable = [|
   -26;   -18;    31;   -26;   -25;    33;    34;   -25;    18;    35;
    21;   -26;    24;    25;    40;   -25;    38;    36;   -20;   -20;
   -11;    14;    39;    30;   -11;   -11;   -11;   -11;   -11;   -11;
   -11;    19;   -11;    16;    26;   -13;   -13;   -13;   -13;   -13;
   -13;   -13;     2;   -13;     5;     6;     7;     8;     9;    10;
    11;    -1;    12;    22;
|]

let yycheck = [|
     2;    16;    17;     5;     2;    31;     2;     5;    13;     5;
    16;    13;    18;    19;     4;    13;    18;    13;    16;    17;
     2;     3;     2;    16;     6;     7;     8;     9;    10;    11;
    12;     2;    14;     3;    15;     6;     7;     8;     9;    10;
    11;    12;     1;    14;     6;     7;     8;     9;    10;    11;
    12;     0;    14;     2;
|]

let yyname = [|
"EOF"; "*1*"; "SYMBOL"; "TAG"; "NUMBER"; "SEMACT"; "QUOTE"; 
"TOKEN"; "LEFT"; "RIGHT"; "NONASSOC"; "START"; "TYPE"; "PREC"; 
"PPERCENT"; "COLON"; "SEMI"; "VBAR"; "DOT"; "BADTOK"; "UNREACHABLE"; 
|]

let yyrule = [|
"*start* --> *entry* EOF";
"grammar --> heading PPERCENT body";
"heading --> /* empty */";
"heading --> heading assoc tag symbols";
"heading --> heading START tag symbols";
"heading --> heading TYPE TAG symbols";
"heading --> heading QUOTE";
"assoc --> TOKEN";
"assoc --> LEFT";
"assoc --> RIGHT";
"assoc --> NONASSOC";
"tag --> /* empty */";
"tag --> TAG";
"symbols --> /* empty */";
"symbols --> SYMBOL symbols";
"body --> /* empty */";
"body --> body para";
"para --> SYMBOL COLON prods SEMI";
"prods --> prod";
"prods --> prod VBAR prods";
"prod --> init rhs SEMACT";
"init --> /* empty */";
"rhs --> /* empty */";
"rhs --> rhs symbol";
"rhs --> rhs PREC SYMBOL";
"rhs --> rhs SEMACT";
"symbol --> SYMBOL";
"symbol --> SYMBOL DOT NUMBER";
"*entry* --> *1* grammar";
|]

let yysemact = [|
(fun () -> failwith "parser");
(fun () ->
  let _1 = (yypeek 1 : 't_heading) in
  let _3 = (yypeek 3 : 't_body) in
  Obj.repr (
# 140 "yacc.mly"
                                ( () )
# 253 "yacc.ml"
		: unit));
(fun () ->
  Obj.repr (
# 143 "yacc.mly"
                                ( () )
# 259 "yacc.ml"
		: 't_heading));
(fun () ->
  let _1 = (yypeek 1 : 't_heading) in
  let _2 = (yypeek 2 : 't_assoc) in
  let _3 = (yypeek 3 : 't_tag) in
  let _4 = (yypeek 4 : 't_symbols) in
  Obj.repr (
# 145 "yacc.mly"
      ( List.iter (dcl_token _2     _3   (rhs_start_pos 2)) _4       )
# 269 "yacc.ml"
		: 't_heading));
(fun () ->
  let _1 = (yypeek 1 : 't_heading) in
  let _3 = (yypeek 3 : 't_tag) in
  let _4 = (yypeek 4 : 't_symbols) in
  Obj.repr (
# 147 "yacc.mly"
      ( List.iter (dcl_start _3   (rhs_start_pos 2)) _4       )
# 278 "yacc.ml"
		: 't_heading));
(fun () ->
  let _1 = (yypeek 1 : 't_heading) in
  let _3 = (yypeek 3 : string) in
  let _4 = (yypeek 4 : 't_symbols) in
  Obj.repr (
# 149 "yacc.mly"
      ( List.iter (dcl_type _3   (rhs_start_pos 2)) _4       )
# 287 "yacc.ml"
		: 't_heading));
(fun () ->
  let _1 = (yypeek 1 : 't_heading) in
  Obj.repr (
# 150 "yacc.mly"
                                ( () )
# 294 "yacc.ml"
		: 't_heading));
(fun () ->
  Obj.repr (
# 153 "yacc.mly"
                                ( Token )
# 300 "yacc.ml"
		: 't_assoc));
(fun () ->
  Obj.repr (
# 154 "yacc.mly"
                                ( incr prec; Left )
# 306 "yacc.ml"
		: 't_assoc));
(fun () ->
  Obj.repr (
# 155 "yacc.mly"
                                ( incr prec; Right )
# 312 "yacc.ml"
		: 't_assoc));
(fun () ->
  Obj.repr (
# 156 "yacc.mly"
                                ( incr prec; Nonassoc )
# 318 "yacc.ml"
		: 't_assoc));
(fun () ->
  Obj.repr (
# 159 "yacc.mly"
                                ( None )
# 324 "yacc.ml"
		: 't_tag));
(fun () ->
  let _1 = (yypeek 1 : string) in
  Obj.repr (
# 160 "yacc.mly"
                                ( Some _1   )
# 331 "yacc.ml"
		: 't_tag));
(fun () ->
  Obj.repr (
# 163 "yacc.mly"
                                ( [] )
# 337 "yacc.ml"
		: 't_symbols));
(fun () ->
  let _1 = (yypeek 1 : Grammar.symbol) in
  let _2 = (yypeek 2 : 't_symbols) in
  Obj.repr (
# 164 "yacc.mly"
                                ( _1     ::_2       )
# 345 "yacc.ml"
		: 't_symbols));
(fun () ->
  Obj.repr (
# 167 "yacc.mly"
                                ( () )
# 351 "yacc.ml"
		: 't_body));
(fun () ->
  let _1 = (yypeek 1 : 't_body) in
  let _2 = (yypeek 2 : 't_para) in
  Obj.repr (
# 168 "yacc.mly"
                                ( () )
# 359 "yacc.ml"
		: 't_body));
(fun () ->
  let _1 = (yypeek 1 : Grammar.symbol) in
  let _3 = (yypeek 3 : 't_prods) in
  Obj.repr (
# 172 "yacc.mly"
      ( if not (is_nonterm _1     ) then
	  Error.syntax (rhs_start_pos 1) "left-hand side is a token" [];
	if !Grammar.start_syms = [] then
	  Grammar.start_syms := [_1     ];
	List.iter (fun (line, rhs, prec, semact) ->
	  ignore (make_rule _1      rhs prec semact line)) _3     )
# 372 "yacc.ml"
		: 't_para));
(fun () ->
  let _1 = (yypeek 1 : 't_prod) in
  Obj.repr (
# 180 "yacc.mly"
                                ( [_1] )
# 379 "yacc.ml"
		: 't_prods));
(fun () ->
  let _1 = (yypeek 1 : 't_prod) in
  let _3 = (yypeek 3 : 't_prods) in
  Obj.repr (
# 181 "yacc.mly"
                                ( _1::_3 )
# 387 "yacc.ml"
		: 't_prods));
(fun () ->
  let _1 = (yypeek 1 : 't_init) in
  let _2 = (yypeek 2 : 't_rhs) in
  let _3 = (yypeek 3 : Lexing.position * string) in
  Obj.repr (
# 185 "yacc.mly"
      ( let a = make_action _2   _3      in
	(rhs_start_pos 2, _2  , !pr_sym, a) )
# 397 "yacc.ml"
		: 't_prod));
(fun () ->
  Obj.repr (
# 189 "yacc.mly"
                                ( pr_sym := None )
# 403 "yacc.ml"
		: 't_init));
(fun () ->
  Obj.repr (
# 193 "yacc.mly"
      ( index := 0; rhsmap := []; [] )
# 409 "yacc.ml"
		: 't_rhs));
(fun () ->
  let _1 = (yypeek 1 : 't_rhs) in
  let _2 = (yypeek 2 : 't_symbol) in
  Obj.repr (
# 195 "yacc.mly"
      ( let (x, y) = _2      in
	if is_token x then begin
	  pr_sym := Some x;
          if not (same_syms x error_sym) then x.x_genuine <- true 
        end;
	incr index; rhsmap := !rhsmap @ [(y, !index)];
        _1   @ [x] )
# 423 "yacc.ml"
		: 't_rhs));
(fun () ->
  let _1 = (yypeek 1 : 't_rhs) in
  let _3 = (yypeek 3 : Grammar.symbol) in
  Obj.repr (
# 203 "yacc.mly"
      ( let x = _3      in
        if not (is_token x) then 
          Error.syntax (rhs_start_pos 2)
            "%prec must be followed by a token" [];
        if x.x_kind = Token then 
          Error.syntax (rhs_start_pos 2)
            "%prec must be followed by a token with defined precedence" [];
        pr_sym := Some x;
        _1   )
# 439 "yacc.ml"
		: 't_rhs));
(fun () ->
  let _1 = (yypeek 1 : 't_rhs) in
  let _2 = (yypeek 2 : Lexing.position * string) in
  Obj.repr (
# 213 "yacc.mly"
      ( let g = gensym () in
	let a = make_action _1   _2      in
        let r = make_rule g [] None a (fst a) in
	incr index;
	r.r_context <- _1  ; 
	_1   @ [g] )
# 452 "yacc.ml"
		: 't_rhs));
(fun () ->
  let _1 = (yypeek 1 : Grammar.symbol) in
  Obj.repr (
# 221 "yacc.mly"
               ( (_1     , _1     .x_name) )
# 459 "yacc.ml"
		: 't_symbol));
(fun () ->
  let _1 = (yypeek 1 : Grammar.symbol) in
  let _3 = (yypeek 3 : string) in
  Obj.repr (
# 222 "yacc.mly"
                         ( (_1     , _1     .x_name ^ "." ^ _3     ) )
# 467 "yacc.ml"
		: 't_symbol));
(* Entry grammar *)
(fun () -> raise (YYexit (yypeek 2)));
|]

let yytables =
  { yysemact = yysemact;
    yyconst = yyconst;
    yyblock = yyblock;
    yylhs = yylhs;
    yyrlen = yyrlen;
    yyaction = yyaction;
	   yydefact = yydefact;
    yygoto = yygoto;
    yydefgoto = yydefgoto;
    yytabsize = yytabsize;
    yytable = yytable;
    yycheck = yycheck;
    yyerror = parse_error;
    yyname = yyname;
    yyrule = yyrule }

let grammar (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
  (yyparse yytables 1 lexfun lexbuf : unit)


