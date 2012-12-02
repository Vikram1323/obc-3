(* Definitions generated by Mike's special yacc *)

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
  | AT
  | BADTOK
  | UNREACHABLE

val grammar :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> unit

