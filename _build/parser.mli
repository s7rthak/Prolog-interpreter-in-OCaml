type token =
  | LPAR
  | RPAR
  | EOL
  | IMP
  | AND
  | EOF
  | LBRAC
  | RBRAC
  | QUO
  | VAR of (string)
  | NVAR of (string)

val repl :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Prolog.goal
val database :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Prolog.program
val file :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string
