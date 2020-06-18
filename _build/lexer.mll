{
open Parser
exception Eof
}

rule token = parse
    [' ' '\t' '\n']           {token lexbuf}
  | "."                       {EOL}
  | ","                       {AND}
  | "("                       {LPAR}
  | ")"                       {RPAR}
  | "["                       {LBRAC}
  | "]"                       {RBRAC}
  | "\""                      {QUO}
  | ":-"                      {IMP}
  | ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as var          {VAR(var)}
  | ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as nvar         {NVAR(nvar)}
  | eof                        {EOF}