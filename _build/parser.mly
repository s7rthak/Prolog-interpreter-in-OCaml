%{
  open Prolog
%}

%token LPAR RPAR EOL IMP AND EOF
%token LBRAC RBRAC QUO
%token <string> VAR
%token <string> NVAR
%start repl
%start database
%start file
%type <Prolog.goal> repl
%type <Prolog.program> database
%type <string> file
%%

repl:
      | query EOL                               {[$1]}
      | query AND repl                          {$1 :: $3}
;

database:
      | EOF                                     {[]}
      | clause database                         {$1 :: $2}
;

file: LBRAC QUO NVAR EOL NVAR QUO RBRAC EOL     {let a = $3 ^ "." ^ $5 in a}
;

query: atofor                                   {$1}
;

atofor: NVAR LPAR term_list RPAR                {Atofor($1,$3)}
;

clause:
      | head EOL                                {Fact($1)}
      | head IMP body                           {Rule($1,$3)}
;

head: atofor                                    {$1}
;

body: repl                                      {$1}
;

term_list: 
      | term                                    {[$1]}
      | term AND term_list                      {$1 :: $3}
;

term:
      | constant                                {C($1)}
      | variable                                {V($1)}
      | atofor                                  {F($1)}
;

constant: NVAR                                 {Constant($1)}
;

variable: VAR                                  {Variable($1)}
;

