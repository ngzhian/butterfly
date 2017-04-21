{
  open Parser
}

let var = ['a'-'z' 'A'-'Z']+

rule token = parse
    [' ' '\t' '\r' '\n'] { token lexbuf }
  | "bool"               { TBOOL }
  | "true"               { TRUE }
  | "false"               { FALSE }
  | "unit"               { TUNIT }
  | "fun"           { FUN }
  | "effect" { EFFECT }
  | "val" { VAL }
  | "handler" { HANDLER }
  | "handle" { HANDLE }
  | "with" { WITH }
  | "let"           { LET }
  | ";;"            { SEMISEMI }
  | '='             { EQUAL }
  | "->"            { TARROW }
  | ':'             { COLON }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | '#'             { OPHASH }
  | '|'             { BAR }
  | var             { VAR (Lexing.lexeme lexbuf) }
  | eof             { EOF }

{
}
