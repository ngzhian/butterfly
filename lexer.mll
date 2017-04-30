{
  open Lexing
  open Parser
}

let var = ['a'-'z' 'A'-'Z']+

rule token = parse
    [' ' '\t' '\r' '\n'] { token lexbuf }
  | ['0'-'9']+           { INT (int_of_string(lexeme lexbuf)) }
  | "bool"               { TBOOL }
  | "int"                { TINT }
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
  | '+'             { PLUS }
  | var             { VAR (Lexing.lexeme lexbuf) }
  | eof             { EOF }

{
}
