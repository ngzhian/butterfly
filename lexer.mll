{
  open Parser
}

let var = ['a'-'z' 'A'-'Z']+

rule token = parse
    [' ' '\t' '\r' '\n'] { token lexbuf }
  (* | ['0'-'9']+           { INT (int_of_string(Lexing.lexeme lexbuf)) } *)
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
  (* | "if"            { IF } *)
  (* | "then"          { THEN } *)
  (* | "else"          { ELSE } *)
  | "let"           { LET }
  | ";;"            { SEMISEMI }
  | '='             { EQUAL }
  (* | '<'             { LESS } *)
  | "->"            { TARROW }
  | ':'             { COLON }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | '#'             { OPHASH }
  | '|'             { BAR }
  (* | '+'             { PLUS } *)
  (* | '-'             { MINUS } *)
  (* | '*'             { TIMES } *)
  | var             { VAR (Lexing.lexeme lexbuf) }
  | eof             { EOF }

{
}
