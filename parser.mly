%{
  open Syntax
%}

(* %token TINT *)
%token TBOOL
%token TUNIT
%token TARROW
%token <Syntax.name> VAR
(* %token <int> INT *)
%token TRUE FALSE
%token EFFECT VAL
%token OPHASH
%token HANDLE WITH
%token HANDLER
%token BAR
(* %token PLUS *)
(* %token MINUS *)
(* %token TIMES *)
%token EQUAL
  (* LESS *)
(* %token IF THEN ELSE *)
%token FUN
%token COLON
%token LPAREN RPAREN
%token LET
%token SEMISEMI
%token EOF

%start file
%type <Syntax.toplevel list> file

(* %nonassoc IS *)
(* %nonassoc ELSE *)
(* %nonassoc EQUAL LESS *)
(* %left PLUS MINUS *)
(* %left TIMES *)
(* %right TARROW *)

%%

file:
  | EOF
    { [] }
  | e = expr EOF
    { [Expr e] }
  | e = comp EOF
    { [Comp e] }
  | e = expr SEMISEMI lst = file
    { Expr e :: lst }
  | e = comp SEMISEMI lst = file
    { Comp e :: lst }

expr: plain_expr { $1 }
plain_expr:
  | LPAREN RPAREN
    { Unit }
  | e = plain_app_expr
    { e }
  | FUN f = VAR LPAREN x = VAR COLON ty1 = ty RPAREN c = comp
    { Fun (f, x, ty1, c) }
  | HANDLER BAR p = pattern
    { Handler (p, []) }

pattern:
  | VAL v = VAR COLON ty1 = ty TARROW c = comp
    { PVal (v, ty1, c) }

effect:
  | EFFECT e = VAR COLON ty1 = ty TARROW ty2 = ty
    { Effect (e, ty1, ty2) }

app_expr: plain_app_expr { $1 }
plain_app_expr:
  | OPHASH op = VAR
    { Var op }
  | e = simple_expr
    { e }
  | e = effect
    { e }

simple_expr: plain_simple_expr { $1 }
plain_simple_expr:
  | x = VAR
    { Var x }
  | TRUE    
    { Bool true }
  | FALSE
    { Bool false }
  | LPAREN e = plain_expr RPAREN
    { e }

comp: plain_comp { $1 }
plain_comp:
  | VAL e = expr
    { Val e }
  | e1 = app_expr e2 = expr
    { App (e1, e2) }
  | HANDLE c = comp WITH e = expr
    { Handle (c, e) }
  | LET v = VAR  EQUAL c = comp
    { Let (v, c) }

ty:
  | TBOOL
    { TBool }
  | TUNIT
    { TUnit }
  (* | TINT *)
  (*   { TInt } *)
  | ty1 = ty TARROW ty2 = ty
    { TArrow (ty1, (ty2, [])) }
  | LPAREN t = ty RPAREN
    { t }

%%
