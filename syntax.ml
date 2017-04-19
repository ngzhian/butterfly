(* Abstract syntax for butterfly *)

type op =
  Op of string

(* Pure types *)
type ty =
    TUnit                     (* Unit *)
  | TBool                     (* Boolean *)
  | TArrow of ty * dirty      (* Function abstraction, argument type and return type *)
  | TEffect of string * ty * ty        (* Effect type, name, argument type and return type *)
                              (* TODO should make this algebraic *)
  | THandler of dirty * dirty (* Handler type, takes a dirty type to another dirty type *)

(* type _ ty = *)
(*   | GTBool : bool -> bool ty *)
(*   | GTarrow : ('a ty -> dirty) ty *)
(*   | GEffect : ('a ty -> 'a ty) ty *)
(*   | GTHandler : (dirty -> dirty) ty *)

(* dirt is the list of effects that may be called during evaluation *)
and dirt = op list

(* Dirty types are pure types with dirt, dirt is a list of effect types *)
and dirty =
      ty * dirt

(* Names used in expressions *)
type name = string

(* Expressions are pure, evaluating them will not result in side effects *)
type expr =
  | Bool of bool                         (* Boolean constant *)
  | Fun of name * name * ty * ty * expr  (* Function abstraction, fun f (x : bool) : bool -> true *)
  | Effect of name * ty * ty             (* Effect definition, effect choice : unit -> bool *)
  | Var of name                          (* Variable *)
  | Handler of clause list               (* Handler, with clauses, handler | #op _ k -> true *)

and clause =                             (* clauses in a handler *)
  | PVal of name                         (* When a value is returned *)
  | PEffect of name * name * name * comp (* | #op y k -> c, y is the argument #op is called with *)

and comp =
  | Val of expr           (* Expression *)
  | Op of name * expr     (* Calling an operation, #op () *)
  | Handle of comp * expr (* Handling a computation, handle (#op ()) with handler *)
  | App of expr * comp    (* Function application *)
  | Let of name * comp    (* Let expression *)

let rec string_of_expr (e : expr) : string =
  match e with
  | Bool b -> string_of_bool b
  | Fun (f,arg,tya,tyf,body) -> "fun " ^ f ^ "(" ^ arg ^ ")" ^ " : " ^ (string_of_expr body)
  | Effect (e,ty1,ty2) -> "effect " ^ e
  | Var x -> x
  | Handler cls -> String.concat " | " (List.map string_of_clause cls)

and string_of_clause (cls: clause) : string =
  match cls with
  | PVal v -> v
  | PEffect (op, arg, k, cont) -> "#" ^ op ^ " " ^ arg ^ " " ^ k ^ "<comp>"

and string_of_comp (c: comp) : string =
  (match c with
   | Val e -> string_of_expr e
   | Op (op, arg) -> "#" ^ op ^ " " ^ string_of_expr arg
   | Handle (c,h) -> "handle " ^ (string_of_comp c) ^ " with " ^ (string_of_expr h)
   | App (e1, c2) -> (string_of_expr e1) ^ " " ^ (string_of_comp c2)
   | Let (v, c) -> "let " ^ v ^ " = " ^ (string_of_comp c))
