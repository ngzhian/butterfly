(* Abstract syntax for butterfly *)

(* Names used in expressions *)
type name = string

(* Name of operation used, currently each effect is it's operation *)
type op = string

(* Pure types *)
type ty =
    TUnit                       (* Unit *)
  | TBool                       (* Boolean *)
  | TArrow of ty * dirty        (* Function abstraction, argument type and return type *)
  | TEffect of string * ty * ty (* Effect type, name, argument type and return type *)
                                (* TODO should make this algebraic *)
  | THandler of dirty * dirty   (* Handler type, takes a dirty type to another dirty type *)
  | TyVar of int              (* Type parameter *)

(* dirt is the list of effects that may be called during evaluation *)
and dirt = op list

(* Dirty types are pure types with dirt, dirt is a list of effect names *)
and dirty = ty * dirt

type scheme =
    Forall of (int list) * dirty

(* Expressions are pure, evaluating them will not result in side effects *)
type expr =
  | Unit
  | Bool of bool                    (* Boolean constant *)
  | Fun of name * name * ty * comp  (* Function abstraction, fun f (x : bool) -> true *)
  | Effect of name * ty * ty        (* Effect definition, effect choice : unit -> bool *)
  | Var of name                     (* Variable *)
  | Handler of clause * clause list (* Handler, with clauses, handler | #op _ k -> true *)
(* the first clause is the value clause which is always required *)

and clause =                             (* clauses in a handler *)
  | PVal of name * ty * comp             (* When a value is returned, val x : ty -> c *)
  | PEffect of name * name * name * comp (* | #op y k -> c, y is the argument #op is called with *)

and comp =
  | Val of expr           (* Expression *)
  | Handle of comp * expr (* Handling a computation, handle (#op ()) with handler *)
  | App of expr * expr    (* Function application *)
  | Let of name * comp    (* Let expression *)

(* Conversion from syntax to string *)
let rec string_of_expr (e : expr) : string =
  match e with
  | Unit -> "()"
  | Bool b -> string_of_bool b
  | Fun (f, arg, tya, body) -> "fun " ^ f ^ "(" ^ arg ^ ")" ^ " : " ^ (string_of_comp body)
  | Effect (e,ty1,ty2) -> "effect " ^ e ^ " : " ^ (string_of_tys ty1 ty2)
  | Var x -> x
  | Handler (v, cls) -> "handler | " ^ String.concat " | " (List.map string_of_clause (v :: cls))

and string_of_clause (cls: clause) : string =
  match cls with
  | PVal (v, ty, c) -> "val " ^ v ^ " : " ^ (string_of_ty ty) ^ " -> " ^ (string_of_comp c)
  | PEffect (op, arg, k, cont) -> "#" ^ op ^ " " ^ arg ^ " " ^ k ^ "<comp>"

and string_of_comp (c: comp) : string =
  (match c with
   | Val e -> string_of_expr e
   | Handle (c,h) -> "handle " ^ (string_of_comp c) ^ " with " ^ (string_of_expr h)
   | App (e1, c2) -> (string_of_expr e1) ^ " " ^ (string_of_expr c2)
   | Let (v, c) -> "let " ^ v ^ " = " ^ (string_of_comp c))

and string_of_ty (ty : ty) =
  match ty with
  | TUnit  -> "TUnit"
  | TBool  -> "TBool"
  | TArrow (ty1, ty2) -> "TArrow(" ^ (string_of_ty ty1) ^ "," ^ (string_of_dirty ty2) ^ ")"
  | TEffect (e, ty1, ty2) -> (string_of_ty ty1) ^ " -{" ^ e ^ "}> " ^ (string_of_ty ty2)
  | THandler (ty1, ty2) -> (string_of_dirty ty1) ^ "=>" ^ (string_of_dirty ty2)
  | TyVar tv -> string_of_int tv ^ "'"

and string_of_tys ty1 ty2 =
  (string_of_ty ty1) ^ "->" ^ (string_of_ty ty2)

and string_of_dirty (ty: dirty) =
  match ty with
  | (ty, dirt) -> string_of_ty ty ^
                  if List.length dirt = 0 then "" else "[" ^ (String.concat ", " dirt) ^ "]"

(* Top level used to read in files *)
type toplevel = Expr of expr | Comp of comp

let string_of_toplevel (tl: toplevel) =
  match tl with
  | Expr e -> string_of_expr e
  | Comp c -> string_of_comp c
