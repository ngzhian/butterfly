open Syntax

(* Context maps a name (variable) to a type *)
type context = (name * dirty) list

(* Check each clause and ensure consistency *)
let rec type_clause c (context : context) : dirty =
  match c with
  | PVal (x, ty, c) ->
    let cty = type_comp c context in
    (* lookup x in context, or lookup the effect associated with this clause, and the type is the result type *)
    cty
  | PEffect (op, y, k, c) ->
    let ty = try List.assoc op context with Not_found -> failwith "Effect nf" in
    (* check that op is indeed an op *)
    let kty = (match ty with
        (* TODO fix this, k is hardcoded to be of type ty2 -> (Unit, []) *)
        | (Syntax.TEffect (_, ty1, ty2), dirt) -> (k, (TArrow(ty2, (TUnit, [])), []))
        | _ -> failwith "wrong"
      ) in
    let cls_context = (y, ty) :: kty :: context in
    (* look up op in context to get the type of effect *)
    (* k should match op *)
    let cty = type_comp c cls_context in
    cty

(* Type a pure expression *)
and type_expr e (context : context) =
  match e with
  | Unit  -> TUnit

  | Bool _ -> TBool

  | Fun (_, _, ty1, body) ->
    (* argument types will be provided as annotations on the function *)
    let ty2 = type_comp body context in
    TArrow (ty1, ty2)

  (* effect declaration *)
  | Effect (e, ty1, ty2) -> TEffect (e, ty1, ty2)

  | Var x ->
    (* type of a variable is looked up from the context *)
    fst (try List.assoc x context with Not_found -> failwith "var not found")

  | Handler (cls, clauses) ->
    (* first try to get the arg type of the handler using the value clause *)
    let val_pat_context, arg_ty = (match cls with
        | PVal (x, ty, _) -> (x, (ty, [])) :: context, ty
        | _ -> failwith "Missing value pattern clause") in
    (* get type of the value clause *)
    let ty1, dirt1 = type_clause cls val_pat_context in
    (* get types of all the patter clauses *)
    let tys = List.map (fun c -> type_clause c context) clauses in
    let op_of = function
      | PEffect (op, _, _, _) -> op
      | _ -> failwith "Unexpected value pattern" in
    (* dirt that can be handled by the clauses by pattern matching *)
    let handled_dirt = List.map op_of clauses in
    (* dirt produced by computation in the pattern clauses *)
    let dirt2 = List.concat (List.map snd tys) in
    (* in handling an effect, dirt can be produced by the value and pattern clauses *)
    let produced_dirt = dirt1 @ dirt2 in
    (* the resulting type of a handler is like an arrow, *)
    (* it has an input type with some dirt, and produced some dirt as well *)
    THandler ((arg_ty, handled_dirt), (ty1, produced_dirt))

(* Type an effectful expression *)
and type_comp c context : dirty =
  match c with
  | Val x -> type_expr x context , []

  | Handle (c, e) ->
    (* e should be of type Handler *)
    let handler_ty = type_expr e context in
    (match handler_ty with
     | THandler ((ty1, handled_dirt), (ty2, produced_dirt)) ->
       let comp_ty, comp_dirt = type_comp c context in
       (* ensure that handler input and computation has same type *)
       (* the dirt doesnt matter here, we only use it to calculate the dirt of this comp *)
       if comp_ty == ty1
       then ty2, (List.filter (fun d -> not (List.mem d handled_dirt)) comp_dirt) @ produced_dirt
       else failwith "Incompatible type between handler and computation"
     | _ -> failwith "Can only handle with an expression of type Handler")

  | App (e, c) ->
    let e_ty = type_expr e context in
    (match e_ty with
     (* normal function application *)
     | TArrow (t1, t2) -> t2
     (* calling an effectful operation *)
     | TEffect (op, t1, t2) -> t2, [op]
     | _ -> failwith "Cannot apply non-arrow or non-effect type")

  | Let (x, c) -> type_comp c context
