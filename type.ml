open Syntax

(* Context maps a name (variable) to a type *)
type context = (name * ty) list

(* Effect environment maps a name to an effect *)
type effects = (name * (ty * ty)) list

let string_of_effect_env env =
  String.concat ", " (List.map (fun (n, (ty1, ty2)) -> n) env)

(* Check each clause and ensure consistency *)
let rec type_clause c context effects : dirty =
  match c with
  | PVal (x, ty, c) ->
    let cty = type_comp c context effects in
    (* lookup x in context, or lookup the effect associated with this clause, and the type is the result type *)
    (* todo need to figure out somehow what the input type is *)
    cty
  | PEffect (op, y, k, c) ->
    (* look up op in effects to get the type of effect *)
    (* let ty1, ty2 = List.assoc op effects in *)
    let cty = type_comp c context effects in
    cty

(* Type a pure expression *)
and type_expr e context effects : ty =
  match e with
  | Unit  -> TUnit
  | Bool _ -> TBool
  | Fun (_, _, ty1, body) ->
    (* types will be provided as annotations on the function *)
    let ty2 = type_comp body context effects in
    TArrow (ty1, ty2)
  | Effect (e, ty1, ty2) -> TEffect (e, ty1, ty2)
  | Var x ->
    (* type of a variable is looked up from the context *)
    List.assoc x context

  (* type check handlers *)
  | Handler (cls, clauses) ->
    (* first try to get the arg type of the handler using the value clause *)
    let handler_context, arg_ty = (match cls with
     | PVal (x, ty, _) -> (x, ty) :: context, ty
     | _ -> failwith "fail") in
    (* get type of the value clause *)
    let ty1, dirt1 = type_clause cls handler_context effects in
    (* get types of all the patter clauses *)
    let tys = List.map (fun c -> type_clause c context effects) clauses in
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
and type_comp c context effects : dirty =
  match c with
  | Val x -> type_expr x context effects, []
  (* lookup op in effects map, ensure types match *)
  (* probably want to store effects in a separate map *)
  | Op (o, _) -> snd (List.assoc o effects), [o]

  | Handle (c, e) ->
    (* e should be of type Handler *)
    let handler_ty = type_expr e context effects in
    (match handler_ty with
     | THandler ((ty1, handled_dirt), (ty2, produced_dirt)) ->
       let comp_ty, comp_dirt = type_comp c context effects in
       (* ensure that handler input and computation has same type *)
       (* the dirt doesnt matter here, we only use it to calculate the dirt of this comp *)
       if comp_ty == ty1
       then ty2, (List.filter (fun d -> not (List.mem d handled_dirt)) comp_dirt) @ produced_dirt
       else failwith "Incompatible type between handler and computation"
     | _ -> failwith "Can only handle with an expression of type Handler")

  | App (e, c) ->
    let e_ty = type_expr e context [] in
    (match e_ty with
     | TArrow (t1, t2) -> t2
     | _ -> failwith "Cannot apply non-arrow type")
  | Let (x, c) -> type_comp c context []
