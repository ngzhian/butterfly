open Syntax

(* Context maps a name (variable) to a type *)
type context = (name * dirty) list

let add_pure name ty context = (name, (ty, [])) :: context
let add_dirty name ty context = (name, ty) :: context

(* adding some tentative HM inference stuff *)
(* based on pure types, should add in dirt afterwards *)
(* type scheme context for hm inference *)
type senv = (name * scheme) list
let string_of_ints sep ints = String.concat sep (List.map string_of_int ints)
let string_of_scheme (Forall (alphas, t)) = "∀" ^ (string_of_ints ", " alphas)
let string_of_senv senv =
  String.concat ", " (
    List.map (fun (n, s) -> n ^ (string_of_scheme s)) senv
  )
(* type representing substitution for type unification *)
type subs = (int * dirty) list
let string_of_sub (i, t) = string_of_int i ^ ">->" ^ (string_of_dirty t)
let string_of_subs subs = String.concat ", " (List.map string_of_sub subs)

(* Get a fresh type variable *)
let fresh () =
  let count = ref 0 in
  count := !count + 1;
  TyVar !count

(* substitue ty using subs *)
let rec sub subs dty =
  match dty with
  | TUnit, _ | TBool, _ | TEffect _, _ | THandler _, _ -> dty
  | TArrow (t1, (t2, d)), dirt ->
    let arg_ty, _ = sub subs (t1, []) in
    TArrow(arg_ty, (sub subs (t2, d))), dirt
  | TyVar x, _ ->
    try
      List.assoc x subs
    with Not_found -> dty

let rec sub_s subs (Forall (alphas, dty)) =
  (* cannot substitute under quantification *)
  let subs' = List.filter (fun (ty, _) -> not (List.mem ty alphas)) subs in
  Forall(alphas, sub subs' dty)

let rec sub_senv (subs : subs) (senv : senv) =
  List.map (fun (tv, s) -> tv, sub_s subs s) senv

(* TODO change to ty scheme *)
let rec ftv (ty : ty) =
  match ty with
  | TUnit | TBool
  | TEffect _ | THandler _
    -> []
  | TArrow (t1, (t2, _)) -> ftv(t1) @ ftv(t2)
  | TyVar a -> [a]

let rec ftv_d (ty, _) =
  ftv ty

(* subtract l2 from l1, return members of l1 which cannot be found in l2 *)
let list_subtract l1 l2 =
  List.filter (fun x -> List.mem x l2) l1

(* Free type variables in type scheme *)
let ftv_s (Forall (alphas, ty)) =
  list_subtract (ftv_d ty) alphas

(* Free type variables in type scheme env *)
let rec ftv_senv senv =
  List.concat (List.map (fun x -> ftv_s (snd x)) senv)

(* Unify two types, returns a substitution making two types the same *)
let rec unify (t1 : ty) (t2 : ty) =
  let bind (tvar : int) (t : ty) =
    if TyVar tvar = t
    then []
    else if List.mem tvar (ftv t)
    then failwith "Bound tvar"
    else [(tvar, (t, []))]
  in
  match t1, t2 with
  | TUnit, TUnit -> []
  | TBool, TBool -> []
  | TArrow (t1, (t2,d)), TArrow (t1', (t2', d')) ->
    let s1 = unify t1 t1' in
    (* TODO arbitrary choose dirt from left *)
    let s2 = unify_d (sub s1 (t2, d)) (sub s1 (t2', d)) in
    s1 @ s2
  | TEffect _, TEffect _ -> []
  | THandler ((t1, d1), (t2, d2)), THandler ((t1', d1'), (t2', d2'))->
    let s1 = unify t1 t1' in
    (* TODO arbitrary choose dirt from left *)
    let s2 = unify_d (sub s1 (t2, d2)) (sub s1 (t2', d2')) in
    s1 @ s2
  | TyVar a, t | t, TyVar a -> bind a t
  | _ -> failwith "Cannot unify"

and unify_d (t1, _) (t2, _) =
  unify t1 t2


(* Instantiate all quantifiers in a type scheme *)
let rec instantiate (Forall (alphas, ty)) : dirty =
  match alphas with
  | [] -> ty
  | a::alphas ->
    let ty' = sub [(a, (fresh (), []))] ty in
    instantiate (Forall (alphas, ty'))

(* Generalize a type *)
let rec generalize (senv : senv) ty : scheme =
  let alphas = list_subtract (ftv ty) (ftv_senv senv) in
  Forall (alphas, (ty, []))

let rec infer (term : expr) senv : ty * subs =
  match term with
  | Unit  -> TUnit, []
  | Bool _ -> TBool, []

  | Fun (_,x,_, body) ->
    let tv = fresh (), [] in
    let senv' = (x, Forall ([], tv)) :: senv in
    let t1, s1 = infer_c body senv' in
    let sty, _ = sub s1 tv in
    TArrow(sty, t1), s1
  | Effect (e, ty1, ty2) -> TEffect (e, ty1, ty2), []
  | Var x ->
    let x = try List.assoc x senv with Not_found -> failwith ("Cannot infer " ^ x) in
    let ity, _ = instantiate x in
    ity, []
  | Handler (cls, clauses) -> THandler((TUnit, []), (TUnit, [])), []

and infer_c (comp : comp) senv : dirty * subs =
  match comp with
  | Val e -> let ty, subs = infer e senv in (ty, []), subs
  | Handle (c, e) ->
    let t1, s1 = infer e senv in
    (match t1 with
     | Syntax.THandler (_, (t2, d2)) -> sub s1 (t2, d2), s1
     | _ -> failwith "Can only handle with THandler")
  | App (e1, e2) ->
    let t1, s1 = infer e1 senv in
    let t2, s2 = infer e1 (sub_senv s1 senv) in
    let tv = fresh () in (* represents type of application *)
    let s3 = unify (fst (sub s2 (t1, []))) (TArrow (t2, (tv, []))) in
    sub s3 (tv, []), s1 @ s2 @ s3
  (* this let expression is wrong theres no body :( let me fix it next time *)
  | Let (x, e1) ->
    (* let ty = generalize (fresh ()) in *)
    let t1, s1 = infer_c e1 senv in
    t1, s1

(* Check each clause and ensure consistency *)
let rec type_clause c (context : context) k_ty : dirty =
  match c with
  | PVal (x, ty, c) ->
    let cty = type_comp c context in
    (* lookup x in context, or lookup the effect associated with this clause, and the type is the result type *)
    cty
  | PEffect (op, y, k, c) ->
    let ty = try List.assoc op context with Not_found -> failwith "Effect nf" in
    (* check that op is indeed an op *)
    let kty = (match ty with
        | (TEffect (_, ty1, ty2), dirt) -> (k, (TArrow(ty2, k_ty), []))
        | _ -> failwith "wrong"
      ) in
    let cls_context = (y, ty) :: kty :: context in
    (* look up op in context to get the type of effect *)
    (* k should match op *)
    let cty = type_comp c cls_context in
    cty

and type_handler cls clauses context : ty =
    (* arg type of the handler using the value clause *)
    let val_pat_context, arg_ty = (match cls with
        | PVal (x, ty, _) -> add_pure x ty context, ty
        | _ -> failwith "Missing value pattern clause") in
    (* get type of the value clause *)
    (* this is also the type that all other clauses should have *)
    let dty1 = type_clause cls val_pat_context (TUnit, []) in
    let add_dirt (ty, d) d' = (ty, d @ d') in
    (* get types of all the pattern clauses *)
    let k_ty = dty1 in
    let tys = List.map (fun c -> type_clause c context k_ty) clauses in
    let op_of = function
      | PEffect (op, _, _, _) -> op
      | _ -> failwith "Unexpected value pattern" in
    (* dirt that can be handled by the clauses by pattern matching *)
    let handled_dirt = List.map op_of clauses in
    (* dirt produced by computation in the pattern clauses *)
    let dirt2 = List.concat (List.map snd tys) in
    (* in handling an effect, dirt can be produced by the value and pattern clauses *)
    (* the resulting type of a handler is like an arrow, *)
    (* it has an input type with some dirt, and produced some dirt as well *)
    THandler ((arg_ty, handled_dirt), add_dirt dty1 dirt2)

(* Type a pure expression *)
and type_expr e (context : context) =
  match e with
  | Unit  -> TUnit

  | Bool _ -> TBool

  | Fun (_, x, ty1, body) ->
    (* argument types will be provided as annotations on the function *)
    let context' = add_pure x ty1 context in
    let ty2 = type_comp body context' in
    TArrow (ty1, ty2)

  (* effect declaration *)
  | Effect (e, ty1, ty2) -> TEffect (e, ty1, ty2)

  | Var x ->
    (* type of a variable is looked up from the context *)
    fst (try List.assoc x context with Not_found -> failwith "var not found")

  | Handler (cls, clauses) ->
    type_handler cls clauses context

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

  | App (e1, e2) ->
    let e_ty = type_expr e1 context in
    let e2_ty = type_expr e2 context in
    (match e_ty with
     (* normal function application *)
     | TArrow (ty1, ty2) ->
       if ty1 = e2_ty (* make sure function param and formal arg matches *)
       then ty2
       else failwith "TArrow type mismatch"
     (* calling an effectful operation *)
     | TEffect (op, ty1, ty2) ->
       if ty1 = e2_ty
       then ty2, [op]
       else failwith "TEffect type mismatch"
     | _ -> failwith "Cannot apply non-arrow or non-effect type")

  | Let (x, c) -> type_comp c context
