open Syntax

(* type scheme context for hm inference *)
type senv = (name * scheme) list
let string_of_ints sep ints = String.concat sep (List.map string_of_int ints)
let string_of_scheme (Forall (alphas, t)) = "∀" ^ (string_of_ints ", " alphas) ^ (string_of_dirty t)
let string_of_senv senv =
  String.concat ", " (List.map (fun (n, s) -> n ^ (string_of_scheme s)) senv)

(* type representing substitution for type unification *)
type subs = (int * dirty) list
let string_of_sub (i, t) = string_of_int i ^ ">->" ^ (string_of_dirty t)
let string_of_subs subs = String.concat ", " (List.map string_of_sub subs)

(* substitue ty using subs *)
let rec sub subs dty =
  match dty with
  | TUnit, _ | TBool, _ | TInt, _ | TEffect _, _ | THandler _, _ -> dty
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

(* Get a fresh type variable *)
let fresh () =
  let count = ref 0 in
  count := !count + 1;
  TyVar !count

let rec ftv (ty : ty) =
  match ty with
  | TUnit | TBool | TInt
  | TEffect _ | THandler _
    -> []
  | TArrow (t1, (t2, _)) -> ftv(t1) @ ftv(t2)
  | TyVar a -> [a]

let rec ftv_d (ty, _) =
  ftv ty

(* subtract l2 from l1, return members of l1 which cannot be found in l2 *)
let list_subtract l1 l2 =
  List.filter (fun x -> not (List.mem x l2)) l1

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
  | TInt, TInt -> []
  | TArrow (t1, (t2,d)), TArrow (t1', (t2', d')) ->
    let s1 = unify t1 t1' in
    let s2 = unify_d (sub s1 (t2, d)) (sub s1 (t2', d)) in
    s1 @ s2
  | TEffect _, TEffect _ -> []
  | THandler ((t1, d1), (t2, d2)), THandler ((t1', d1'), (t2', d2'))->
    (* Let's say we are trying to unify these 2 handlers:
     *  1. tbool []         -> tbool []
     *  2. tbool ['choice'] -> tv
     * What should tv be? Ans: tbool ['choice']
     * Because the handler doesn't handle 'choice'
     * *)
    (* infer argument types *)
    let s1 = unify t1 t1' in
    (* infer return param *)
    let s2 = unify_d (sub s1 (t2, d2)) (sub s1 (t2', d2')) in
    (* infer the effects by observing what is handled, this is left-biased
     * because we decide what effects is handled based on the left arg *)
    let handled = list_subtract  d1 d2 in
    let unhandled = list_subtract d1' handled in
    (* add unhandled effects to the substitution *)
    List.map (fun (i, (ty, d)) -> i, (ty, unhandled @ d)) (s2 @ s2)
  | TyVar a, t | t, TyVar a -> bind a t
  | TEffect (op, t1, t2), TArrow (t1', t2') ->
    let s1 = unify t1 t1' in
    let s2 = unify_d (t2, [op]) t2' in
    s1 @ s2
  | _ -> failwith ("Cannot unify: " ^ (string_of_ty t1) ^ " with " ^ (string_of_ty t2))

(* right now only unify types *)
and unify_d (t1, d1) (t2, d2) =
  (* need to merge dirty types somehow *)
  let subs = unify t1 t2 in
  List.map (fun (tv, (ty, d)) -> tv, (ty, d1 @ d2 @ d)) subs


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
  | Int _ -> TInt, []

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
  | Handler (cls, clauses) ->
    infer_clauses cls clauses senv
    (* let _ = infer_clauses cls clauses senv in *)
    (* THandler((TUnit, []), (TUnit, [])), [] *)

and infer_clauses cls clauses (senv : senv) : ty * subs =
  let open Type in
  let context = List.map (fun (n, Forall (_, ty)) -> n, ty) senv in
  type_handler cls clauses context, []
  (* let tv = fresh (), [] in *)
  (* (1* first infer a type for the val clause *1) *)
  (* let t1, s1 = *)
  (*   begin *)
  (*     match cls with *)
  (*     | PVal (n, _, c) -> *)
  (*       let t1, s1 = infer_c c ((n, Forall ([], tv)) :: senv) in *)
  (*       t1, s1 *)
  (*     | _ -> failwith "Should only have PVal in cls" *)
  (*   end *)
  (* in *)
  (* let i c = *)
  (*   begin *)
  (*     match c with *)
  (*     | PEffect (op, n, k, c) -> *)
  (*       let tv' = fresh (), [] in *)
  (*       let t, s = infer_c c ((n, Forall ([], tv)) :: (k, Forall ([], tv')) :: senv) in *)
  (*       t, s *)
  (*       (1* lookup op to get types *1) *)
  (*       (1* give y and k type *1) *)
  (*       (1* get comp type *1) *)
  (*       (1* unify op with comp type *1) *)
  (*     | _ -> failwith "dk effect" *)
  (*   end *)
  (* in *)
  (* let ctys = List.map i clauses in *)
  (* let tots = (t1, s1) :: ctys in *)
  (* (1* cls should be a' to b' *1) *)
  (* (1* | PVal of name * ty * comp *1) *)
  (* (1*           b'     b'   b' *1) *)
  (* (1* | PEffect of name * name * name * comp *1) *)
  (* (1*              a'->b' a'     b'->k' b' *1) *)
  (* THandler((TUnit, []), (TUnit, [])), [] *)


and infer_c (comp : comp) senv : dirty * subs =
  match comp with
  | Val e -> let ty, subs = infer e senv in (ty, []), subs
  | Handle (c, e) ->
    let t1, s1 = infer e senv in
    let t2, s2 = infer_c c senv in
    let ty : dirty = fresh (), [] in
    let s3 = unify t1 (THandler(t2, ty)) in
    sub s3 ty, s1 @ s2 @ s3
    (* (match t1 with *)
    (*  | Syntax.THandler (_, (t2, d2)) -> sub s1 (t2, d2), s1 *)
    (*  | _ -> failwith "Can only handle with THandler") *)
  | App (e1, e2) ->
    let t1, s1 = infer e1 senv in
    let t2, s2 = infer e2 (sub_senv s1 senv) in
    let tv = fresh () in (* represents type of application *)
    let s3 = unify (fst (sub s2 (t1, []))) (TArrow (t2, (tv, []))) in
    sub s3 (tv, []), s1 @ s2 @ s3
  (* this let expression is wrong theres no body :( let me fix it next time *)
  | Let (x, e1) ->
    (* let ty = generalize (fresh ()) in *)
    let t1, s1 = infer_c e1 senv in
    t1, s1
