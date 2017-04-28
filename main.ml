open Parser
open Syntax
open Type
open Infer

(* Inititialize context by collecting all effects and lets *)
let collect_context (ast : toplevel list) : context =
  let rec go ast acc =
    match ast with
    | [] -> acc
    | Expr (Effect (e, ty1, ty2) as expr) :: xs ->
      go xs ((e, (type_expr expr acc, [])) :: acc)
    | Comp (Let (v, c)) :: xs ->
      go xs ((v, type_comp c acc) :: acc)
    | _::xs -> go xs acc
  in
  go ast []
;;

let type_of (effects : context) (toplevel : toplevel) =
  match toplevel with
    | Expr e -> type_expr e effects, []
    | Comp c -> type_comp c effects

let infer (effects : context) (toplevel : toplevel) =
  let senv = List.map (fun (n, (t, d)) -> n, Forall ([], (t, d))) effects in
  let (t, d), s = match toplevel with
    | Syntax.Expr e -> let t, s = infer e senv in (t, []), s
    | Syntax.Comp c -> infer_c c senv in
  (t, d), s

let main () =
  (* exit if we don't get a filename *)
  if Array.length Sys.argv < 2
  then failwith "Filename not given";

  (* parse the file into an ast *)
  let filename = Sys.argv.(1) in
  let ch = open_in filename in
  let lexbuf = Lexing.from_channel ch in
  let ast = (Parser.file Lexer.token) lexbuf in

  (* first collect lets and effects, assume they are global *)
  let context = collect_context ast in
  (* using the context as context, type check the file *)
  List.iter
    (fun tl ->
       let (it, id), is = infer context tl in
       let _ = print_endline "### Inferred ###" in
       let _ = print_endline (string_of_subs is) in
       let _ = print_endline (string_of_dirty (it, id)) in
       let _ = print_endline ">>> Typed ###" in
       let ty = type_of context tl in
       print_endline (string_of_toplevel tl ^ " : " ^ (string_of_dirty ty)))
    ast
;;

main ()
