open Parser
open Syntax
open Type

(* Inititialize context by collecting all effects and lets *)
let collect_context (ast : toplevel list) : context =
  let rec go ast acc =
    match ast with
    | [] -> acc
    | Expr (Effect (e, ty1, ty2) as expr) :: xs ->
      go xs ((e, (type_expr expr acc, [])) :: acc)
    | Comp (Let (v, c)) :: xs ->
      go xs ((v, type_comp c []) :: acc)
    | _::xs -> go xs acc
  in
  go ast []
;;

let type_check (effects : context) (toplevel : toplevel) =
  let ty = match toplevel with
    | Expr e -> type_expr e effects, []
    | Comp c -> type_comp c effects
  in
  let _ = print_endline (string_of_dirty ty)
  in ()

let main () =
  (* TODO warn if file name not given *)
  let filename = Sys.argv.(1) in
  let ch = open_in filename in
  let lexbuf = Lexing.from_channel ch in
  let ast = (Parser.file Lexer.token) lexbuf in
  (* first collect effects, assume they are global *)
  let effects = collect_context ast in
  (* using the effects as context, type check the file *)
  let _ = List.map (type_check effects) ast in
  (* type check all expr/comp in file *)
  (* when encounter an effect definition, add to context *)
  print_endline (String.concat "\n" (List.map string_of_toplevel ast))
;;

main ()