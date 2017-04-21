open Syntax
open Type

let op = "choice"
let x = Bool true;;
let eff = Effect (op, TUnit, TBool);;
let eff_ty = TEffect (op, TUnit, TBool);;
let op_call = App (Var op, Val Unit);;
let handler = Handler (PVal ("x", TBool, Val (Var "x")), []);;
let handle = Handle (op_call, handler);;
let actually_handler = Handler (PVal ("x", TBool, Val (Var "x")), [PEffect (op, "y", "k", Val Unit)]);;
let actually_handle = Handle (op_call, actually_handler);;

let main () =
  print_endline (string_of_expr x)
;;
print_endline (string_of_expr x);;
print_endline (string_of_comp handle);;
print_endline (string_of_dirty (type_comp op_call [(op, eff_ty)]));;
print_endline (string_of_dirty (type_comp handle [(op, eff_ty)]));;
print_endline (string_of_dirty (type_comp actually_handle [(op, eff_ty)]));;
