open Knel_lib
open Parser
open P

let is_some = function
  | Some _ -> true
  | None -> false

let unwrap = function
  | Some v -> v
  | None -> failwith "NYI"

let test_fun_fun () =
  Alcotest.(check bool) "parse 01" (is_some (parse p_state terms "\\x:i->\\x:i->x")) true;
  Alcotest.(check bool) "parse 02" (is_some (parse p_state terms "\\x:i->\\y:option i->y x")) true;
  Alcotest.(check bool) "parse 03" (is_some (parse p_state terms "\\x:i->\\y:option i->")) false;
  Alcotest.(check bool) "parse white spaces" (is_some (parse p_state terms "x\t\n\n\t   \t\n x")) true

let test = [
  ("parsing (short)", `Quick, test_fun_fun)
]

let test_alpha () =
  Alcotest.(check bool) "fun x x->x x <> fun x y -> y x" (Typer.is_alpha_eq []
  (unwrap (parse p_state terms "\\x:i->\\x:i->x x"))
  (unwrap (parse p_state terms "\\x:i->\\y:i->y x"))
  ) false;
  Alcotest.(check bool) "fun x x->x = fun x y -> y" (Typer.is_alpha_eq []
  (unwrap (parse p_state terms "\\x:i->\\x:i->x"))
  (unwrap (parse p_state terms "\\x:i->\\y:i->y"))
  ) true

let test_alpha = [
  ("alpha equivalence (short)", `Quick, test_alpha)
]