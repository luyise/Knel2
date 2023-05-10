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
  Alcotest.(check bool) "parse_full 01" (is_some (parse_full p_state terms "\\x:i->\\x:i->x")) true;
  Alcotest.(check bool) "parse_full 02" (is_some (parse_full p_state terms "\\x:i->\\y:option i->y x y")) true;
  Alcotest.(check bool) "parse_full 03" (is_some (parse_full p_state terms "\\x:i->\\y:option i->")) false;
  Alcotest.(check bool) "parse_full white spaces" (is_some (parse_full p_state terms "x\t\n\n\t   \t\n x")) true

let test_notation () =
  Alcotest.(check bool) "parse_full notation 01" (is_some (parse_full p_state default "Notation \"x '+' y\" := (add x y) left-assoc 40")) true;
  Alcotest.(check bool) "parse_full notation 02" (is_some (parse_full p_state default "Notation \"x '-' y\" := (sub x y) left-assoc 40")) true;
  Alcotest.(check bool) "parse_full notation 03" (is_some (parse_full p_state default "Notation \"'|' x '|'\" := (abs x ) no-assoc 50")) true;
  Alcotest.(check bool) "parse_full notation 04" (is_some (parse_full p_state default "Notation \"'-' x\" := (neg x) right-assoc 30")) true;
  Alcotest.(check bool) "parse_full notation 05" (is_some (parse_full p_state default "Notation \"x '+' y\" := (add x y) left-assoc")) false;
  Alcotest.(check bool) "parse_full notation 06" (is_some (parse_full p_state default "Notation \"x '-' y\" := (sub x y) 40")) false;
  Alcotest.(check bool) "parse_full notation 07" (is_some (parse_full p_state default "Notation \"'| x '|'\" := (abs x ) no-assoc 50")) false;
  Alcotest.(check bool) "parse_full notation 08" (is_some (parse_full p_state default "Notation \"- x\" := (neg x) right-assoc 30")) false

let test = [
  ("parsing (short)", `Quick, test_fun_fun);
  ("parsing (notation)", `Quick, test_notation)
]

let test_alpha () =
  Alcotest.(check bool) "fun x x->x x <> fun x y -> y x" (Alpha.is_alpha_eq []
  (unwrap (parse_full p_state terms "\\x:i->\\x:i->x x"))
  (unwrap (parse_full p_state terms "\\x:i->\\y:i->y x"))
  ) false;
  Alcotest.(check bool) "fun x x->x = fun x y -> y" (Alpha.is_alpha_eq []
  (unwrap (parse_full p_state terms "\\x:i->\\x:i->x"))
  (unwrap (parse_full p_state terms "\\x:i->\\y:i->y"))
  ) true

let test_alpha = [
  ("alpha equivalence (short)", `Quick, test_alpha)
]