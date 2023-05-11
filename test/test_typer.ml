open Knel_lib
open Syntax
open Parser
open Beta
open Typer
open P

let p_state = new_p_state ()

let is_some = function
  | Some _ -> true
  | None -> false

let unwrap = function
  | Some v -> v
  | None -> failwith "NYI"

let test_typing () =
  Alcotest.(check bool) "(\\x:i->x) : Pi(x:i),i" (Beta.is_alpha_beta_eq std_rules ["i"]
    (typ std_rules ["i"]  [("i", wrap_nterm (UU(0)))] (unwrap (parse_full p_state terms "\\x:i->x")))
    (unwrap (parse_full p_state terms "Pi x:i, i"))
    ) true;
  Alcotest.(check bool) "(\\x:i->\\y:i->y) : Pi(x:i),Pi(y:i),i" (Beta.is_alpha_beta_eq std_rules ["i"]
    (typ std_rules ["i"]  [("i", wrap_nterm (UU(0)))] (unwrap (parse_full p_state terms "\\x:i->\\y:i->y")))
    (unwrap (parse_full p_state terms "Pi x:i, Pi y:i, i"))
    ) true;
  Alcotest.(check bool) "(\\x:i->\\x:j->x) : Pi(x:i),Pi(y:j),j" (Beta.is_alpha_beta_eq std_rules ["i"; "j"]
    (typ std_rules ["i"]  [("i", wrap_nterm (UU(0))); ("j", wrap_nterm (UU(1)))] (unwrap (parse_full p_state terms "\\x:i->\\x:j->x")))
    (unwrap (parse_full p_state terms "Pi x:i, Pi y:j, j"))
    ) true

let test_typer = [
  ("typing (short)", `Quick, test_typing)
]