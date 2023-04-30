open Knel_lib
open Syntax
open Parser

let p_state = gen_p_state ()

let wrap_loc : naked_term parsing -> term parsing = fun r ->
  combine_rules
      get_loc
      (combine_rules r get_loc (fun v l -> (v, l)))
      (fun loc_start (t, loc_end) -> {term = t; loc = loc_start})

let ws_before : 'a parsing -> 'a parsing = fun r ->
  combine_rules match_wspace r (fun _ i -> i)

let () = add_custom_rule p_state 0 Terms (combine_rules
    get_loc
    (combine_rules match_alphas get_loc (fun i l -> (i, l)))
    (fun loc_start (i, loc_end) -> {term = Var (i, None); loc = loc_start}))


let () = add_custom_rule p_state 80 Terms (wrap_loc (
  combine_rules
    (combine_rules (match_char '\\') (ws_before match_alphas) (fun _ i -> i))
    (combine_rules
      (combine_rules (ws_before (match_char ':')) (ws_before (get_rules Terms)) (fun _ t -> t))
      (combine_rules (ws_before (match_string "->")) (ws_before (get_rules Terms)) (fun _ t -> t))
      (fun t1 t2 -> (t1, t2))
    )
    (fun i (t1, t2) -> Lam (i, t1, t2))))

let () = add_left_assoc p_state 40 Terms
  (map (ws_before (get_rules Terms)) (fun a f -> let (fname, l1, _) = f.loc in
                                      let (_, _, l2) = a.loc in
                                      {term = App (f, a); loc = (fname, l1, l2)}))

let is_some = function
  | Some _ -> true
  | None -> false

let unwrap = function
  | Some v -> v
  | None -> failwith "NYI"

let test_fun_fun () =
  Alcotest.(check bool) "parse 01" (is_some (parse p_state Terms "\\x:i->\\x:i->x")) true;
  Alcotest.(check bool) "parse 02" (is_some (parse p_state Terms "\\x:i->\\y:option i->y x")) true;
  Alcotest.(check bool) "parse 03" (is_some (parse p_state Terms "\\x:i->\\y:option i->")) false;
  Alcotest.(check bool) "parse white spaces" (is_some (parse p_state Terms "x\t\n\n\t   \t\n x")) true

let test = [
  ("parsing (short)", `Quick, test_fun_fun)
]

let test_alpha () =
  Alcotest.(check bool) "fun x x->x x <> fun x y -> y x" (Typer.is_alpha_eq []
  (unwrap (parse p_state Terms "\\x:i->\\x:i->x x"))
  (unwrap (parse p_state Terms "\\x:i->\\y:i->y x"))
  ) false;
  Alcotest.(check bool) "fun x x->x = fun x y -> y" (Typer.is_alpha_eq []
  (unwrap (parse p_state Terms "\\x:i->\\x:i->x"))
  (unwrap (parse p_state Terms "\\x:i->\\y:i->y"))
  ) true

let test_alpha = [
  ("alpha equivalence (short)", `Quick, test_alpha)
]