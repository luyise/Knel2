
open Syntax
open Typer
open Parser

let wrap_loc : naked_term parsing -> term parsing = fun r ->
  combine_rules
      get_loc
      (combine_rules r get_loc (fun v l -> (v, l)))
      (fun l1 (t, l2) -> {term = t; loc = ("", l1, l2)})

let ws_before : 'a parsing -> 'a parsing = fun r ->
  combine_rules match_wspace r (fun _ i -> i)

let () = add_custom_rule p_state 0 Terms (combine_rules
    get_loc
    (combine_rules match_alphas get_loc (fun i l -> (i, l)))
    (fun l1 (i, l2) -> {term = Var (i, None); loc = ("", l1, l2)}))


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

let unwrap = function
  | Some v -> v
  | None -> failwith "Unwrap failed"

let () = assert (not (is_alpha_eq []
      (unwrap (parse p_state Terms "\\x:i->\\x:i->x x"))
      (unwrap (parse p_state Terms "\\x:i->\\y:i->y x"))))

let () = assert (is_alpha_eq []
    (unwrap (parse p_state Terms "\\x:i->\\x:i->x"))
    (unwrap (parse p_state Terms "\\x:i->\\y:i->y")))


let () = print_endline "Hello, world"
