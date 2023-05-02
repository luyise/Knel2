open Knel_lib
open Parser
open Test_syntax

let () =
  let p_state = gen_p_state () in
  let () = add_custom_rule p_state 0 Ints match_int in
  let () = add_left_assoc p_state 50 Ints match_add in
  let () = add_left_assoc p_state 50 Ints match_sub in
  let () = add_left_assoc p_state 30 Ints match_mul in
  let () = add_left_assoc p_state 30 Ints match_div in
  match parse p_state Ints "1+3*4/3/1*1000/3/4/5-6-2" with
  | Some i when i = 1+3*4/3/1*1000/3/4/5-6-2 -> print_endline "Success"
  | Some i -> print_string "Computation error : got "; print_int i; print_newline ()
  | None -> print_endline "Parse failure"
