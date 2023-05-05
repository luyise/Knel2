open Knel_lib
open Parser_lib

let match_add : (int -> int) parsing =
  seq_operator (match_char '+') (get_rules Ints) (fun _ j i -> i + j)

let match_sub : (int -> int) parsing =
  seq_operator (match_char '-') (get_rules Ints) (fun _ j i -> i - j)

let match_mul : (int -> int) parsing =
  seq_operator (match_char '*') (get_rules Ints) (fun _ j i -> i * j)

let match_div : (int -> int) parsing =
  seq_operator (match_char '/') (get_rules Ints) (fun _ j i -> i / j)

let p_state = gen_p_state ()
let () = add_custom_rule p_state 0 Ints match_int
let () = add_left_assoc p_state 50 Ints match_add
let () = add_left_assoc p_state 50 Ints match_sub
let () = add_left_assoc p_state 30 Ints match_mul
let () = add_left_assoc p_state 30 Ints match_div

let test_parsing () =
  Alcotest.(check (option int)) "big expr" (parse p_state Ints "1+3*4/3/1*1000/3/4/5-6-2") (Some (1+3*4/3/1*1000/3/4/5-6-2))


let test = [
  ("Parsing int", `Quick, test_parsing)
]
