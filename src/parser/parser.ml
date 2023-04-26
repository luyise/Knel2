
type parsing_state = string * int

let max_prio = 100
(* type priority = int * int *)

type 'a parser_rule =
  | Custom : 'a parsing -> 'a parser_rule
  | Units : unit parser_rule
  | Ints : int parser_rule
and 'a parsing = int -> parser_state -> parsing_state -> ('a * parsing_state) option
and 'a rule =
  | NoRule
  | BasicRules : 'a parsing list -> 'a rule
  | LeftAssoc : ('b parsing * ('a -> 'b -> 'a)) list -> 'a rule
and 'a parsers = 'a rule array
and parser_state = {
  units : unit parsers;
  ints : int parsers;
}

let fold_left (r1 : 'a parsing) (b2 : 'b parsing) (f : 'a -> 'b -> 'a) : 'a parsing =
  fun prio p_state c_state ->
    match r1 prio p_state c_state with
    | None -> None
    | Some (init, c_state) ->
      let rec aux v c_state =
        match b2 prio p_state c_state with
        | Some (v', c_state) ->  aux (f v v') c_state
        | None -> Some (v, c_state)
      in aux init c_state

let fold_leftL (r1 : 'a parsing) (casesL : ('b parsing * ('a -> 'b -> 'a)) list) : 'a parsing =
  fun prio p_state c_state ->
    match r1 prio p_state c_state with
    | None -> None
    | Some (init, c_state) ->
      let rec aux1 v l c_state =
        match l with
        | [] -> None
        | (r, comb)::tl -> match r prio p_state c_state with
          | None -> aux1 v tl c_state
          | Some (v', c_state') -> Some (comb v v', c_state')
      in
      let rec aux v c_state =
        match aux1 v casesL c_state with
        | Some (v', c_state) ->  aux v' c_state
        | None -> Some (v, c_state)
      in aux init c_state
      
let parsing_of_parsers (l : 'a parsers) (self : 'a parsing) : 'a parsing = fun p state s ->
  let rec aux p : 'a parsing list -> ('a * parsing_state) option = function
    | [] -> None
    | hd::tl -> match hd p state s with
      | None -> aux p tl
      | out -> out
  in
  let rec aux2 p =
    if p < 0
    then None
    else match l.(p) with
      | NoRule -> aux2 (p - 1)
      | BasicRules rules ->  
        begin match aux p rules with
          | None -> aux2 (p - 1)
          | out -> out
        end
      | LeftAssoc possibilities ->
        begin match fold_leftL self possibilities (p - 1) state s with
          | None -> aux2 (p - 1)
          | out -> out
        end
  in aux2 p

let match_char c : unit parsing = fun _ _ (str, pos) ->
  if pos >= String.length str || str.[pos] <> c
  then None
  else Some ((), (str, pos + 1))

let match_string s : unit parsing = fun _ _ (str, pos) ->
  if pos + String.length s >= String.length str
  then None
  else 
    let b = ref true in
    for i = 0 to String.length s - 1 do
      b := !b && s.[i] = str.[pos + i]
    done;
    if !b
    then Some ((), (str, pos + String.length s))
    else None

let combine_rules r1 r2 combinator p p_state c_state =
  match r1 p p_state c_state with
  | None -> None
  | Some (a1, c_state') -> match r2 p p_state c_state' with
    | None -> None
    | Some (a2, state'') -> Some (combinator a1 a2, state'')

let rec get_rules : type a. a parser_rule -> a parsing = function
  | Custom f -> f
  | Units -> fun p s -> parsing_of_parsers s.units (get_rules Units) p s
  | Ints  -> fun p s -> parsing_of_parsers s.ints  (get_rules Ints) p s

let match_int : int parsing = fun _ _ (s, pos) ->
  if String.length s <= pos || s.[pos] < '0' || s.[pos] > '9'
  then None
  else
    let rec aux pos n =
      if String.length s <= pos || s.[pos] < '0' || s.[pos] > '9'
      then Some (n, (s, pos))
      else aux (pos + 1) (n * 10 + int_of_char s.[pos] - int_of_char '0')
    in aux pos 0

let match_add : int parsing * (int -> int -> int) =
    (combine_rules (match_char '+') (get_rules Ints) (fun _ i -> i), fun i j -> i + j)

let match_sub : int parsing * (int -> int -> int) =
    (combine_rules (match_char '-') (get_rules Ints) (fun _ i -> i), fun i j -> i - j)

let match_mul : int parsing * (int -> int -> int) =
    (combine_rules (match_char '*') (get_rules Ints) (fun _ i -> i), fun i j -> i * j)

let match_div : int parsing * (int -> int -> int) =
  (combine_rules (match_char '/') (get_rules Ints) (fun _ i -> i), fun i j -> i / j)
            
let default = {
  units = Array.make (max_prio + 1) NoRule;
  ints =
    let array = Array.make (max_prio + 1) NoRule in
    let () = array.(0) <- BasicRules [match_int] in
    let () = array.(50) <- LeftAssoc [match_add; match_sub] in
    let () = array.(30) <- LeftAssoc [match_mul; match_div] in
    array
  ;
}


let parse (parser : parser_state) rule s =
  match get_rules rule max_prio parser (s, 0) with
  | Some (v, (_, p)) when p = String.length s -> Some v
  | _ -> None
