
type parsing_state = {
  str : string;
  file_name : string;
  pos : int;
  line : int;
  char_line : int;
}

let finished p_state = String.length p_state.str <= p_state.pos

let get_char p_state = p_state.str.[p_state.pos]

let incr_pos p_state =
  let (line, char_line) = if get_char p_state = '\n'
  then (p_state.line + 1, 0)
  else (p_state.line, p_state.char_line + 1) in
  {p_state with line; char_line; pos = p_state.pos + 1 }

let get_pos p_state = (p_state.file_name, p_state.line, p_state.char_line)

let max_prio = 100
(* type priority = int * int *)

type 'a parser_rule =
  | Custom : 'a parsing -> 'a parser_rule
  | Units : unit parser_rule
  | Ints : int parser_rule
  | Terms : Syntax.term parser_rule
and 'a parsing = int -> parser_state -> parsing_state -> ('a * parsing_state) option
and 'a rule =
  | NoRule
  | BasicRules : 'a parsing list -> 'a rule
  | LeftAssoc : ('a -> 'a) parsing list -> 'a rule
  | RightAssoc : ('a -> 'a) parsing list -> 'a rule
and 'a parsers = 'a rule array
and parser_state = {
  units : unit parsers;
  ints : int parsers;
  terms : Syntax.term parsers;
}

let map (r : 'a parsing) (f : 'a -> 'b) : 'b parsing =
  fun prio p_state c_state ->
    match r prio p_state c_state with
    | None -> None
    | Some (v, c_state') -> Some (f v, c_state')

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

let fold_leftL (r1 : 'a parsing) (casesL : (('a -> 'a) parsing) list) : 'a parsing =
  fun prio p_state c_state ->
    match r1 prio p_state c_state with
    | None -> None
    | Some (init, c_state) ->
      let rec aux1 v l c_state =
        match l with
        | [] -> None
        | r::tl -> match r prio p_state c_state with
          | None -> aux1 v tl c_state
          | Some (f, c_state') -> Some (f v, c_state')
      in
      let rec aux v c_state =
        match aux1 v casesL c_state with
        | Some (v', c_state) ->  aux v' c_state
        | None -> Some (v, c_state)
      in aux init c_state

let fold_leftR (r1 : 'a parsing) (casesL : (('a -> 'a) parsing) list) : 'a parsing =
  fun prio p_state c_state ->
    let rec aux1 l c_state =
      match l with
      | [] -> None
      | r::tl -> match r prio p_state c_state with
        | None -> aux1 tl c_state
        | out -> out
    in
    let rec aux c_state =
      match aux1 casesL c_state with
      | Some (f, c_state') ->
        begin match aux c_state' with
          | None -> r1 prio p_state c_state
          | Some (v, c_state) -> Some (f v, c_state)
        end
      | None -> r1 prio p_state c_state
    in aux c_state
      
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
        begin match aux (p - 1) rules with
          | None -> aux2 (p - 1)
          | out -> out
        end
      | LeftAssoc possibilities ->
        begin match fold_leftL self possibilities (p - 1) state s with
          | None -> aux2 (p - 1)
          | out -> out
        end
      | RightAssoc possibilities ->
        begin match fold_leftR self possibilities (p - 1) state s with
          | None -> aux2 (p - 1)
          | out -> out
        end
    in aux2 p

let get_loc : Syntax.position parsing = fun _ _ c_state ->
  (* TODO correct position *)
  Some (get_pos c_state, c_state)

let match_alpha : char parsing = fun _ _ c_state ->
  if finished c_state
  then None
  else if ('a' <= get_char c_state && get_char c_state <= 'z')
      || ('A' <= get_char c_state && get_char c_state <= 'Z')
    then Some (get_char c_state, incr_pos c_state)
    else None

let rec match_wspace : unit parsing = fun prio p_state c_state ->
  if finished c_state || (get_char c_state <> ' ' && get_char c_state <> '\t' && get_char c_state <> '\n')
  then Some ((), c_state)
  else match_wspace prio p_state (incr_pos c_state)

let match_alphas : string parsing =
    (fold_left (map match_alpha (String.make 1)) (map match_alpha (String.make 1)) String.cat)

let match_char c : unit parsing = fun _ _ c_state ->
  if finished c_state || get_char c_state <> c
  then None
  else Some ((), incr_pos c_state)

let match_string s : unit parsing = fun _ _ c_state ->
  if c_state.pos + String.length s >= String.length c_state.str
  then None
  else 
    let b = ref true in
    for i = 0 to String.length s - 1 do
      b := !b && s.[i] = c_state.str.[c_state.pos + i]
    done;
    if !b
    then Some ((), {c_state with pos = c_state.pos + String.length s}) (* TODO : correct incorrect line count *)
    else None

let to_option (r : 'a parsing) : 'a option parsing =
  fun p p_state c_state ->
    match r p p_state c_state with
    | None -> Some (None, c_state)
    | Some (v, c_state) -> Some (Some v, c_state)

let and_operator (r : 'a parsing) : unit parsing =
  fun p p_state c_state ->
      match r p p_state c_state with
      | None -> None
      | Some _ -> Some ((), c_state)

let not_operator (r : 'a parsing) : unit parsing =
  fun p p_state c_state ->
      match r p p_state c_state with
      | None -> Some ((), c_state)
      | Some _ -> None
      
let combine_rules r1 r2 combinator p p_state c_state =
  match r1 p p_state c_state with
  | None -> None
  | Some (a1, c_state') -> match r2 p p_state c_state' with
    | None -> None
    | Some (a2, state'') -> Some (combinator a1 a2, state'')

let rec get_rules : type a. a parser_rule -> a parsing = function
  | Custom f -> f
  | Units -> fun p s -> parsing_of_parsers s.units  (get_rules Units) p s
  | Ints  -> fun p s -> parsing_of_parsers s.ints   (get_rules Ints) p s
  | Terms  -> fun p s -> parsing_of_parsers s.terms (get_rules Terms) p s

let match_int : int parsing = fun _ _ c_state ->
  if finished c_state || get_char c_state < '0' || get_char c_state > '9'
  then None
  else
    let rec aux c_state n =
      if finished c_state || get_char c_state < '0' || get_char c_state > '9'
      then Some (n, c_state)
      else aux (incr_pos c_state) (n * 10 + int_of_char (get_char c_state) - int_of_char '0')
    in aux c_state 0

let add_custom_rule (type a) (ps : parser_state) (prio : int) : a parser_rule -> a parsing -> unit = function
  | Custom _ -> failwith "NYI"
  | Ints -> fun rule ->
    begin match ps.ints.(prio) with
      | NoRule -> ps.ints.(prio) <- BasicRules [rule]
      | BasicRules l -> ps.ints.(prio) <- BasicRules (rule::l)
      | _ -> failwith "Already a left associative rule at this priority"
    end
  | Units -> fun rule ->
    begin match ps.units.(prio) with
      | NoRule -> ps.units.(prio) <- BasicRules [rule]
      | BasicRules l -> ps.units.(prio) <- BasicRules (rule::l)
      | _ -> failwith "Already a left associative rule at this priority"
    end
  | Terms -> fun rule ->
    begin match ps.terms.(prio) with
      | NoRule -> ps.terms.(prio) <- BasicRules [rule]
      | BasicRules l -> ps.terms.(prio) <- BasicRules (rule::l)
      | _ -> failwith "Already a left associative rule at this priority"
    end

let add_left_assoc (type a) (ps : parser_state) (prio : int) : a parser_rule -> (a->a) parsing -> unit = function
  | Custom _ -> failwith "NYI"
  | Ints -> fun rule ->
    begin match ps.ints.(prio) with
      | NoRule -> ps.ints.(prio) <- LeftAssoc [rule]
      | LeftAssoc l -> ps.ints.(prio) <- LeftAssoc (rule::l)
      | _ -> failwith "Already a non-left associative rule at this priority"
    end
  | Units -> fun rule ->
    begin match ps.units.(prio) with
      | NoRule -> ps.units.(prio) <- LeftAssoc [rule]
      | LeftAssoc l -> ps.units.(prio) <- LeftAssoc (rule::l)
      | _ -> failwith "Already a non-left associative rule at this priority"
    end
  | Terms -> fun rule ->
    begin match ps.terms.(prio) with
      | NoRule -> ps.terms.(prio) <- LeftAssoc [rule]
      | LeftAssoc l -> ps.terms.(prio) <- LeftAssoc (rule::l)
      | _ -> failwith "Already a non-left associative rule at this priority"
    end

let add_right_assoc (type a) (ps : parser_state) (prio : int) : a parser_rule -> (a->a) parsing -> unit = function
  | Custom _ -> failwith "NYI"
  | Ints -> fun rule ->
    begin match ps.ints.(prio) with
      | NoRule -> ps.ints.(prio) <- RightAssoc [rule]
      | RightAssoc l -> ps.ints.(prio) <- RightAssoc (rule::l)
      | _ -> failwith "Already a non-left associative rule at this priority"
    end
  | Units -> fun rule ->
    begin match ps.units.(prio) with
      | NoRule -> ps.units.(prio) <- RightAssoc [rule]
      | RightAssoc l -> ps.units.(prio) <- RightAssoc (rule::l)
      | _ -> failwith "Already a non-right associative rule at this priority"
    end
  | Terms -> fun rule ->
    begin match ps.terms.(prio) with
      | NoRule -> ps.terms.(prio) <- RightAssoc [rule]
      | RightAssoc l -> ps.terms.(prio) <- RightAssoc (rule::l)
      | _ -> failwith "Already a non-right associative rule at this priority"
    end
      
let gen_p_state () = {
  units = Array.make (max_prio + 1) NoRule;
  ints = Array.make (max_prio + 1) NoRule;
  terms = Array.make (max_prio + 1) NoRule;
}


let parse (parser : parser_state) rule str =
  match get_rules rule max_prio parser {str; line = 0; pos = 0; char_line = 0; file_name = "no-name"} with
  | Some (v, c_state) when finished c_state -> Some v
  | _ -> None
