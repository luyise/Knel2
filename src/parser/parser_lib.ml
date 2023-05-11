
type pos = {
  total : int;
  line : int;
  char_line : int;
  }
  
type parsing_state = {
  str : string;
  file_name : string;
  pos : pos;
  pos_history : pos list;
  }
    
type 'a parsing_error =
  | Value of 'a * parsing_state
  | Err of Syntax.position

let finished p_state = String.length p_state.str <= p_state.pos.total

let get_char p_state = p_state.str.[p_state.pos.total]

let incr_pos p_state =
  let (line, char_line) = if get_char p_state = '\n'
  then (p_state.pos.line + 1, 0)
  else (p_state.pos.line, p_state.pos.char_line + 1) in
  {p_state with pos = {line; char_line; total = p_state.pos.total + 1 }}

let get_pos p_state = (p_state.file_name, p_state.pos.line, p_state.pos.char_line)

let max_prio = 100
(* type priority = int * int *)

module Make (Elt : sig type t end) = struct

  type 'a parser_rule =
    | Custom : 'a parsing -> 'a parser_rule
    | Default : Elt.t parser_rule
    | Terms : Syntax.term parser_rule
  and 'a parsing = int -> parser_state -> parsing_state -> 'a parsing_error
  and 'a rule =
    | NoRule
    | BasicRules of 'a parsing list
    | LeftAssoc of ('a -> 'a) parsing list
    | RightAssoc of ('a -> 'a) parsing list
  and 'a parsers = 'a rule array
  and parser_state = {
    default : Elt.t parsers;
    terms : Syntax.term parsers;
  }

  let custom v = Custom v
  let default = Default
  let terms = Terms

  let map (r : 'a parsing) (f : 'a -> 'b) : 'b parsing =
    fun prio p_state c_state ->
      match r prio p_state c_state with
      | Err p -> Err p
      | Value (v, c_state') -> Value (f v, c_state')

  let fold_left (r1 : 'a parsing) (b2 : 'b parsing) (f : 'a -> 'b -> 'a) : 'a parsing =
    fun prio p_state c_state ->
      match r1 prio p_state c_state with
      | Err p -> Err p
      | Value (init, c_state) ->
        let rec aux v c_state =
          match b2 prio p_state c_state with
          | Value (v', c_state) ->  aux (f v v') c_state
          | Err _ -> Value (v, c_state)
        in aux init c_state

  let fold_right (b2 : 'b parsing) (r1 : 'a parsing) (f : 'b -> 'a -> 'a) : 'a parsing =
    fun prio p_state c_state ->
      let rec aux c_state =
        match b2 prio p_state c_state with
        | Err _ -> r1 prio p_state c_state
        | Value (v, c_state') -> begin match aux c_state' with
          | Value (v', c_state) -> Value (f v v', c_state)
          | Err _ -> r1 prio p_state c_state
        end
      in aux c_state
      
  let fold_leftL (r1 : 'a parsing) (casesL : (('a -> 'a) parsing) list) : 'a parsing =
    fun prio p_state c_state ->
      match r1 prio p_state c_state with
      | Err p -> Err p
      | Value (init, c_state) ->
        let rec aux1 v l c_state =
          match l with
          | [] -> None
          | r::tl -> match r prio p_state c_state with
            | Err _ -> aux1 v tl c_state
            | Value (f, c_state') -> Some (f v, c_state')
        in
        let rec aux v c_state =
          match aux1 v casesL c_state with
          | Some (v', c_state) ->  aux v' c_state
          | None -> Value (v, c_state)
        in aux init c_state

  let fold_rightL (r1 : 'a parsing) (casesL : (('a -> 'a) parsing) list) : 'a parsing =
    fun prio p_state c_state ->
      let rec aux1 l c_state =
        match l with
        | [] -> None
        | r::tl -> match r prio p_state c_state with
          | Err _ -> aux1 tl c_state
          | Value (f, c_state) -> Some (f, c_state)
      in
      let rec aux c_state =
        match aux1 casesL c_state with
        | Some (f, c_state') ->
          begin match aux c_state' with
            | Err _ -> r1 prio p_state c_state
            | Value (v, c_state) -> Value (f v, c_state)
          end
        | None -> r1 prio p_state c_state
      in aux c_state
      
  let parsing_of_parsers (l : 'a parsers) (self : 'a parsing) : 'a parsing = fun p state c_state ->
    let rec aux p : 'a parsing list -> 'a parsing_error = function
      | [] -> Err (c_state.file_name, c_state.pos.line, c_state.pos.char_line)
      | hd::tl -> match hd p state c_state with
        | Err _ -> aux p tl
        | out -> out
    in
    let rec aux2 p =
      if p < 0
      then Err (c_state.file_name, c_state.pos.line, c_state.pos.char_line)
      else match l.(p) with
        | NoRule -> aux2 (p - 1)
        | BasicRules rules ->  
          begin match aux (p - 1) rules with
            | Err _ -> aux2 (p - 1)
            | out -> out
          end
        | LeftAssoc possibilities ->
          begin match fold_leftL self possibilities (p - 1) state c_state with
            | Err _ -> aux2 (p - 1)
            | out -> out
          end
        | RightAssoc possibilities ->
          begin match fold_rightL self possibilities (p - 1) state c_state with
            | Err _ -> aux2 (p - 1)
            | out -> out
          end
      in aux2 p

  let get_loc : Syntax.position parsing = fun _ _ c_state ->
    (* TODO correct position *)
    Value (get_pos c_state, c_state)

  let lift_prio : 'a parsing -> 'a parsing = fun r _ parser c_state ->
      r max_prio parser c_state 

  let match_alpha : char parsing = fun _ _ c_state ->
    if finished c_state
    then Err (c_state.file_name, c_state.pos.line, c_state.pos.char_line)
    else if ('a' <= get_char c_state && get_char c_state <= 'z')
        || ('A' <= get_char c_state && get_char c_state <= 'Z')
      then Value (get_char c_state, incr_pos c_state)
      else Err (c_state.file_name, c_state.pos.line, c_state.pos.char_line)

  let is_white_space (c : char) : bool =
      c = ' ' || c = '\t' || c = '\n'

  let rec match_wspace : unit parsing = fun prio p_state c_state ->
    if finished c_state || not (is_white_space (get_char c_state))
    then Value ((), c_state)
    else match_wspace prio p_state (incr_pos c_state)

  let match_wspace_ne : unit parsing = fun prio p_state c_state ->
      if finished c_state || not (is_white_space (get_char c_state))
      then Err (c_state.file_name, c_state.pos.line, c_state.pos.char_line)
      else match_wspace prio p_state (incr_pos c_state)

  let match_char c : unit parsing = fun _ _ c_state ->
    if finished c_state || get_char c_state <> c
    then Err (c_state.file_name, c_state.pos.line, c_state.pos.char_line)
    else Value ((), incr_pos c_state)

  let match_fchar (f : char ->  bool) : char parsing = fun _ _ c_state ->
    if finished c_state || not (f (get_char c_state))
    then Err (c_state.file_name, c_state.pos.line, c_state.pos.char_line)
    else Value (get_char c_state, incr_pos c_state)
            
  let match_string s : unit parsing = fun _ _ c_state ->
    if c_state.pos.total + String.length s > String.length c_state.str
    then Err (c_state.file_name, c_state.pos.line, c_state.pos.char_line)
    else 
      let b = ref true in
      for i = 0 to String.length s - 1 do
        b := !b && s.[i] = c_state.str.[c_state.pos.total + i]
      done;
      if !b
      then Value ((), {c_state with pos = {c_state.pos with total = c_state.pos.total + String.length s}}) (* TODO : correct incorrect line count *)
      else Err (c_state.file_name, c_state.pos.line, c_state.pos.char_line)

  let match_falphas (f : char -> bool): string parsing =
    (fold_left (map (match_fchar f) (String.make 1)) (map (match_fchar f) (String.make 1)) String.cat)

  let match_alphas 
    (* (type d) : (string, d) parsing *)
    =
      (fold_left (map match_alpha (String.make 1)) (map match_alpha (String.make 1)) String.cat)

  let to_option (r : 'a parsing) : 'a option parsing =
    fun p p_state c_state ->
      match r p p_state c_state with
      | Err _ -> Value (None, c_state)
      | Value (v, c_state) -> Value (Some v, c_state)

  let and_operator (r : 'a parsing) : unit parsing =
    fun p p_state c_state ->
        match r p p_state c_state with
        | Err pos -> Err pos
        | Value _ -> Value ((), c_state)

  let not_operator (r : 'a parsing) : unit parsing =
    fun p p_state c_state ->
        match r p p_state c_state with
        | Err _ -> Value ((), c_state)
        | Value _ -> Err (c_state.file_name, c_state.pos.line, c_state.pos.char_line)

  let seq_operator r1 r2 combinator p p_state c_state =
    match r1 p p_state c_state with
    | Err pos -> Err pos
    | Value (a1, c_state') -> match r2 p p_state c_state' with
      | Err pos -> Err pos
      | Value (a2, state'') -> Value (combinator a1 a2, state'')


  let or_operator r1 r2 p p_state c_state =
    match r1 p p_state c_state with
    | Err _ -> r2 p p_state c_state
    | out -> out


  let rec get_rules : type a. a parser_rule -> a parsing = function
    | Custom f -> f
    | Default -> fun p s -> parsing_of_parsers s.default  (get_rules Default) p s
    | Terms  -> fun p s -> parsing_of_parsers s.terms (get_rules Terms) p s

  let match_int : int parsing = fun _ _ c_state ->
    if finished c_state || get_char c_state < '0' || get_char c_state > '9'
    then Err (c_state.file_name, c_state.pos.line, c_state.pos.char_line)
    else
      let rec aux c_state n =
        if finished c_state || get_char c_state < '0' || get_char c_state > '9'
        then Value (n, c_state)
        else aux (incr_pos c_state) (n * 10 + int_of_char (get_char c_state) - int_of_char '0')
      in aux c_state 0

  let add_custom_rule (type a) (ps : parser_state) (prio : int) : a parser_rule -> a parsing -> unit = function
    | Custom _ -> failwith "NYI"
    | Default -> fun rule ->
      begin match ps.default.(prio) with
        | NoRule -> ps.default.(prio) <- BasicRules [rule]
        | BasicRules l -> ps.default.(prio) <- BasicRules (rule::l)
        | _ -> failwith "Already a left associative rule at this priority"
      end
    | Terms -> fun rule ->
      begin match ps.terms.(prio) with
        | NoRule -> ps.terms.(prio) <- BasicRules [rule]
        | BasicRules l -> ps.terms.(prio) <- BasicRules (rule::l)
        | _ -> failwith "Already a left associative rule at this priority"
      end

  let add_left_assoc (type a) (ps : parser_state) (prio : int) : a parser_rule -> (a -> a) parsing -> unit = function
    | Custom _ -> failwith "NYI"
    | Default -> fun rule ->
      begin match ps.default.(prio) with
        | NoRule -> ps.default.(prio) <- LeftAssoc [rule]
        | LeftAssoc l -> ps.default.(prio) <- LeftAssoc (rule::l)
        | _ -> failwith "Already a non-left associative rule at this priority"
      end
    | Terms -> fun rule ->
      begin match ps.terms.(prio) with
        | NoRule -> ps.terms.(prio) <- LeftAssoc [rule]
        | LeftAssoc l -> ps.terms.(prio) <- LeftAssoc (rule::l)
        | _ -> failwith "Already a non-left associative rule at this priority"
      end

  let add_right_assoc (type a) (ps : parser_state) (prio : int) : a parser_rule -> (a -> a) parsing -> unit = function
    | Custom _ -> failwith "NYI"
    | Default -> fun rule ->
      begin match ps.default.(prio) with
        | NoRule -> ps.default.(prio) <- RightAssoc [rule]
        | RightAssoc l -> ps.default.(prio) <- RightAssoc (rule::l)
        | _ -> failwith "Already a non-right associative rule at this priority"
      end
    | Terms -> fun rule ->
      begin match ps.terms.(prio) with
        | NoRule -> ps.terms.(prio) <- RightAssoc [rule]
        | RightAssoc l -> ps.terms.(prio) <- RightAssoc (rule::l)
        | _ -> failwith "Already a non-right associative rule at this priority"
      end
        
  let gen_p_state () = {
    default = Array.make (max_prio + 1) NoRule;
    terms = Array.make (max_prio + 1) NoRule;
  }

  let create_state str file_name =
    let pos = { total = 0; line = 0; char_line = 0} in
      { str; pos; file_name; pos_history = [pos] }

  let check_finished (parser : parsing_state) : bool =
    parser.pos.total >= String.length parser.str

  let check_finished_with_ws (parser : parsing_state) : bool =
    try
      for i = parser.pos.total to String.length parser.str - 1 do
        if not (is_white_space parser.str.[i])
        then failwith "not a white space";
      done;
      true
    with Failure _ -> false
  
  let parse_single (parser : parser_state) rule c_state =
    match get_rules rule max_prio parser c_state with
      | Value (v, c_state) -> Value (v, {c_state with pos_history = c_state.pos :: c_state.pos_history })
      | Err p -> Err p

  let restart (c_state : parsing_state) =
    let pos = {total = 0; line = 0; char_line = 0} in
    {c_state with pos_history = [pos]; pos}

  let change_str (c_state : parsing_state) str : parsing_state * int =
    let last_same = ref 0 in
    let min_length = min (String.length str) (String.length c_state.str) in
    while !last_same < min_length && str.[!last_same] == c_state.str.[!last_same] do
      incr last_same
    done;
    let rec aux = function
      | [] -> assert false
      | hd::tl when hd.total <= !last_same -> (0, hd, hd::tl)
      | _::tl -> let (n, hd', tl') = aux tl in (n + 1, hd', tl')
    in
    let (n, pos, pos_history) = aux c_state.pos_history in
    ({c_state with pos; pos_history; str}, n)

  let parse_full (parser : parser_state) rule str =
    match get_rules rule max_prio parser {str; pos = {line = 0; total = 0; char_line = 0}; file_name = "no-name"; pos_history = []} with
    | Value (v, c_state) when finished c_state -> Some v
    | _ -> None

  let parse_full_raw (parser : parser_state) rule str =
    match rule max_prio parser {str; pos = {line = 0; total = 0; char_line = 0}; file_name = "no-name"; pos_history = []} with
    | Value (v, c_state) when finished c_state -> Some v
    | _ -> None
end