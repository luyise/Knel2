
type parsing_state

type 'a parsing

type 'a parser_rule =
  | Custom : 'a parsing -> 'a parser_rule
  | Units : unit parser_rule
  | Ints : int parser_rule
  | Terms : Syntax.term parser_rule

type parser_state

val parse : parser_state -> 'a parser_rule -> string -> 'a option

val map : 'a parsing -> ('a -> 'b) -> 'b parsing

val get_loc : Syntax.position parsing

val match_alpha : char parsing

val match_alphas : string parsing

val match_wspace : unit parsing

val match_char : char -> unit parsing

val match_string : string -> unit parsing

val to_option : 'a parsing -> 'a option parsing

val and_operator : 'a parsing -> unit parsing

val not_operator : 'a parsing -> unit parsing

val combine_rules : 'a parsing -> 'b parsing -> ('a -> 'b -> 'c) -> 'c parsing

val get_rules : 'a parser_rule -> 'a parsing

val match_int : int parsing

val add_custom_rule : parser_state -> int -> 'a parser_rule -> 'a parsing -> unit

val add_left_assoc : parser_state -> int -> 'a parser_rule -> ('a -> 'a) parsing -> unit

val add_right_assoc : parser_state -> int -> 'a parser_rule -> ('a -> 'a) parsing -> unit

val gen_p_state : unit -> parser_state
