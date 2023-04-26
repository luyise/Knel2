
type parsing_state

type 'a parsing

type 'a parser_rule =
  | Custom : 'a parsing -> 'a parser_rule
  | Units : unit parser_rule
  | Ints : int parser_rule

type parser_state

val parse : parser_state -> 'a parser_rule -> string -> 'a option

val match_char : char -> unit parsing

val match_string : string -> unit parsing

val combine_rules : 'a parsing -> 'b parsing -> ('a -> 'b -> 'c) -> 'c parsing

val get_rules : 'a parser_rule -> 'a parsing

val default : parser_state

