type parsing_state

val get_pos : parsing_state -> Syntax.position

module Make : functor (Elt : sig type t end) ->
  sig
    type 'a parsing

    type 'a parser_rule

    val custom : 'a parsing -> 'a parser_rule
    val default : Elt.t parser_rule
    val terms : Syntax.term parser_rule
    
    type parser_state

    val map : 'a parsing -> ('a -> 'b) -> 'b parsing
    val fold_left :
      'a parsing -> 'b parsing -> ('a -> 'b -> 'a) -> 'a parsing
    val fold_right :
      'b parsing -> 'a parsing -> ('b -> 'a -> 'a) -> 'a parsing
    val fold_leftL : 'a parsing -> ('a -> 'a) parsing list -> 'a parsing
    val fold_rightL : 'a parsing -> ('a -> 'a) parsing list -> 'a parsing

    val get_loc : Syntax.position parsing
    val lift_prio : 'a parsing -> 'a parsing
    val match_alpha : char parsing
    val match_wspace : unit parsing
    val match_wspace_ne : unit parsing
    val match_char : char -> unit parsing
    val match_fchar : (char -> bool) -> char parsing
    val match_string : string -> unit parsing
    val match_falphas : (char -> bool) -> string parsing
    val match_alphas : string parsing
    val to_option : 'a parsing -> 'a option parsing
    val and_operator : 'a parsing -> unit parsing
    val not_operator : 'a parsing -> unit parsing
    val seq_operator : 'a parsing -> 'b parsing -> ('a -> 'b -> 'c) -> 'c parsing
    val or_operator : 'a parsing -> 'a parsing -> 'a parsing
    val get_rules : 'a parser_rule -> 'a parsing
    val match_int : int parsing
    val add_custom_rule :
      parser_state -> int -> 'a parser_rule -> 'a parsing -> unit
    val add_left_assoc :
      parser_state -> int -> 'a parser_rule -> ('a -> 'a) parsing -> unit
    val add_right_assoc :
      parser_state -> int -> 'a parser_rule -> ('a -> 'a) parsing -> unit
    val gen_p_state : unit -> parser_state

    val check_finished : parsing_state -> bool

    val parse_single : parser_state -> 'a parser_rule -> parsing_state -> ('a * parsing_state) option 
    val restart : parsing_state -> parsing_state

    val parse_full : parser_state -> 'a parser_rule -> string -> 'a option
    val parse_full_raw :
      parser_state ->
      'a parsing ->
      string -> 'a option
  end