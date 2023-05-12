{
    open Parse_saving

    exception UnknownChar of char

}

let lower_case = ['a'-'z']
let upper_case = ['A'-'Z']
let digit = ['0'-'9']
let grec_lc = "α" | "β" | "γ" | "δ" | "ε" | "ζ" | "η" | "θ" | "ι" | "κ"(* λ *)| "μ" | "ν" | "ξ" | "ο" | "π" | "ρ" | "ς" | "σ" | "τ" | "υ" | "φ" | "χ" | "ψ" | "ω"
let grec_uc = "Α" | "Β" | "Γ" | "Δ" | "Ε" | "Ζ" | "Η" | "Θ" | "Ι" | "Κ" | "Λ" | "Μ" | "Ν" | "Ξ" | "Ο"  (*Π*)| "Ρ" | "Ϛ"  (*Σ*)| "Τ" | "Υ" | "Φ" | "Χ" | "Ψ" | "Ω"

let other_char = "∞"

let special = grec_lc | grec_uc | other_char

let integer = digit+

let letter = lower_case | upper_case | '_' | '\'' | special

let word_mid = (letter | digit) *

let word_end = '.' word_mid

let ident = letter word_mid word_end*

let comment_smp = "//" [^'\n']*

let str = [^'\"']*

let space = ' ' | '\t' | comment_smp

let integer = digit+

rule next_tokens = parse
    | space         { next_tokens lexbuf }
    | '\n'          { Lexing.new_line lexbuf; next_tokens lexbuf }
    | ident as id   { IDENT id }
    | integer as i  { INT (int_of_string i) }
    | '='           { EQ }
    | "{"           { LBRACKET }
    | "}"           { RBRACKET }
    | "["           { LSBRACKET }
    | "]"           { RSBRACKET }
    | ";"           { SEMICOLON }
    | "."           { DOT }
    | "@version"    { VERSION }
    | "@term"       { TERM_DECL }
    | "@ident"      { IDENT_DECL }
    | "@rules"      { RULES }
    | "\"" (str as s) "\"" { STRING s}
    | "@pi"         { PI }
    | "@lam"        { LAM }
    | "@app"        { APP }
    | "("           { LPAREN }
    | ")"           { RPAREN }
    | ","           { COMMA }
    | eof           { EOF }
    | _ as c        { raise (UnknownChar c) }

{
    let next_token =
        next_tokens
}