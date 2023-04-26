type position = string * int * int
  (* The position of a token in the code.
    The first argument is the file name,
    the second is the line,
    the third is the position of the first character of the token on the line.
  *)

type ident = string

type naked_term =
  | Var of ident * (term option)
  | Lam of ident * term * term
  | App of term * term
  | Pi of ident * term * term
  | Op of term
  | Const of ident * (term option)
and term = 
  { term : naked_term
  ; loc : position
  }

type context = (ident * term) list