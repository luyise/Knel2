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

let rec pp_term fmt t = pp_nterm fmt (t.term)
and pp_nterm fmt = function
  | Var (v, _) -> Format.fprintf fmt "%s" v
  | Lam (i, t1, t2) -> Format.fprintf fmt "%s : %a -> %a" i pp_term t1 pp_term t2
  | App (t1, t2) -> Format.fprintf fmt "%a (%a)" pp_term t1 pp_term t2
  | Pi (i, t1, t2) -> Format.fprintf fmt "exists %s : %a, %a" i pp_term t1 pp_term t2
  | Op t -> Format.fprintf fmt "(%a)" pp_term t
  | Const (c, _) -> Format.fprintf fmt "%s" c

type context = (ident * term) list