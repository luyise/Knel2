open Parser
open Syntax

let int_of_assoc = function
  | LeftAssoc -> 0
  | RightAssoc -> 1
  | NoAssoc -> 2

let assoc_of_int = function
  | 0 -> LeftAssoc
  | 1 -> RightAssoc
  | 2 -> NoAssoc
  | _ -> assert false

let pp_assoc fmt assoc =
  Format.fprintf fmt "%d" (int_of_assoc assoc)

let pp_term_decl fmt = function
  | Symbol str -> Format.fprintf fmt "\"%s\"" str
  | Ident id -> Format.fprintf fmt "@ident %s" id
  | Term id -> Format.fprintf fmt "@term %s" id

let pp_list printer fmt l =
  let rec aux fmt = function
    | [] -> ()
    | [hd] -> Format.fprintf fmt "%a" printer hd
    | hd::tl -> Format.fprintf fmt "%a;%a" printer hd aux tl
  in Format.fprintf fmt "[%a]" aux l

let pp_int fmt i = Format.fprintf fmt "%d" i

let pp_ids fmt l =
  let rec aux fmt = function
    | [] -> ()
    | hd::tl -> Format.fprintf fmt "%s %a" hd aux tl
  in Format.fprintf fmt "[%a]" aux l

let rec pp_term fmt (e : Syntax.term) =
  match e.term with
  | Var (v, None) -> Format.fprintf fmt "%s" v
  | Var (_, Some _) -> assert false
  | App (e1, e2) -> Format.fprintf fmt "@app(%a,%a)" pp_term e1 pp_term e2
  | Lam (i, e1, e2) -> Format.fprintf fmt "@lam(%s,%a,%a)" i pp_term e1 pp_term e2
  | Pi (i, e1, e2) -> Format.fprintf fmt "@pi(%s,%a,%a)" i pp_term e1 pp_term e2
  | _ -> assert false

let pp_decl_notation fmt (decl, prio, assoc, id, tm) =
  Format.fprintf fmt "(%a,%a,%a,%a,%a)"
    (pp_list pp_term_decl) decl
    pp_int prio
    pp_assoc assoc
    pp_ids (ISet.elements id)
    pp_term tm

let pp_decl_notations fmt l =
  Format.fprintf fmt "@rules = %a" (pp_list pp_decl_notation) l

let pp_version fmt () =
  let (v1, v2, v3) = Config.version in
  Format.fprintf fmt "@version = %d.%d.%d" v1 v2 v3

let pp_file fmt file =
  Format.fprintf fmt "{ %a ; %a }"
    pp_version ()
    pp_decl_notations file
