open Syntax
open Context
open Substitution

(* Check if two terms are alpha-equivalent *)
let rec is_alpha_eq
  (id_pool : ident list)
  (tm1 : term) 
  (tm2 : term)
  : bool
= match tm1.term, tm2.term with

  | Var(id1, _), Var(id2, _) -> (id1 = id2)

  | Lam(id1, id1_typ, body1), Lam(id2, id2_typ, body2) when id1 = id2 ->
      is_alpha_eq id_pool id1_typ id2_typ
      && is_alpha_eq (id1 :: id_pool) body1 body2
  | Lam(id1, id1_typ, body1), Lam(id2, id2_typ, body2) ->
      is_alpha_eq id_pool id1_typ id2_typ
      && (
        let id3 = new_ident (id1 :: id2 :: id_pool) id1 in
        let body1' = rename (id1 :: id_pool) id1 id3 body1 in
        let body2' = rename (id2 :: id_pool) id2 id3 body2 in
        is_alpha_eq (id3 :: id_pool) body1' body2'
      )

  | App(func1, arg1), App(func2, arg2) ->
      (is_alpha_eq id_pool func1 func2)
      && (is_alpha_eq id_pool arg1 arg2)

  | Pi(id1, id1_typ, body1), Pi(id2, id2_typ, body2) when id1 = id2 ->
      is_alpha_eq id_pool id1_typ id2_typ
      && is_alpha_eq (id1 :: id_pool) body1 body2
  | Pi(id1, id1_typ, body1), Pi(id2, id2_typ, body2) ->
      is_alpha_eq id_pool id1_typ id2_typ
      && (
        let id3 = new_ident (id1 :: id2 :: id_pool) id1 in
        let body1' = rename (id1 :: id_pool) id1 id3 body1 in
        let body2' = rename (id2 :: id_pool) id2 id3 body2 in
        is_alpha_eq (id3 :: id_pool) body1' body2'
      )

  | Op(typ1), Op(typ2) ->
      is_alpha_eq id_pool typ1 typ2

  | Const(id1, _), Const(id2, _) -> (id1 = id2)

  | UU(k), UU(l) -> k = l

  | _ -> false