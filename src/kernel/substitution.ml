open Syntax
open Context

(* Rename an identifiant into another in a given term *)
let rec rename
  (id_pool : ident list)  (* List of already used identifiants in tm (that is the free variables of tm) *)
  (id1 : ident)           (* identifiant to replace by id2 (must be the name of a variable) *)
  (id2 : ident)           (* identifiant to replace id1  *)
  (tm : term)             (* term in which the replacement occurs *)
  : term
= match tm.term with

  | Var(id, typ_op) when id = id1 -> 
      {tm with term = Var(id2, typ_op)}
  | Var(_, _) -> tm

  | Lam(id, id_typ, body) when id = id1 ->
      let id_typ' = rename id_pool id1 id2 id_typ in
      {tm with term = Lam(id, id_typ', body)}
  | Lam(id, id_typ, body) when id = id2 ->
      let id_typ' = rename id_pool id1 id2 id_typ in
      let id' = new_ident (id1 :: id2 :: id_pool) id in
      let body' = rename id_pool id id' body in
      let body'' = rename (id' :: id_pool) id1 id2 body' in
      {tm with term = Lam(id', id_typ', body'')}
  | Lam (id, id_typ, body) ->
      let id_typ' = rename id_pool id1 id2 id_typ in
      let body' = rename (id :: id_pool) id1 id2 body in
      {tm with term = Lam(id, id_typ', body')}

  | App(func, arg) ->
      let func' = rename id_pool id1 id2 func in
      let arg' = rename id_pool id1 id2 arg in
      {tm with term = App(func', arg')}

  | Pi(id, id_typ, body) when id = id1 ->
      let id_typ' = rename id_pool id1 id2 id_typ in
      {tm with term = Pi(id, id_typ', body)}
  | Pi(id, id_typ, body) when id = id2 ->
      let id_typ' = rename id_pool id1 id2 id_typ in
      let id' = new_ident (id1 :: id2 :: id_pool) id in
      let body' = rename id_pool id id' body in
      let body'' = rename (id' :: id_pool) id1 id2 body' in
      {tm with term = Pi(id', id_typ', body'')}
  | Pi(id, id_typ, body) ->
      let id_typ' = rename id_pool id1 id2 id_typ in
      let body' = rename (id :: id_pool) id1 id2 body in
      {tm with term = Pi(id, id_typ', body')}

  | Op(typ) -> 
      let typ' = rename id_pool id1 id2 typ in
      {tm with term = Op(typ')}

  | Const(_, _) -> tm

  | UU(_) -> tm

(* Replace a variable into a term in a given term *)
let rec substitute
  (id_pool : ident list)  (* List of already used identifiants in tg and tm (that is the free variables of tm) *)
  (id_in : ident)           (* identifiant to replace by tm_out (must be the name of a variable) *)
  (tm_out : term)             (* term used to replace id_in *)         
  (tm : term)           (* term in which the replacement occurs *)
  : term
= match tm.term with

  | Var(id, _) when id = id_in -> 
      tm_out
  | Var(_, _) -> tm

  | Lam(id, id_typ, body) when id = id_in ->
      let id_typ' = substitute id_pool id_in tm_out id_typ in
      {tm with term = Lam(id, id_typ', body)}
  | Lam(id, id_typ, body) when List.mem id id_pool ->
      let id_typ' = substitute id_pool id_in tm_out id_typ in
      let id' = new_ident (id_in :: id_pool) id in
      let body' = rename id_pool id id' body in
      let body'' = substitute (id' :: id_pool) id_in tm_out body' in
      {tm with term = Lam(id', id_typ', body'')}
  | Lam (id, id_typ, body) ->
      let id_typ' = substitute id_pool id_in tm_out id_typ in
      let body' = substitute (id :: id_pool) id_in tm_out body in
      {tm with term = Lam(id, id_typ', body')}

  | App(func, arg) ->
      let func' = substitute id_pool id_in tm_out func in
      let arg' = substitute id_pool id_in tm_out arg in
      {tm with term = App(func', arg')}

  | Pi(id, id_typ, body) when id = id_in ->
      let id_typ' = substitute id_pool id_in tm_out id_typ in
      {tm with term = Pi(id, id_typ', body)}
  | Pi(id, id_typ, body) when List.mem id id_pool ->
      let id_typ' = substitute id_pool id_in tm_out id_typ in
      let id' = new_ident (id_in :: id_pool) id in
      let body' = rename id_pool id id' body in
      let body'' = substitute (id' :: id_pool) id_in tm_out body' in
      {tm with term = Pi(id', id_typ', body'')}
  | Pi(id, id_typ, body) ->
      let id_typ' = substitute id_pool id_in tm_out id_typ in
      let body' = substitute (id :: id_pool) id_in tm_out body in
      {tm with term = Pi(id, id_typ', body')}

  | Op(typ) -> 
      let typ' = substitute id_pool id_in tm_out typ in
      {tm with term = Op(typ')}

  | Const(_, _) -> tm

  | UU(_) -> tm