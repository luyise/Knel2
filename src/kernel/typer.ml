open Syntax

exception TypeError
  of  position      (* position of the error *)
  *   term          (* term whose typing failed *)
  *   (term option) (* expected type of the term *)

exception NonExistingVariable
  of  position      (* position of the variable call *)
  *   ident         (* identifiant of the variable *)
  *   (term option) (* expected type of the variable *)

(* Replace an identifiant by an unused one *)
let new_ident
  (id_pool : ident list)  (* List of already used identifiants *)
  (init_id : ident)       (* Identifiant to replace with a fresh one *)
  : ident
= let rec find_aux (n : int) =
    let id' = init_id ^ string_of_int(n) in
    if List.mem id' id_pool then
      find_aux (n+1)
    else id'
  in
  if List.mem init_id id_pool then 
    init_id 
  else find_aux 2

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

  | _ -> false

(* TODO : Finir, en ajoutant la réduction classique/ les réductions custom *)
(* beta-reduce a term, n times *)
let rec beta_reduce_n
  (id_pool : ident list) (* already used identifiants in tm *)
  (tm : term)            (* term to beta-reduce *)
  (n : int)              (* number of beta-reduction to perform, when n=-1, as much as possible beta reductions are performed *)
  : term * int           (* beta-reduced term, with the number of performed reductions *)
= if n = 0 then (tm, 0)
  else begin match tm.term with

    | Var(_, None) -> (tm, 0)
    | Var(id, Some typ) ->
        let (typ', m) = beta_reduce_n id_pool typ n in
        ({tm with term = Var(id, Some typ')}, m)

    | Lam(id, id_typ, body) ->
        let id_typ', k = beta_reduce_n id_pool id_typ n in
        let body', l = beta_reduce_n (id :: id_pool) body (n-k) in
        ({tm with term = Lam(id, id_typ', body')}, k+l)
    
    | App(func, arg) ->
        let func', k = beta_reduce_n id_pool func n in
        let arg', l = beta_reduce_n id_pool arg (n-k) in
        ({tm with term = App(func', arg')}, k+l)

    | Pi(id, id_typ, body) ->
        let id_typ', k = beta_reduce_n id_pool id_typ n in
        let body', l = beta_reduce_n (id :: id_pool) body (n-k) in
        ({tm with term = Pi(id, id_typ', body')}, k+l)

    | Op(typ) ->
        let typ', m = beta_reduce_n id_pool typ n in
        ({tm with term = Op(typ')}, m)

    | Const(_, None) -> (tm, 0)
    | Const(id, Some typ) ->
        let (typ', m) = beta_reduce_n id_pool typ n in
        ({tm with term = Const(id, Some typ')}, m)

  end

(* beta-reduce a term *)
let beta_reduce
  (id_pool : ident list) (* already used identifiants in tm *)
  (tm : term)            (* term to beta-reduce *)
  : term                 (* beta-reduced term *)
= fst (beta_reduce_n id_pool tm (-1))

(* check whether tm1 is alpha-beta-equivalent to tm2*)
let is_alpha_beta_eq
  (id_pool : ident list)  (* already used identifiants *)
  (tm1 : term)            (* first term *)
  (tm2 : term)            (* second term *)
  : bool
= let tm1' = beta_reduce id_pool tm1 in
  let tm2' = beta_reduce id_pool tm2 in
  is_alpha_eq id_pool tm1' tm2'

(* look for the type associated to a certain variable identifiant in a given context *)
let rec var_typ
  (id_pool : ident list) (* already used identifiants *)
  (ctx : context) (* context in which the type is seeked *)
  (id : ident)    (* identifiant of the variable *)
  (loc : position)  (* position of the variable call, used for errors *)
  (typ_guess : term option) (* expected type, optional *)
  : term  (* type found in the context *)
= match ctx with
  | [] -> raise (NonExistingVariable(loc, id, typ_guess))
  | (id', id'_typ) :: _ when id' = id ->
      begin match typ_guess with
        | None -> id'_typ
        | Some id_typ ->
            if is_alpha_beta_eq id_pool id_typ id'_typ
            then id_typ
            else raise (NonExistingVariable(loc, id, typ_guess))
      end
  | _ :: ctx_tail -> var_typ id_pool ctx_tail id loc typ_guess

(* compute te type of a term, in a given context *)
let typ
  (_id_pool : ident list) (* already used identifiants *)
  (_ctx : context) (* context used for typing *)
  (_tm : term)     (* term being typed *)
  (_typ_guess : term option) (* expected type, optional *)
  : term  (* type of the term in the provided context, fail if the type can't be derived *)
= match _tm.term with
  | _ -> assert false