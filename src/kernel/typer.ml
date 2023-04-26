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
  (id_pool : ident list)
  (init_id : ident)
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
  (id_pool : ident list)
  (id1 : ident)
  (id2 : ident)
  (tm : term)
  : term
= match tm.term with
  | Var(id, typ_op) when id = id1 -> 
      {tm with term = Var(id2, typ_op)}
  | Lam(id, id_typ, body) ->
      let body', id' =
        if id = id2 then
          let id' = new_ident (id2 :: id_pool) id in
          rename id_pool id id' body, id'
        else body, id
      in
      let body'' = rename (id' :: id_pool) id1 id2 body' in
      let id_typ' = rename id_pool id1 id2 id_typ in
      {tm with term = Lam(id', id_typ', body'')}
  | _ -> assert false

(* Check if two terms are alpha-equivalent *)
let rec is_alpha_eq
  (id_pool : ident list)
  (tm : term) 
  (tm' : term)
  : bool
= match tm.term, tm'.term with
  | Var(id, typ_op), Var(id', typ_op') -> (id = id')
  | Lam(id, id_typ, body), Lam(id', id'_typ, body') ->
      let new_body' = rename id_pool id id' body' in
      assert false
  | _ -> assert false

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
  | (id', id'_typ) :: ctx_tail when id' = id ->
      begin match typ_guess with
        | None -> id'_typ
        | Some id_typ ->
            if is_alpha_eq id_pool id_typ id'_typ
            then id_typ
            else raise (NonExistingVariable(loc, id, typ_guess))
      end
  | _ :: ctx_tail -> var_typ id_pool ctx_tail id loc typ_guess

(* compute te type of a term, in a given context *)
let typ
  (id_pool : ident list) (* already used identifiants *)
  (ctx : context) (* context used for typing *)
  (tm : term)     (* term being typed *)
  (typ_guess : term option) (* expected type, optional *)
  : term  (* type of the term in the provided context, fail if the type can't be derived *)
= match tm.term with
  | _ -> assert false