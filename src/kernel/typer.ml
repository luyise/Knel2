open Syntax
open Context
open Substitution
open Alpha
open Beta

exception TypeError
  of  position      (* position of the error *)
  *   term          (* term whose typing failed *)
  *   ((naked_term) option) (* expected type of the term *)

(* look for the type associated to a certain variable identifiant in a given context *)
let rec var_typ
  (beta_rules : beta_rule list) (* List of custom beta-reduction rules *)
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
            if Beta.is_alpha_beta_eq beta_rules id_pool id_typ id'_typ
            then id_typ
            else raise (NonExistingVariable(loc, id, typ_guess))
      end
  | _ :: ctx_tail -> var_typ beta_rules id_pool ctx_tail id loc typ_guess

(* compute te type of a term, in a given context *)
let rec typ
  (beta_rules : beta_rule list) (* List of custom beta-reduction rules *)
  (id_pool : ident list) (* already used identifiants *)
  (ctx : context) (* context used for typing *)
  (tm : term)     (* term being typed *)
  (* (typ_guess : term option) (* expected type, optional *) *)
  : term  (* type of the term in the provided context, fail if the type can't be derived *)
= try match tm.term with
    | Var (id, maybe_id_typ) ->
        var_typ beta_rules id_pool ctx id tm.loc maybe_id_typ
    | Lam (id, id_typ, body) ->
        let _ = typ beta_rules id_pool ctx id_typ in
        let body_typ =
          typ
            beta_rules
            (id :: id_pool)
            ((id, id_typ) :: ctx)
            body
        in
        {term = Pi (id, id_typ, body_typ); loc = tm.loc}
    | App (func, arg) ->
        let func_typ = typ beta_rules id_pool ctx func in
        let reduced_func_typ = beta_reduce beta_rules id_pool func_typ in
        begin match reduced_func_typ.term with
          | Pi(id', expected_arg_typ, body)
            -> let arg_typ = typ beta_rules id_pool ctx arg in
              if is_alpha_beta_eq beta_rules id_pool arg_typ expected_arg_typ then
                substitute id_pool id' arg body
              else
                raise (TypeError(tm.loc, tm, None))
          | _ -> raise (TypeError(tm.loc, tm, None))
        end
    | Pi (id, id_typ, body) ->
      let lvl1 = begin match 
        (typ 
          beta_rules 
          id_pool 
          ctx 
          id_typ).term
        with
          | UU(lvl1) -> lvl1
          | _ -> raise (TypeError(tm.loc, tm, None))
        end in
      let lvl2 = begin match
        (typ
          beta_rules
          (id :: id_pool)
          ((id, id_typ) :: ctx)
          body).term
        with
          | UU(lvl2) -> lvl2
          | _ -> raise (TypeError(tm.loc, tm, None))
        end in
      {term = UU (max lvl1 lvl2); loc = tm.loc}
    | Op tm ->
        typ beta_rules id_pool ctx tm
    | Const (id, maybe_id_typ) ->
        var_typ beta_rules id_pool ctx id tm.loc maybe_id_typ
    | UU lvl -> {term = UU (lvl+1); loc = tm.loc}
  with
    | TypeError ((filename, line, character), problematic_term, maybe_expected_type)
      -> 
        let buffer = Buffer.create 64 in
        let fmt = Format.formatter_of_buffer buffer in
        let _ = Format.fprintf fmt
          "\nType Error:\nIn %s, at line %i, character %i.\nWhen typing %a\n"
          filename
          line
          character
          pp_term problematic_term
        in
        let _ = begin match maybe_expected_type with
          | None -> ()
          | Some expected_type ->
              Format.fprintf fmt
                "which was expected of type %a"
                pp_nterm expected_type
          end in
        failwith (Buffer.contents buffer)