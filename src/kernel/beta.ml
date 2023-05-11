open Syntax
open Substitution
open Alpha

type beta_rule = (ident list -> term -> term * bool)

let (beta_redex : beta_rule)
= fun
  (id_pool : ident list)
  (tm : term)
->
  match tm.term with
    | App( {term = Lam(id, _, body); loc = _}, arg ) ->
        ( substitute (id :: id_pool) id arg body
          ,
          true
        )
    | _ -> tm, false

let (op_redex : beta_rule)
= fun
  (_id_pool : ident list)
  (tm : term)
->
match tm.term with
  | Op( {term = Op(tm'); loc = _} ) ->
      ( tm'
        ,
        true
      )
  | _ -> tm, false

let std_rules : beta_rule list = [beta_redex; op_redex]

(* Apply the first possible reduction in the list of rules given *)
let rec apply_custom_red
  (beta_rules : beta_rule list) (* List of custom beta-reduction rules *)
  (id_pool : ident list) (* already used identifiants in tm *)
  (tm : term) (* term to beta-reduce *)
  : term * bool (* return the maybe reduced term with true if a reduction occured, or false in the other case *)
=
  match beta_rules with
    | [] -> tm, false
    | rule :: rules_tail ->
      let (tm', rule_applied) = rule id_pool tm in
      if rule_applied then tm', true
      else apply_custom_red rules_tail id_pool tm'

(* TODO : Finir, en ajoutant la réduction classique/ les réductions custom *)
(* beta-reduce a term, n times *)
let rec beta_reduce_n
  (beta_rules : beta_rule list) (* List of custom beta-reduction rules *)
  (id_pool : ident list) (* already used identifiants in tm *)
  (tm : term)            (* term to beta-reduce *)
  (n : int)              (* number of beta-reduction to perform, when n=-1, as much as possible beta reductions are performed *)
  : term * int           (* beta-reduced term, with the number of performed reductions *)
= if n = 0 then (tm, 0)
  else let (reduced_term, number_of_reduction) = begin match tm.term with

    | Var(_, None) -> (tm, 0)
    | Var(id, Some typ) ->
        let (typ', m) = beta_reduce_n beta_rules id_pool typ n in
        ({tm with term = Var(id, Some typ')}, m)

    | Lam(id, id_typ, body) ->
        let id_typ', k = beta_reduce_n beta_rules id_pool id_typ n in
        let body', l = beta_reduce_n beta_rules (id :: id_pool) body (n-k) in
        ({tm with term = Lam(id, id_typ', body')}, k+l)
    
    | App(func, arg) ->
        let func', k = beta_reduce_n beta_rules id_pool func n in
        let arg', l = beta_reduce_n beta_rules id_pool arg (n-k) in
        ({tm with term = App(func', arg')}, k+l)

    | Pi(id, id_typ, body) ->
        let id_typ', k = beta_reduce_n beta_rules id_pool id_typ n in
        let body', l = beta_reduce_n beta_rules (id :: id_pool) body (n-k) in
        ({tm with term = Pi(id, id_typ', body')}, k+l)

    | Op(typ) ->
        let typ', m = beta_reduce_n beta_rules id_pool typ n in
        ({tm with term = Op(typ')}, m)

    | Const(_, None) -> (tm, 0)
    | Const(id, Some typ) ->
        let (typ', m) = beta_reduce_n beta_rules id_pool typ n in
        ({tm with term = Const(id, Some typ')}, m)

    | UU(_) -> (tm, 0)

  end in
  (* attempt reducing using custom rules, and return the number of reduction that occured *)
  let rec custom_beta_reduce (tm : term) (n : int) : term * int =
    if n = 0 then (tm, 0)
    else
      let (tm', rule_applied) = apply_custom_red beta_rules id_pool tm in
      if rule_applied then 
        let (tm'', m) = custom_beta_reduce tm' (n-1) in
        (tm'', m+1)
      else (tm', 0)
  in
  let custom_reduced_term, number_of_custom_reduction =
    custom_beta_reduce reduced_term (n-number_of_reduction) 
  in
  custom_reduced_term,
  number_of_reduction + number_of_custom_reduction

(* beta-reduce a term *)
let beta_reduce
  (beta_rules : beta_rule list) (* List of custom beta-reduction rules *)
  (id_pool : ident list) (* already used identifiants in tm *)
  (tm : term)            (* term to beta-reduce *)
  : term                 (* beta-reduced term *)
= fst (beta_reduce_n beta_rules id_pool tm (-1))

(* check whether tm1 is alpha-beta-equivalent to tm2*)
let is_alpha_beta_eq
  (beta_rules : beta_rule list) (* List of custom beta-reduction rules *)
  (id_pool : ident list)  (* already used identifiants *)
  (tm1 : term)            (* first term *)
  (tm2 : term)            (* second term *)
  : bool
= let tm1' = beta_reduce beta_rules id_pool tm1 in
  let tm2' = beta_reduce beta_rules id_pool tm2 in
  is_alpha_eq id_pool tm1' tm2'