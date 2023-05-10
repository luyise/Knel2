open Syntax


type term_decl =
  | Symbol of string
  | Ident of ident
  | Term of ident

type associativity =
  | LeftAssoc
  | RightAssoc
  | NoAssoc

type declaration =
  | Notation of term_decl list * int * associativity * ident list * term

module IMap = Map.Make(String)
module ISet = Set.Make(String)
module P = Parser_lib.Make(struct type t = declaration end)
open P

let p_state = gen_p_state ()

let id_decl : term_decl parsing =
  or_operator
    (* (or_operator *)
      (map match_alphas (fun i -> Term i))
      (* (seq_operator (match_char '@') match_alphas (fun _ i -> Ident i))) *)
    (seq_operator
      (seq_operator (match_char '\'') (match_falphas (fun c -> c <> '\'')) (fun _ s -> Symbol s))
      (match_char '\'')
      (fun t _ -> t))

let match_var =
    seq_operator (seq_operator get_loc match_alphas (fun loc v -> (loc, v))) get_loc (fun (loc_start, t) _loc_end -> {term = Var (t, None); loc = loc_start})

let parsing_of_tdecl = function
  | Symbol str -> map (match_string str) (fun _ l -> l)
  | Ident id -> seq_operator match_alphas get_loc (fun t loc l -> (id, {term = Var (t, None); loc})::l)
  | Term id -> map (get_rules terms) (fun t l -> (id,t)::l)

let id_decls : term_decl list parsing =
  fold_right (seq_operator id_decl match_wspace_ne (fun i _ -> i)) (map id_decl (fun t -> [t])) List.cons

let rec handle_ident (iset : ISet.t) = function
  | [] -> []
  | Term t::tl when ISet.mem t iset -> Ident t::handle_ident iset tl
  | t::tl -> t::handle_ident iset tl

let ws_before : 'a parsing -> 'a parsing = fun r ->
  seq_operator match_wspace r (fun _ i -> i)

let ws_before_if b p =
  if b
  then seq_operator match_wspace p (fun _ i -> i)
  else p

let rec rule_decl : term_decl list -> term IMap.t parsing = function
  | [] -> map get_loc (fun _ -> IMap.empty)
  | Term hd::tl -> seq_operator (get_rules terms) (ws_before_if (tl <> []) (rule_decl tl)) (fun t tl -> IMap.add hd t tl)
  | Ident hd::tl -> seq_operator match_var (ws_before_if (tl <> []) (rule_decl tl)) (fun t tl -> IMap.add hd t tl)
  | Symbol str::tl -> seq_operator (match_string str) (ws_before_if (tl <> []) (rule_decl tl)) (fun _ tl -> tl)

let rec extract_last = function
  | [] -> assert false
  | hd::[] -> (hd, [])
  | hd::tl -> let (last, tl') = extract_last tl in (last, hd::tl')

let build_fold_left_inner (t : term_decl list) : (term -> term IMap.t) parsing =
  match t with
    | [Term _] -> assert false
    | Term hd::tl -> seq_operator match_wspace (rule_decl tl) (fun _ tl t -> IMap.add hd t tl)
    | _ -> assert false

let build_fold_right_inner (t : term_decl list) : (term -> term IMap.t) parsing =
  match extract_last t with
    | (Term _, []) -> assert false
    | (Term hd, tl) -> seq_operator (rule_decl tl) match_wspace (fun tl _ t -> IMap.add hd t tl)
    | _ -> assert false

let rec subst (map : term IMap.t) (t : term) : term = match t.term with
  | Var (v, _) ->
    begin match IMap.find_opt v map with
    | None -> t
    | Some t -> t
    end
  | App (t1, t2) -> {t with term = App (subst map t1, subst map t2)}
  | Lam (id, t1, t2) ->
    begin match IMap.find_opt id map with
      | Some {term = Var (v, _); _} -> {t with term = Lam (v, subst map t1, subst map t2)}
      | _ -> {t with term = Lam (id, subst map t1, subst map t2)}
    end
  | Pi (id, t1, t2) ->
    begin match IMap.find_opt id map with
      | None -> {t with term = Pi (id, subst map t1, subst map t2)}
      | Some t1 -> match t1.term with
        | Var (v, _) -> {t with term = Pi (v, subst map t1, subst map t2)}
        | _ -> {t with term = Pi (id, subst map t1, subst map t2)}
    end
  | Op _ | Const _ -> t

let build_parsing (r : (term -> term IMap.t) parsing) (t : term): (term -> term) parsing =
  map r (fun map t' -> subst (map t') t)

let add_loc (t : naked_term) : term =
  {term = t; loc = ("no-name", 0, 0)}

let wrap_loc : naked_term parsing -> term parsing = fun r ->
  seq_operator
      get_loc
      (seq_operator r get_loc (fun v l -> (v, l)))
      (fun loc_start (t, _loc_end) -> {term = t; loc = loc_start})

let () = add_custom_rule p_state 0 terms match_var

let () = add_right_assoc p_state 80 terms
  (match parse_full_raw p_state id_decls "'\\' v ':' x '->' y" with
  | None -> assert false
  | Some f -> build_parsing (build_fold_right_inner (handle_ident (ISet.add "v" ISet.empty) f)) (add_loc (Lam ("v", add_loc (Var ("x", None)), add_loc (Var ("y", None))))))

let () = add_right_assoc p_state 80 terms
  (match parse_full_raw p_state id_decls "'Pi ' v ':' x ',' y" with
  | None -> assert false
  | Some f -> build_parsing (build_fold_right_inner (handle_ident (ISet.add "v" ISet.empty) f)) (add_loc (Pi ("v", add_loc (Var ("x", None)), add_loc (Var ("y", None))))))

let () = add_left_assoc p_state 40 terms
  (match parse_full_raw p_state id_decls "x y" with
  | None -> assert false
  | Some f -> build_parsing (build_fold_left_inner (handle_ident ISet.empty f)) (add_loc (App (add_loc (Var ("x", None)), add_loc (Var ("y", None))))))

let match_assoc =
  seq_operator (
    or_operator
      (map (match_string "left-assoc") (fun () -> LeftAssoc))
      (or_operator
        (map (match_string "right-assoc") (fun () -> RightAssoc))
        (map (match_string "no-assoc") (fun () -> NoAssoc))
      )
    )
    (
      not_operator (match_fchar (fun c -> c <> ' '))
    )
    (fun a () -> a)

let () = add_custom_rule p_state 0 default (
  seq_operator
    (seq_operator
      (seq_operator
        (seq_operator (match_string "Notation") (ws_before (match_char '\"')) (fun () () -> ()))
        (seq_operator (ws_before id_decls) (ws_before (match_char '\"')) (fun decl () -> decl))
        (fun () decl -> decl))
      (seq_operator
        (seq_operator (ws_before (match_string ":=")) (ws_before (match_char '(')) (fun () () -> ()))
        (seq_operator (map (ws_before (lift_prio (get_rules terms))) (fun t -> t)) (ws_before (match_char ')')) (fun t () -> t))
        (fun () t -> t))
      (fun d t -> (d, t)))
    (seq_operator
        (ws_before match_assoc)
        (ws_before match_int)
        (fun a i -> (i, a)))
    (fun (decl, t) (prio, assoc) -> Notation (decl, prio, assoc, [], t))
  |> ws_before
)
