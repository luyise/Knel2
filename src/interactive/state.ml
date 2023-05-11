open Context
open Syntax
open Beta
open Typer

type local_state =
  { (* name of the subgoals term, 
    which coincide with the variable identifiant who abstract it in the built_term of the reasoning_state*)
    name : ident
  ; local_id_pool : ident list
  ; local_context : context
  ; local_definitions : (ident * term) list
  ; local_display_options : (ident * bool) list
    (* type of the term being defined (aka goal) *)
  ; goal : term
  }

type reasoning_state =
  { (* type of the term being defined in the current reasoning *)
    global_goal : term
    (* name of the holes in the term being build *)
  ; subgoals_names : ident list
    (* list of local environments in which tactics may be applied *)
  ; subgoals : local_state list
    (* term of type global_goal, which is under construction *)
  ; built_term : term
  }

type status =
  | SResting
  | SReasoning of reasoning_state
  | SPanicking of knel_state_error
and knel_state_error =
  | KSETypeError
  | KSESyntaxError
  | KSECannotPerformActionError
  | KSEBusyError

type knel_state =
  { global_id_pool : ident list
  ; global_context : context
  ; global_definitions : (ident * term) list
    (* specifying if an element of the context must be displayed or not *)
  ; global_display_options : (ident * bool) list
  ; red_rules : beta_rule list
  ; status : status
  }

let make_new_reasoning_state
  (goal_typ : term)
  : reasoning_state
= { global_goal = goal_typ
  ; subgoals_names = ["goal"]
  ; subgoals = [
      { name = "goal"
      ; local_id_pool = []
      ; local_context = []
      ; local_definitions = []
      ; local_display_options = []
      ; goal = goal_typ
      }
    ]
  ; built_term = wrap_nterm (Var("goal", Some goal_typ))
  }

(* Some procedures that may be called to interact with the kernel, and modify the knel state *)

let get_status
  (kst : knel_state)
  : status
= kst.status

let get_whole_context
  (kst : knel_state)
  : context
= match kst.status with
    | SReasoning rst ->
        begin match rst.subgoals with
          | [] -> kst.global_context
          | sg :: _ -> (sg.local_context @ kst.global_context)
        end
    | _ -> kst.global_context

let get_whole_id_pool
  (kst : knel_state)
  : ident list
= match kst.status with
    | SReasoning rst ->
        begin match rst.subgoals with
          | [] -> kst.global_id_pool
          | sg :: _ -> (sg.local_id_pool @ kst.global_id_pool)
        end
    | _ -> kst.global_id_pool

let get_displayed_context
  (kst : knel_state)
  : context
= assert false

let get_goals_name
  (kst : knel_state)
  : (ident list) option
= match kst.status with
  | SReasoning rst  -> Some rst.subgoals_names
  | _               -> None

let get_goals_typ
  (kst : knel_state)
  : (term list) option
= match kst.status with
  | SReasoning rst  
    -> Some (
        List.map 
          (fun sg -> sg.goal)
          rst.subgoals
      )
  | _
    -> None

let get_typ
  (kst : knel_state)
  (tm : term)
  : term
=
  match (get_whole_id_pool kst), (get_whole_context kst) with
    | whole_id_pool, whole_context
      -> typ
          kst.red_rules
          whole_id_pool
          whole_context
          tm

let add_to_global_id_pool
  (kst : knel_state)
  (id : ident)
  : knel_state
= {kst with global_id_pool = id :: kst.global_id_pool}

let add_to_local_id_pool
  (kst : knel_state)
  (id : ident)
  : knel_state
= match kst.status with
    | SReasoning rst ->
        begin match rst.subgoals with
          | [] -> kst
          | sg :: sg_tail 
            -> let sg' = {sg with local_id_pool = id :: sg.local_id_pool}
              in let rst' = {rst with subgoals = sg' :: sg_tail}
              in {kst with status = SReasoning rst'}
        end
    | _ -> kst

let add_to_global_context
  (kst : knel_state)
  (id : ident)
  (tm : term)
  : knel_state
= let kst' = add_to_global_id_pool kst id in
  {kst' with global_context = (id, tm) :: kst.global_context}

let add_to_local_context
  (kst : knel_state)
  (id : ident)
  (tm : term)
  : knel_state
= let kst' = add_to_local_id_pool kst id in
  match kst'.status with
    | SReasoning rst ->
        begin match rst.subgoals with
          | [] -> kst'
          | sg :: sg_tail 
            -> let sg' = {sg with local_context = (id, tm) :: sg.local_context}
              in let rst' = {rst with subgoals = sg' :: sg_tail}
              in {kst' with status = SReasoning rst'}
        end
    | _ -> kst'

let add_to_global_definitions
  (kst : knel_state)
  (id : ident)
  (tm : term)
  : knel_state
= {kst with global_definitions = (id, tm) :: kst.global_definitions}

let add_to_local_definitions
  (kst : knel_state)
  (id : ident)
  (tm : term)
  : knel_state
= match kst.status with
    | SReasoning rst ->
        begin match rst.subgoals with
          | [] -> kst
          | sg :: sg_tail 
            -> let sg' = {sg with local_definitions = (id, tm) :: sg.local_definitions}
              in let rst' = {rst with subgoals = sg' :: sg_tail}
              in {kst with status = SReasoning rst'}
        end
    | _ -> kst

let set_global_display_option
  (kst : knel_state)
  (id : ident)
  (b : bool)
  : knel_state
= let glob_disp_optn' = 
    List.map
      (fun (id', b') -> 
        if id' = id then (id, b)
        else (id', b')
      )
      kst.global_display_options
  in {kst with global_display_options = glob_disp_optn'}

let set_local_display_option
  (kst : knel_state)
  (id : ident)
  (b : bool)
  : knel_state
= match kst.status with
| SReasoning rst ->
    begin match rst.subgoals with
      | [] -> kst
      | sg :: sg_tail 
        -> 
          let loc_disp_optn' = 
            List.map
              (fun (id', b') ->
                if id' = id then (id, b)
                else (id', b')
              )
              sg.local_display_options
          in let sg' = {sg with local_display_options = loc_disp_optn'}
          in let rst' = {rst with subgoals = sg' :: sg_tail}
          in {kst with status = SReasoning rst'}
    end
| _ -> kst

let launch_reasoning_mode
  (kst : knel_state)
  (goal_typ : term)
  : knel_state
= match kst.status with
    | SResting ->
        let fresh_reasoning_state = make_new_reasoning_state goal_typ
        in {kst with status = SReasoning fresh_reasoning_state}
    | _ -> {kst with status = SPanicking KSECannotPerformActionError}

let close_reasoning_mode
  (kst : knel_state) 
  (new_term_name : ident)
  : knel_state
= match kst.status with
    | SReasoning rst ->
        begin match rst.subgoals with
          | [] -> 
            let kst' =
              add_to_global_context kst new_term_name rst.global_goal 
            in let kst'' =
              add_to_global_definitions kst' new_term_name rst.built_term
            in
            {kst'' with status = SResting}
          | _ -> {kst with status = SPanicking KSECannotPerformActionError}
        end
    | _ -> {kst with status = SPanicking KSECannotPerformActionError}