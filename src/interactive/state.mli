open Syntax
open Context

type knel_state
type reasoning_state
type status =
  | SResting
  | SReasoning of reasoning_state
  | SPanicking of knel_state_error
and knel_state_error =
  | KSETypeError
  | KSESyntaxError
  | KSECannotPerformActionError
  | KSEBusyError

(* Procedures for interacting with the knel_state *)

val get_status :
  knel_state -> status

val get_whole_context :
  knel_state -> context

val get_whole_id_pool :
  knel_state -> ident list

val get_displayed_context :
  knel_state -> context

val get_goals_name :
  knel_state -> (ident list) option

val get_goals_typ :
  knel_state -> (term list) option

val get_typ :
  knel_state -> term -> term

val add_to_global_id_pool :
  knel_state -> ident -> knel_state

val add_to_local_id_pool :
  knel_state -> ident -> knel_state

(* DOES append the identifiant to the global id pool *)
val add_to_global_context :
  knel_state -> ident -> term -> knel_state

(* DOES append the identifiant to the local id pool *)
val add_to_local_context :
  knel_state -> ident -> term -> knel_state

(* does NOT append the identifiant to the global id pool *)
val add_to_global_definitions :
  knel_state -> ident -> term -> knel_state

(* does NOT append the identifiant to the local id pool *)
val add_to_local_definitions :
  knel_state -> ident -> term -> knel_state

val set_global_display_option :
  knel_state -> ident -> bool -> knel_state

val set_local_display_option :
  knel_state -> ident -> bool -> knel_state

val launch_reasoning_mode :
  knel_state -> term -> knel_state

val close_reasoning_mode :
  knel_state -> ident -> knel_state

(* val run_basic_tac :
  knel_state -> basic_tac -> knel_state *)