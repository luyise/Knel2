open Syntax

type context = (ident * term) list

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
  if not (List.mem init_id id_pool) then 
    init_id
  else find_aux 2