open Parser

let check_file_name filename =
  if not (Filename.check_suffix filename ".knl")
    then raise (Arg.Bad ("file \""^filename^"\" is not a .knl file"))

exception Finished_Parsing

let rec read_file filename =
  let fdesc = open_in filename in
  let content = In_channel.input_all fdesc in
  let p_state = new_p_state () in
  let c_state = ref (P.create_state content filename) in
  while not (P.check_finished_with_ws !c_state) do
    match P.parse_single p_state P.default !c_state with
    | Err (file_name, line, chr) ->
      Format.printf "Parsing error in file %s:%d:%d\n" file_name line chr;
      failwith "Parse Error"
    | Value (t, c') ->
      match t with
      | Notation (decl, prio, assoc, id, tm) ->
        let () = match assoc with
        | LeftAssoc ->
          P.add_left_assoc p_state prio P.terms
            (build_parsing (build_fold_left_inner (handle_ident id decl)) tm)
        | RightAssoc ->
          P.add_right_assoc p_state prio P.terms
            (build_parsing (build_fold_right_inner (handle_ident id decl)) tm)
        | _ -> failwith "NYI"
        in c_state := c'
      | _ ->
        begin
          print_endline "parsed unhandled token";
          c_state := c'
        end
  done
  