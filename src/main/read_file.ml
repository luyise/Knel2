open Parser

let check_file_name filename =
  if not (Filename.check_suffix filename ".knl")
    then raise (Arg.Bad ("file \""^filename^"\" is not a .knl file"))

exception Finished_Parsing

let read_file filename =
  let fdesc = open_in filename in
  let content = In_channel.input_all fdesc in
  let () = close_in fdesc in
  let p_state = new_p_state () in
  let c_state = ref (P.create_state content filename) in
  let declared_rules = ref [] in
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
        in
        let () = declared_rules := (decl, prio, assoc, id, tm) :: !declared_rules in
        c_state := c'
      | Open [name] ->
          let fname2 = Filename.dirname filename ^ "/" ^ name ^ ".knlo" in
          let fdesc = open_in fname2 in
          let lexbuf = Lexing.from_channel fdesc in
          let content = Parse_saving.file Lexer.next_token lexbuf in
          let rec aux = function
          | [] -> ()
          | hd::tl ->
              let () = match hd with
              | (decl, prio, LeftAssoc, id, tm) ->
                let () = print_endline "new rule" in
                P.add_left_assoc p_state prio P.terms
                (build_parsing (build_fold_left_inner (handle_ident id decl)) tm)
              | (decl, prio, RightAssoc, id, tm) ->
                P.add_right_assoc p_state prio P.terms
                  (build_parsing (build_fold_right_inner (handle_ident id decl)) tm)
              | _ -> failwith "NYI" in
              aux tl
          in 
          let () = aux content in
          let () = declared_rules := content @ !declared_rules in
          let () = c_state := c' in
          ()
      | Open _ -> assert false
      | _ ->
        begin
          print_endline "parsed unhandled token";
          c_state := c'
        end
  done;
  let fdesc = open_out (filename ^ "o") in
  let fmt = Format.formatter_of_out_channel fdesc in
  let () = Saving.pp_file fmt !declared_rules in
  close_out fdesc

  