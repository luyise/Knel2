open Knel_lib
open Syntax
open Typer
open Parser
open P
open Context
open Beta

let () =
  let _ = match parse_full p_state terms "\\x:i->x"
  with
    | None -> failwith "Parse error"
    | Some tm ->
        pp_term Format.std_formatter tm;
        Format.print_flush ();
        print_endline "";
        let tm_typ = typ std_rules ["i"] [("i", wrap_nterm (UU(0)))] tm in
        pp_term Format.std_formatter tm_typ;
        Format.print_flush ();
        print_endline "";
  in
  print_endline "Hello world"
