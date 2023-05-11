open Knel_lib
open Syntax
open Typer
open Parser
open P
open Context
open Beta

let files_to_compile = ref []

let set_file f = files_to_compile := f :: !files_to_compile

let () = Config.parse_arguments set_file

let main () =
    match !files_to_compile with
    | [h] ->
      let () = Read_file.check_file_name h in
      Read_file.read_file h
    | _ ->
        print_endline Config.usage;
        exit 1

let () = main ()
