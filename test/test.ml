
let () = Alcotest.run "Tests" [
  ("Parsing term test", Parse.test);
  ("Parsing int test", Parse_int.test);
  ("Alpha equivalence", Parse.test_alpha);
  ("Typing", Test_typer.test_typer);
]