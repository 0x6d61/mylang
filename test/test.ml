open OUnit
open Core

let eval expr = let lexbuf = Lexing.from_string expr in
  let expr = Parser.f Lexer.tokenize lexbuf in
  expr |> Core.Lib.eval |> Core.Util.to_string

let four_arithmetic_operations_test () = 
  let test_case = [("5", "5");
                   ("10", "10");
                   ("5", "5");
                   ("10", "10");
                   ("-5", "-5");
                   ("-10", "-10");
                   ("5+5+5+5-10", "10");
                   ("2*2*2*2*2", "32");
                   ("-50+100+ -50", "0");
                   ("5*2+10", "20");
                   ("5+2*10", "25");
                   ("20 + 2 * -10", "0");
                   ("50/2 * 2 +10", "60");
                   ("2*(5+10)", "30");
                   ("3*3*3+10", "37");
                   ("3*(3*3)+10", "37");
                   ("(5+10*2+15/3)*2+-10", "50");
                   ("1.2", "1.2");
                   ("-2.3", "-2.3");
                   ("1.2+3.2", "4.4");
                   ("1.0+2.3", "3.3");
                   ("2.3*1.0", "2.3");
                   ("3.2-5.8", "-2.6")] in 
  let rec test list =
    match list with
    | [] -> ()
    | x::xs -> let (expr,ans) = x in (
        print_string (expr ^ "\n");
        (assert_equal (eval expr) ans);test xs)
  in test test_case

let suite = "Test" >::: [
    "four_arithmetic_operations_test" >:: (four_arithmetic_operations_test);
  ]

let _ = run_test_tt_main suite