open OUnit
open Core
(* eval *)

let eval env expr = let lexbuf = Lexing.from_string expr in
  let expr = Parser.f Lexer.tokenize lexbuf in
  expr |> List.map (Core.Lib.eval env) |> List.map Core.Util.to_string

(* テスト回す関数 *)
let rec test_run env list =
  match list with
  | [] -> ()
  | x::xs -> let (expr,ans) = x in (
      print_string (expr ^ "\n");
      (assert_equal (eval env expr ) [ans]);test_run env xs)

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
                   ("3.2-5.8", "-2.6")] 
  in test_run Core.Env.env test_case

let boolean_test () = 
  let test_case = [
    ("true == true", "true");
    ("false == false", "true");
    ("true == false", "false");
    ("true != false", "true");
    ("\"a\" == \"b\"","false");
    ("\"a\" != \"b\"","true");
    ("\"a\" == \"a\"","true");
    ("'a' == 'b'","false");
    ("'a' != 'b'","true");
    ("'a' == 'a'","true");
    ("1 == 2","false");
    ("1 != 2","true");
    ("1 == 1","true");
    ("1.0 == 2.0","false");
    ("1.0 != 2.0","true");
    ("1.0 == 1.0","true");
    ("1 < 2","true");
    ("3 < 2","false");
    ("2 <= 2","true");
    ("3 <= 2","false");
    ("2 > 1","true");
    ("2 > 2","false");
    ("2 >= 2","true");
    ("2 >= 3","false");
    ("3.0 < 2.0","false");
    ("3.0 <= 2.0","false");
    ("2.0 <= 2.0","true");
    ("1.0 < 2.0","true");
    ("2.0 > 1.1","true");
    ("2.0 > 2.0","false");
    ("2.0 >= 2.0","true");
    ("2.0 >= 2.1","false");

  ] 
  in test_run Core.Env.env test_case

let if_test () =
  let test_case = [
    ("if 1 == 1 {
            true
          }else{
            false
          }
        ","true");
    ("if 1 != 1 {
      true
    } else {
      false
    }","false");
    ("if (13+26)*54 == 1 {
      true
    }else{
      false
    }","false");
    ("if \"aaa\" == \"aaa\" {
      3*3*4
      \"aaaa\"
    }else{
      \"bb\"
    }","aaaa");
    ("if 1 < 3 {
      \"aaaa\"
    }else{
      \"bb\"
    }","aaaa");
  ] in test_run Core.Env.env test_case
let suite = "Test" >::: [
    "four_arithmetic_operations_test" >:: (four_arithmetic_operations_test);
    "boolean_test" >:: (boolean_test);
    "if_test" >:: (if_test);
  ]

let _ = run_test_tt_main suite


