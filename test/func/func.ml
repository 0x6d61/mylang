open OUnit
open Core

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

(* Ast.Env を取得したいときの eval*)
let set_func_and_val_eval expr  = let lexbuf = Lexing.from_string expr in
  let expr = Parser.f Lexer.tokenize lexbuf in
  expr |> List.map (Core.Lib.eval Core.Env.env)

(* envから名前が存在するか *)
let rec get_env var env = match env with
  | [] -> false
  | x::xs -> if x.Ast.ident_name = var then
      true
    else
      get_env var xs

(* 変数とか関数が定義されているか *)
let rec set_func_and_val_test_run list = match list with
  | [] -> ()
  | x::xs -> let (expr,ans) = x in 
    let result = set_func_and_val_eval expr in
    match result with
    | [] -> ()
    | r::_ -> match r with
      | Ast.Env(n) -> (print_string (expr ^ "\n");
                       (assert_equal (get_env ans n) true);
                       set_func_and_val_test_run xs)
      | _ -> assert_bool "error" false

let set_func_test () = 
  let test_case = [
    ("fn add(x) -> {
            x + 3
        }","add");
    ("fn sub(x) -> {
            10-3
        }","sub");
    ("fn add(x,y) -> {
      x + y
    }","add");
    ("fn fizzbuzz(s,e) -> {
    if s == e then
        -1
    else
       _ = if s % 15 == 0 then
        print(\"FizzBuzz\")
    else if s % 3 == 0 then
        print(\"Fizz\")
    else if s % 5 == 0 then
        print(\"Buzz\")
    else
        print(s) in fizzbuzz(s+1,e)
}
      ","fizzbuzz");
  ]
  in set_func_and_val_test_run test_case


let func_call_test () = 
  let test_env = [
    {
      Core.Ast.ident_name = "add";
      Core.Ast.args = [Core.Ast.Ident("x");Core.Ast.Ident("y")];
      Core.Ast.body = [Core.Ast.Add(Core.Ast.Ident("x"),Core.Ast.Ident("y"))];
    };
    {
      Core.Ast.ident_name = "sub";
      Core.Ast.args = [Core.Ast.Ident("x");Core.Ast.Ident("y")];
      Core.Ast.body = [Core.Ast.Add(Core.Ast.Ident("x"),Core.Ast.Ident("y"));Core.Ast.Sub(Ast.Int(10),Ast.Int(12))];
    };
    {
      Core.Ast.ident_name = "wei";
      Core.Ast.args = [];
      Core.Ast.body = [Core.Ast.String("wei")]
    };
    {
      Core.Ast.ident_name = "sqrt";
      Core.Ast.args = [Core.Ast.Ident("x");];
      Core.Ast.body = [Core.Ast.Mul(Core.Ast.Ident("x"),Core.Ast.Ident("x"))];
    };
  ] in 
  let test_case = [
    ("add(4,5)","9");
    ("sub(1,1)","-2");
    ("wei()","wei");
    ("sqrt(2)","4");
  ] in test_run test_env test_case

let suite = "Func Test" >::: [
    "set_func_test" >:: (set_func_test);
    "func_call_test" >:: (func_call_test);
  ]

let _ = run_test_tt_main suite
