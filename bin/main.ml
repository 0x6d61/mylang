open Core

let read_file filename = 
  let fi = open_in filename in 
  let line = really_input_string fi (in_channel_length fi) in
  close_in fi;
  line

let eval env expr =
  expr |> Core.Lib.eval env

let rec eval_map env expr = match expr with 
  [] -> Core.Ast.Bool(true)
  | e::es -> let result =  e |> Core.Lib.eval env in
            match result with
            | Core.Ast.Env(ret_env) ->  eval_map ret_env es
            | _ -> eval_map env es


let () = if Array.length Sys.argv > 0 then
  try
    let file = read_file Sys.argv.(1) in 
    let lexbuf = Lexing.from_string file in
    let expr = Parser.f Lexer.tokenize lexbuf in 
    let _ = eval_map Core.Env.env expr in ()
  with Core.Error.Error msg -> (print_newline (); print_string msg; print_newline ();)
  | Core.Error.NameError msg -> (print_newline (); print_string msg; print_newline ();)
  | Core.Error.TypeError msg -> (print_newline (); print_string msg; print_newline ();)
  else
    ()