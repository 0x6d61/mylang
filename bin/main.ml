open Core

let _ = let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.f Lexer.tokenize lexbuf in
  expr |> Core.Lib.eval Core.Env.env