open Core

let _ = let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.f Lexer.tokenize lexbuf in
  (print_string (Core.Lib.to_string (Core.Lib.eval expr)))