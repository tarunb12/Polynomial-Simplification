let filename = Sys.argv.(1)

let () = open_in filename
  |> Lexing.from_channel
  |> Parser.main Lexer.token 
  |> Expr.print_expr
  |> Poly.from_expr
  |> Poly.simplify
  |> Poly.print_polyExpr