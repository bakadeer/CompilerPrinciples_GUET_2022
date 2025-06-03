module Compiler (compile) where

import Ast (Ast)
import Lexer (SpannedToken (..), alexScanTokens)
import Parser
import Token
import TokenPrinter (printTokens)

compile :: String -> Bool -> Either [String] Ast
compile src useLl
  | useLl = error "Ll unimplemented yet!"
  | otherwise =
      let tokens = alexScanTokens src
       in if any isError tokens
            then Left (printTokens tokens)
            else Right (parseToAst tokens)
  where
    isError (SpannedToken _ token) = case token of
      Token.IllegalChar _ -> True
      Token.IdentTooLong _ -> True
      Token.IntOverflow _ -> True
      _ -> False
