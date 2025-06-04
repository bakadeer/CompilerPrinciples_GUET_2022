module Compiler (compile) where

import Ast (Ast)
import Lexer (SpannedToken (..), alexScanTokens)
import Parser
import Token
import TokenPrinter (printTokens)

compile :: String -> Bool -> [String]
compile src useLl
  | useLl = error "Ll unimplemented yet!"
  | otherwise =
      let tokens = alexScanTokens src
       in (printTokens tokens)
