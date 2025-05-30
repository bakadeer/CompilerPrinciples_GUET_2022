module Compiler (compile) where

import Lexer (alexScanTokens)
import Parser
import Ast (Ast)

compile :: String -> Bool -> Ast
compile src useLl
  | not useLl = parseToAst . alexScanTokens $ src
  | otherwise = error "Ll unimplemented yet!"
