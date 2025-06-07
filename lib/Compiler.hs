-- Compiler.hs
module Compiler (compile) where

import Lexer (alexScanTokens)
import Parser
import Ast (Ast, Ident)
import Codegen (generateQuads)
import Ir (Quad)
import Semantic (semanticCheck, SymbolInfo)
import qualified Data.Map as Map

compile :: String -> Bool -> ([Quad], Ast, Map.Map Ident SymbolInfo)
compile src useLl
  | not useLl =
    let tokens = alexScanTokens src
        ast = parseToAst tokens
        semanticResult = semanticCheck ast
    in case semanticResult of
      Left err -> error $ "Semantic error: " ++ show err
      Right table ->
        let quads = generateQuads ast
        in (quads, ast, table)