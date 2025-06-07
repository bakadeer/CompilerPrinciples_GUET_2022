-- Compiler.hs
module Compiler (compile) where

import Lexer (alexScanTokens)
import Parser
import Ast (Ast)
import Codegen (generateQuads)
import Ir (Quad)
import Semantic (semanticCheck)

compile :: String -> Bool -> ([Quad], Ast)
compile src useLl
  | not useLl =
    let tokens = alexScanTokens src
        ast = parseToAst tokens
        -- 新增语义检查
        semanticResult = semanticCheck ast
    in case semanticResult of
      Left err -> error $ "Semantic error: " ++ show err
      Right () -> 
        let quads = generateQuads ast
            _ = putStrLn "Generated Quads:" >> mapM_ print quads
        in (quads, ast)