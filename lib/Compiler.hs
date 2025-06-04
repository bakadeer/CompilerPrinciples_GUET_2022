-- Compiler.hs
module Compiler (compile) where

import Lexer (alexScanTokens)
import Parser
import Ast (Ast)
import Codegen (generateQuads)
import Ir (Quad)

compile :: String -> Bool -> ([Quad], Ast)
compile src useLl
  | not useLl =
    let ast = parseToAst . alexScanTokens $ src
        quads = generateQuads ast
        _ = putStrLn "Generated Quads:" >> mapM_ print quads
        in (quads, ast)
  | otherwise = error "Ll unimplemented yet!"