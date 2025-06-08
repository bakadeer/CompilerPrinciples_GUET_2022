-- Compiler.hs
module Compiler (compile) where

import Lexer (alexScanTokens)
import Parser
import Ast (Ast, Ident)
import Codegen (generateQuads)
import Ir(Quad)
import Semantic (semanticCheck, SymbolInfo)
import qualified Data.Map as Map

compile :: String -> Bool -> ([Quad], Ast, Map.Map Ident SymbolInfo)
compile src useLl
  | not useLl =
    -- 词法分析：将源代码字符串解析为标记（Token）列表
    let tokens = alexScanTokens src
        -- 语法分析：将Token列表解析为抽象语法树（AST）
        ast = parseToAst tokens
        -- 语义分析：对AST进行语义检查，返回符号表或错误信息
        semanticResult = semanticCheck ast
    -- 处理语义检查结果
    in case semanticResult of
      -- 语义错误，抛出异常
      Left err -> error $ "Semantic error: " ++ show err
      Right table ->
        -- 中间代码生成：将AST转换为四元组列表
        let quads = generateQuads ast
        -- 成功返回四元组、AST和符号表
        in (quads, ast, table)

  | otherwise =
    error "LLVM code generation is not yet implemented"