module Main where

import Compiler (compile)
import Data.List (intercalate)
import qualified Data.Map as Map

-- 程序主函数：IO 操作流程
main :: IO ()
main = do
  -- 从标准输入读取源代码内容
  src <- getContents

  -- 调用编译器函数 compile：
  -- 参数说明：
  -- - src: 源代码字符串
  -- - False: 不启用 LLVM 后端（当前仅支持非 LLVM 模式）
  -- 返回值：
  -- - quads: 生成的四元组列表（[Quad]）
  -- - _: 忽略的 AST（未使用）
  -- - table: 构建完成的符号表（Map Ident SymbolInfo）
  let (quads, _, table) = compile src False

  -- 输出符号表信息
  putStrLn "Symbol Table:"
  mapM_ print (Map.toList table)  -- 将符号表转为列表并逐项打印

  -- 输出生成的四元组
  putStrLn "Generated Quadruples:"
  mapM_ print quads              -- 逐个打印每个四元组