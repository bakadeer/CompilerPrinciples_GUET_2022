module SymbolTable where

import Ast
import Data.Map as Map

-- 符号表条目类型
data SymbolEntry
  = ConstEntry {constValue :: Val}
  | VarEntry {varLevel :: Int, varAddr :: Int}
  | ProcEntry {procLevel :: Int, procAddr :: Int}
  deriving (Show, Eq)

-- 符号表
type SymbolTable = Map.Map Ident SymbolEntry
