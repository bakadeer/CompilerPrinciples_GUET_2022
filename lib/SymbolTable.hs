module SymbolTable (
  Ident,
  Val,
  SymbolTable,
  SymbolTableBuilder,
  Kind,
  Scope,
  addSymbol,
  enterScope,
  leaveScope,
) where

import Data.Map
import Control.Monad.State

type Ident = String
type Val = Int

type Scope = Int
type SymbolTable = Map Ident (Kind, Scope)
type SymbolTableBuilder = State (SymbolTable, Scope)

data Kind = Var | Const | Procedure deriving (Show, Eq)

addSymbol :: Ident -> Kind -> SymbolTableBuilder ()
addSymbol name kind = modify $ \(table, scope) -> (insert name (kind, scope) table, scope)

enterScope :: SymbolTableBuilder ()
enterScope = modify $ \(table, scope) -> (table, scope + 1)

leaveScope :: SymbolTableBuilder ()
leaveScope = modify $ \(table, scope) -> (table, scope - 1)
