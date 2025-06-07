module Semantic (semanticCheck, SemanticError(..),SymbolInfo) where

import Ast
import qualified Data.Map as Map

data SemanticError
  = UndeclaredVariable Ident        -- 未声明变量
  | DuplicateDeclaration Ident      -- 重复声明变量/过程
  | UndeclaredProcedure Ident       -- 未声明过程
  | AssignToConst Ident             -- 赋值给常量变量
  | OtherError String               -- 其他错误
  deriving (Show, Eq)

type SymbolTable = Map.Map Ident SymbolInfo

data SymbolInfo = VarInfo | ConstInfo | ProcInfo deriving (Show, Eq)

-- 入口
semanticCheck :: Ast -> Either SemanticError SymbolTable
semanticCheck (Program sub) = checkSubprogram Map.empty sub

-- 检查子程序
checkSubprogram :: SymbolTable -> Subprogram -> Either SemanticError SymbolTable
checkSubprogram table (Subprogram mConst mVar mProc s_stmt) = do
  table1 <- case mConst of
    Nothing -> Right table
    Just (ConstDecl defs) -> foldl (\acc (ConstDefi ident _) -> acc >>= declareConst ident) (Right table) defs
  table2 <- case mVar of
    Nothing -> Right table1
    Just (VarDecl idents) -> foldl (\acc ident -> acc >>= declareVar ident) (Right table1) idents
  table3 <- case mProc of
    Nothing -> Right table2
    Just proc -> declareProcs table2 proc
  _ <- checkStmt table3 s_stmt
  Right table3

-- 声明所有过程（递归处理 moreProcs）
declareProcs :: SymbolTable -> ProcDecl -> Either SemanticError SymbolTable
declareProcs table (ProcDecl (ProcHeader ident) sub more) = do
  table1 <- declareProc ident table
  -- 过程体用新作用域（但这里只做符号检查，实际作用域可根据需要扩展）
  _ <- checkSubprogram table1 sub
  foldl (\acc p -> acc >>= \t -> declareProcs t p) (Right table1) more

-- 声明常量
declareConst :: Ident -> SymbolTable -> Either SemanticError SymbolTable
declareConst ident table =
  if Map.member ident table
    then Left (DuplicateDeclaration ident)
    else Right (Map.insert ident ConstInfo table)

-- 声明变量
declareVar :: Ident -> SymbolTable -> Either SemanticError SymbolTable
declareVar ident table =
  if Map.member ident table
    then Left (DuplicateDeclaration ident)
    else Right (Map.insert ident VarInfo table)

-- 声明过程
declareProc :: Ident -> SymbolTable -> Either SemanticError SymbolTable
declareProc ident table =
  if Map.member ident table
    then Left (DuplicateDeclaration ident)
    else Right (Map.insert ident ProcInfo table)

-- 检查语句
checkStmt :: SymbolTable -> Stmt -> Either SemanticError ()
checkStmt table s_stmt = case s_stmt of
  AssignStmt ident expr -> do
    case Map.lookup ident table of
      Nothing -> Left (UndeclaredVariable ident)
      Just ConstInfo -> Left (AssignToConst ident)
      Just ProcInfo -> Left (OtherError $ "不能给过程赋值: " ++ ident)
      Just VarInfo -> checkExpr table expr
  CondStmt cond s -> checkCond table cond >> checkStmt table s
  LoopStmt cond s -> checkCond table cond >> checkStmt table s
  CallStmt ident ->
    case Map.lookup ident table of
      Just ProcInfo -> Right ()
      Just _ -> Left (OtherError $ "调用的不是过程: " ++ ident)
      Nothing -> Left (UndeclaredProcedure ident)
  ReadStmt idents ->
    mapM_ (\ident -> case Map.lookup ident table of
      Nothing -> Left (UndeclaredVariable ident)
      Just ConstInfo -> Left (AssignToConst ident)
      Just ProcInfo -> Left (OtherError $ "不能读入过程: " ++ ident)
      Just VarInfo -> Right ()) idents
  WriteStmt exprs ->
    mapM_ (checkExpr table) exprs
  CompoundStmt stmts -> mapM_ (checkStmt table) stmts
  NullStmt -> Right ()

-- 检查条件
checkCond :: SymbolTable -> Cond -> Either SemanticError ()
checkCond table cond = case cond of
  Cond e1 _ e2 -> checkExpr table e1 >> checkExpr table e2
  OddCond e -> checkExpr table e

-- 检查表达式
checkExpr :: SymbolTable -> Expr -> Either SemanticError ()
checkExpr table (Expr _ item rest) = do
  checkItem table item
  mapM_ (\(_, it) -> checkItem table it) rest

checkItem :: SymbolTable -> Item -> Either SemanticError ()
checkItem table (Item factor rest) = do
  checkFactor table factor
  mapM_ (\(_, f) -> checkFactor table f) rest

checkFactor :: SymbolTable -> Factor -> Either SemanticError ()
checkFactor table factor = case factor of
  Identifier ident ->
    case Map.lookup ident table of
      Nothing -> Left (UndeclaredVariable ident)
      Just ProcInfo -> Left (OtherError $ "过程不能作为表达式: " ++ ident)
      _ -> Right ()
  Number _ -> Right ()
  ParenedExpr expr -> checkExpr table expr
