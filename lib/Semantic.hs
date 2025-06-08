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

-- 符号表
type SymbolTable = Map.Map Ident SymbolInfo

-- 符号信息类型
data SymbolInfo = VarInfo | ConstInfo | ProcInfo deriving (Show, Eq)

-- 入口
semanticCheck :: Ast -> Either SemanticError SymbolTable
semanticCheck (Program sub) = checkSubprogram Map.empty sub

-- 检查子程序
checkSubprogram :: SymbolTable -> Subprogram -> Either SemanticError SymbolTable
checkSubprogram table (Subprogram mConst mVar mProc s_stmt) = do
  -- 声明常量
  table1 <- case mConst of
    -- 若无了常量声明，则返回当前符号表
    Nothing -> Right table
    -- 若仍有常量声明，则检查常量声明并更新符号表，foldl：
    Just (ConstDecl defs) -> foldl (\acc (ConstDefi ident _) -> acc >>= declareConst ident) (Right table) defs
  -- 声明变量
  table2 <- case mVar of
    Nothing -> Right table1
    Just (VarDecl idents) -> foldl (\acc ident -> acc >>= declareVar ident) (Right table1) idents
  -- 声明过程
  table3 <- case mProc of
    Nothing -> Right table2
    Just proc -> declareProcs table2 proc
  _ <- checkStmt table3 s_stmt
  Right table3

-- 声明所有过程（递归处理 moreProcs）
declareProcs :: SymbolTable -> ProcDecl -> Either SemanticError SymbolTable
declareProcs table (ProcDecl (ProcHeader ident) sub more) = do
  table1 <- declareProc ident table
  -- 过程体用新作用域
  _ <- checkSubprogram table1 sub
  foldl (\acc p -> acc >>= \t -> declareProcs t p) (Right table1) more

-- 声明常量，将ident(常量名)加入符号表
declareConst :: Ident -> SymbolTable -> Either SemanticError SymbolTable
declareConst ident table =
  if Map.member ident table                       -- if 符号表已存在 ident
    then Left (DuplicateDeclaration ident)        -- 抛出错误，重复声明
    -- 调用Map.insert，参数是ident和ConstInfo，返回新的符号表
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
  -- 赋值语句
  AssignStmt ident expr -> do
    case Map.lookup ident table of
      -- 如果符号表中没有 ident，抛出未声明变量错误
      Nothing -> Left (UndeclaredVariable ident)
      -- 如果是常量，抛出赋值给常量变量错误
      Just ConstInfo -> Left (AssignToConst ident)
      -- 如果是过程，抛出不能给过程赋值错误
      Just ProcInfo -> Left (OtherError $ "不能给过程赋值: " ++ ident)
      -- 如果是变量，检查表达式
      Just VarInfo -> checkExpr table expr
  -- 条件语句由条件和语句组成 >> 忽略第一个checkCond语句的返回值执行第二个checkStmt
  CondStmt cond s -> checkCond table cond >> checkStmt table s
  LoopStmt cond s -> checkCond table cond >> checkStmt table s
  -- 调用语句
  CallStmt ident ->
    -- 在符号表中查找 ident
    case Map.lookup ident table of
      -- 如果符号表中没有 ident，抛出未声明过程错误
      Just ProcInfo -> Right ()
      Just _ -> Left (OtherError $ "调用的不是过程: " ++ ident)
      Nothing -> Left (UndeclaredProcedure ident)
  -- 读入语句
  ReadStmt idents ->
    -- mapM_ 对每个idents处理，在表中寻找ident
    mapM_ (\ident -> case Map.lookup ident table of
      -- 如果符号表中没有 ident，抛出未声明变量错误
      Nothing -> Left (UndeclaredVariable ident)
      -- 如果是常量，抛出赋值给常量变量错误
      Just ConstInfo -> Left (AssignToConst ident)
      -- 如果是过程，抛出不能读入过程错误
      Just ProcInfo -> Left (OtherError $ "不能读入过程: " ++ ident)
      -- 如果是变量，返回 Right ()
      Just VarInfo -> Right ()) idents
  -- 输出语句，检查输出语句的表达式
  WriteStmt exprs ->
    mapM_ (checkExpr table) exprs
  CompoundStmt stmts -> mapM_ (checkStmt table) stmts
  -- 空语句
  NullStmt -> Right ()

-- 检查条件
checkCond :: SymbolTable -> Cond -> Either SemanticError ()
checkCond table cond = case cond of
  -- 检查两个关系表达式
  Cond e1 _ e2 -> checkExpr table e1 >> checkExpr table e2
  -- 单表达式检查
  OddCond e -> checkExpr table e

-- 检查表达式
checkExpr :: SymbolTable -> Expr -> Either SemanticError ()
-- (Expr _ item rest) 忽略第一个字段，匹配item，其余识别为rest
checkExpr table (Expr _ item rest) = do
  -- 检查项
  checkItem table item
  -- 检查项列表，忽略op，只关注it，详见Ast.hs的Item定义
  mapM_ (\(_, it) -> checkItem table it) rest

-- 检查项
checkItem :: SymbolTable -> Item -> Either SemanticError ()
checkItem table (Item factor rest) = do
  -- 检查因子
  checkFactor table factor
  mapM_ (\(_, f) -> checkFactor table f) rest

-- 检查因子
checkFactor :: SymbolTable -> Factor -> Either SemanticError ()
checkFactor table factor = case factor of
  -- 标识符、数字字面量、括号内表达式
  Identifier ident ->
    case Map.lookup ident table of
      -- 如果符号表中没有 ident，抛出未声明变量错误
      Nothing -> Left (UndeclaredVariable ident)
      -- 如果是常量，抛出赋值给常量变量错误
      Just ProcInfo -> Left (OtherError $ "过程不能作为表达式: " ++ ident)
      _ -> Right ()
  Number _ -> Right ()
  -- 括号内表达式，递归检查表达式
  ParenedExpr expr -> checkExpr table expr
