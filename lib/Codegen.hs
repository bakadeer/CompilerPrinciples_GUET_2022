-- Codegen.hs
module Codegen (generateQuads) where

import Ast 
import Ir
import Control.Monad.State.Lazy
import Token

-- 临时变量计数器和标签计数器
type Context = (Int, Int)

-- 使用 State Monad 来管理临时变量和标签编号
-- 使用双层 StateT：外层是 Quad 列表，内层是 Context
type Codegen a = StateT [Quad] (State Context) a

-- 添加四元式到列表中
emit :: Quad -> Codegen ()
emit quad = modify (quad :)

-- 获取新临时变量名
newTemp :: Codegen String
newTemp = do
  (t, l) <- lift get
  lift $ put (t + 1, l)
  return $ "t" ++ show t

-- 获取新标签名
newLabel :: Codegen String
newLabel = do
  (t, l) <- lift get
  lift $ put (t, l + 1)
  return $ "L" ++ show l

-- 主要函数：AST -> 四元式列表
generateQuads :: Ast -> [Quad]
generateQuads (Program sub) =
  let ((_, quads), _) = runState (runStateT (genProgram sub) []) (0, 0)
  in reverse quads

genProgram :: Subprogram -> Codegen ()
genProgram (Subprogram mConst mVar mProc mainStmt) = do
  emit $ Quad (Ir.SysStart, Nothing, Nothing, Nothing)
  
  -- 常量声明
  case mConst of 
    Just (ConstDecl defs) -> 
      mapM_ (\(ConstDefi name val) -> do
        emit $ Quad (Ir.Const, Just name, Nothing, Nothing)
        emit $ Quad (Ir.Assign, Just (show val), Nothing, Just name)
      ) defs
    Nothing -> return ()

  -- 变量声明
  case mVar of
    Just (VarDecl vars) ->
      mapM_ (\name -> emit $ Quad (Ir.Var, Just name, Nothing, Nothing)) vars
    Nothing -> return ()

  -- 过程声明
  case mProc of
    Just proc -> genProc proc
    Nothing -> return ()

  -- 主程序语句
  genStmt mainStmt
  emit $ Quad (Ir.SysEnd, Nothing, Nothing, Nothing)

genProc :: ProcDecl -> Codegen ()
genProc (ProcDecl (ProcHeader name) body _) = do
  emit $ Quad (Ir.Procedure, Just name, Nothing, Nothing)
  genStmt (stmt body)
  emit $ Quad (Ir.Ret, Nothing, Nothing, Nothing)

genStmt :: Stmt -> Codegen ()
genStmt (AssignStmt dest expr) = do
  result <- genExpr expr
  emit $ Quad (Assign, Just result, Nothing, Just dest)

genStmt (CondStmt (Cond e1 _ e2) thenStmt) = do
  v1 <- genExpr e1
  v2 <- genExpr e2
  label <- newLabel
  emit $ Quad (JumpLe, Just v1, Just v2, Just label)
  genStmt thenStmt

genStmt (LoopStmt (Cond e1 _ e2) doStmt) = do
  start <- newLabel
  loop <- newLabel
  end <- newLabel
  
  -- 循环开始标签
  emit $ Quad (Ir.Label, Just start, Nothing, Nothing)
  
  -- 生成条件判断代码
  v1 <- genExpr e1
  v2 <- genExpr e2
  emit $ Quad (Ir.JumpNeq, Just v1, Just v2, Just loop)
  emit $ Quad (Ir.JumpEq, Just v1, Just v2, Just end)
  
  -- 循环体标签和代码
  emit $ Quad (Ir.Label, Just loop, Nothing, Nothing)
  genStmt doStmt
  emit $ Quad (Ir.Jump, Just start, Nothing, Nothing)
  
  -- 循环结束标签
  emit $ Quad (Ir.Label, Just end, Nothing, Nothing)

genStmt (CallStmt proc) =
  emit $ Quad (Ir.Call, Just proc, Nothing, Nothing)

genStmt (ReadStmt vars) =
  mapM_ (\var -> emit $ Quad (Ir.Read, Just var, Nothing, Nothing)) vars

genStmt (WriteStmt exprs) = do
  mapM_ (\expr -> do
      val <- genExpr expr
      emit $ Quad (Ir.Write, Just val, Nothing, Nothing)
    ) exprs

genStmt (CompoundStmt stmts) =
  mapM_ genStmt stmts

genStmt NullStmt = return ()

genExpr :: Expr -> Codegen String
genExpr (Expr _ item []) = genItem item
genExpr (Expr _ i1 ((op,i2):rest)) = do
  v1 <- genItem i1
  v2 <- genItem i2
  temp <- newTemp
  emit $ Quad (transOp op, Just v1, Just v2, Just temp)
  return temp
  where
    transOp Ast.Add = Ir.Add
    transOp Ast.Sub = Ir.Sub

genItem :: Item -> Codegen String
genItem (Item f []) = genFactor f
genItem (Item f1 ((op,f2):rest)) = do
  v1 <- genFactor f1 
  v2 <- genFactor f2
  temp <- newTemp
  emit $ Quad (transOp op, Just v1, Just v2, Just temp)
  return temp
  where
    transOp Ast.Mul = Ir.Mul
    transOp Ast.Div = Ir.Div

genFactor :: Factor -> Codegen String
genFactor (Ast.Identifier name) = return name
genFactor (Ast.Number val) = return $ show val
genFactor (Ast.ParenedExpr expr) = genExpr expr