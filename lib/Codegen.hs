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
  let ((_, quads), _) = runState (runStateT (checkSubprogram sub) []) (0, 0)
  in reverse quads

checkSubprogram :: Subprogram -> Codegen ()
checkSubprogram (Subprogram mConst mVar mProc mainStmt) = do
  case mProc of
    Nothing -> return ()
    Just (ProcDecl header sub more) -> do
      translateStmt $ stmtToList (stmt sub)
      mapM_ (\p -> translateStmt $ stmtToList (stmt $ subprogram p)) more
  translateStmt $ stmtToList mainStmt

-- 将 Stmt 转换为 Stmt 列表（如果是 CompoundStmt，则展开）
stmtToList :: Stmt -> [Stmt]
stmtToList (CompoundStmt ss) = ss
stmtToList s                 = [s]

-- 翻译一条或多条语句
translateStmt :: [Stmt] -> Codegen ()
translateStmt = mapM_ translateOneStmt

-- 处理赋值语句
translateOneStmt (AssignStmt ident expr) = do
  val <- translateExpr expr
  emit $ Quad (Assign, Just val, Nothing, Just ident)

-- 处理 if 语句（仅支持 then）
translateOneStmt (CondStmt (Cond cond relop expr) stmt) = do
  lFalse <- newLabel
  lEnd   <- newLabel
  -- 条件判断
  left  <- translateExpr cond
  right <- translateExpr expr
  emit $ Quad (case relop of
                Ast.Eq           -> Ir.Eq
                Ast.NotEq        -> Ir.Neq
                Ast.LessThan     -> Ir.Lt
                Ast.LessEqThan   -> Ir.Le
                Ast.GreaterThan  -> Ir.Gt
                Ast.GreaterEqThan-> Ge, Just left, Just right, Just lFalse)
  emit $ Quad (IfFalseJump, Just lFalse, Nothing, Nothing)
  translateOneStmt stmt
  emit $ Quad (Jump, Just lEnd, Nothing, Nothing)
  emit $ Quad (Label, Just lFalse, Nothing, Nothing)
  emit $ Quad (Label, Just lEnd, Nothing, Nothing)

-- 处理 while ... do ...
translateOneStmt (LoopStmt (Cond cond relop expr) stmt) = do
  lStart <- newLabel
  lFalse <- newLabel
  emit $ Quad (Label, Just lStart, Nothing, Nothing)
  left  <- translateExpr cond
  right <- translateExpr expr
  emit $ Quad (case relop of
                Ast.Eq           -> Ir.Eq
                Ast.NotEq        -> Ir.Neq
                Ast.LessThan     -> Ir.Lt
                Ast.LessEqThan   -> Ir.Le
                Ast.GreaterThan  -> Ir.Gt
                Ast.GreaterEqThan-> Ge, Just left, Just right, Just lFalse)
  emit $ Quad (IfFalseJump, Just lFalse, Nothing, Nothing)
  translateOneStmt stmt
  emit $ Quad (Jump, Just lStart, Nothing, Nothing)
  emit $ Quad (Label, Just lFalse, Nothing, Nothing)

-- read(...)
translateOneStmt (ReadStmt idents) =
  mapM_ (\ident -> emit $ Quad (Ir.Read, Just ident, Nothing, Nothing)) idents

-- write(...)
translateOneStmt (WriteStmt exprs) = do
  vals <- mapM translateExpr exprs
  mapM_ (\val -> emit $ Quad (Ir.Write, Just val, Nothing, Nothing)) vals

-- 复合语句 begin ... end
translateOneStmt (CompoundStmt stmts) = translateStmt stmts


-- 其它暂不支持
translateOneStmt _ = return ()

-- 表达式翻译成中间代码
translateExpr :: Expr -> Codegen String
translateExpr (Expr sign item terms) = do
  base <- translateItem item
  rest <- mapM (\(op, i) -> do
                  rhs <- translateItem i
                  temp <- newTemp
                  let quadOp = case op of
                                Ast.Add -> Ir.Add
                                Ast.Sub -> Ir.Sub
                  emit $ Quad (quadOp, Just base, Just rhs, Just temp)
                  return temp) terms
  return $ last (base : rest)

translateItem :: Item -> Codegen String
translateItem (Item factor factors) = do
  base <- translateFactor factor
  rest <- mapM (\(op, f) -> do
                  rhs <- translateFactor f
                  temp <- newTemp
                  let quadOp = case op of
                                Ast.Mul -> Ir.Mul
                                Ast.Div -> Ir.Div
                  emit $ Quad (quadOp, Just base, Just rhs, Just temp)
                  return temp) factors
  return $ last (base : rest)

translateFactor :: Factor -> Codegen String
translateFactor (Ast.Identifier ident) = return ident
translateFactor (Ast.Number val) = return $ show val
translateFactor (ParenedExpr expr) = translateExpr expr

-- 插入四元式到结果中
emit :: Quad -> Codegen ()
emit quad = modify (quad :)

