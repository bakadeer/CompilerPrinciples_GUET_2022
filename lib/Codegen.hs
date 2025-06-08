-- Codegen.hs
module Codegen (generateQuads, Quad) where

import Ast 
import Ir
import Control.Monad.State.Lazy
import qualified Data.Foldable

-- 临时变量计数器和标签计数器
type Context = (Int, Int)

-- 使用 State Monad 来管理临时变量和标签编号
-- 使用双层 StateT：外层是 Quad 列表，内层是 Context
type Codegen a = StateT [Quad] (State Context) a

-- 添加四元式到列表中并分配序号
emit :: Quad -> Codegen ()
emit quad = do
  (t, l) <- lift get
  lift $ put (t, l)
  modify (quad :)

-- 获取新临时变量名 t0, t1, etc
newTemp :: Codegen String
newTemp = do
  -- lift get作用是获取内层context的值
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
-- 最终结果是逆序的，因此使用 reverse 返回正确顺序。
generateQuads :: Ast -> [Quad]
generateQuads (Program sub) =
  let ((_, quads), _) = runState (runStateT (genProgram sub) []) (0, 0)
  -- 反转 quads
  in reverse quads

-- genProgram：处理整个程序结构，包括常量、变量、过程声明及主程序语句。
genProgram :: Subprogram -> Codegen ()
genProgram (Subprogram mConst mVar mProc mainStmt) = do
  -- 生成初始四元式，只有一个参数，表示程序入口
  emit $ Quad (Ir.SysStart, Nothing, Nothing, Nothing)
  
  -- 常量声明，对于每个常量，生成一个四元式，将常量值作为参数
  case mConst of 
    Just (ConstDecl defs) -> 
      -- 一个常量声明包含name 和 val
      mapM_ (\(ConstDefi name val) -> do
        -- 声明常量，再赋值
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
  Data.Foldable.forM_ mProc genProc

  -- 主程序语句
  genStmt mainStmt

  -- 程序结束四元式
  emit $ Quad (Ir.SysEnd, Nothing, Nothing, Nothing)

-- genProc：生成过程的四元组表示。
genProc :: ProcDecl -> Codegen ()
genProc (ProcDecl (ProcHeader name) body _) = do
  -- 生成过程头四元式
  emit $ Quad (Ir.Procedure, Just name, Nothing, Nothing)
  -- 嵌套生成子程序体
  genStmt (stmt body)
  -- 添加返回语句
  emit $ Quad (Ir.Ret, Nothing, Nothing, Nothing)

-- 生成语句：Stmt -> 四元式
genStmt :: Stmt -> Codegen ()
-- 模式匹配
-- 赋值
genStmt (AssignStmt dest expr) = do
  -- 生成表达式代码，返回临时变量名
  result <- genExpr expr
  -- 生成四元式，将结果赋值给目标变量
  emit $ Quad (Assign, Just result, Nothing, Just dest)

-- 条件
genStmt (CondStmt (Cond e1 op e2) thenStmt) = do
  -- 生成条件判断代码
  v1 <- genExpr e1
  v2 <- genExpr e2
  -- 创建新的标签
  labelFalse  <- newLabel

  -- 根据 RelaOp 构造对应的 IR 操作符
  let irOp = case op of
        Ast.Eq            -> Ir.JumpEq       -- 等于
        Ast.NotEq         -> Ir.JumpNeq      -- 不等于
        Ast.LessEqThan    -> Ir.JumpLe       -- 小于等于
        Ast.LessThan      -> Ir.JumpLt       -- 小于
        Ast.GreaterThan   -> Ir.JumpGt       -- 大于
        Ast.GreaterEqThan -> Ir.JumpGe       -- 大于等于

  -- 生成四元式，判断条件
  emit $ Quad (irOp, Just v1, Just v2, Just (show labelFalse))
  -- 如果条件成立，生成then语句的四元式
  genStmt thenStmt
  emit $ Quad (Ir.Label, Nothing, Nothing, Just (show labelFalse))

-- 循环
genStmt (LoopStmt (Cond e1 op e2) doStmt) = do

  labelStart <- newLabel
  labelEnd <- newLabel
  emit $ Quad (Ir.Label, Nothing, Nothing, Just (show labelStart))  -- 
  
  -- 生成条件判断代码
  v1 <- genExpr e1
  v2 <- genExpr e2

  let irOp = case op of
        Ast.Eq            -> Ir.JumpEq       -- 等于
        Ast.NotEq         -> Ir.JumpNeq      -- 不等于
        Ast.LessEqThan    -> Ir.JumpLe       -- 小于等于
        Ast.LessThan      -> Ir.JumpLt       -- 小于
        Ast.GreaterThan   -> Ir.JumpGt       -- 大于
        Ast.GreaterEqThan -> Ir.JumpGe       -- 大于等于

  emit $ Quad (irOp, Just v1, Just v2, Just (show labelEnd))
  -- 生成循环体的四元式
  genStmt doStmt
  emit $ Quad (Ir.Jump, Nothing, Nothing, Just (show labelStart))
  
  -- 设置循环结束标签（实际只是一个标记点）
  emit $ Quad (Ir.Label, Nothing, Nothing, Just (show labelEnd))

-- 调用
genStmt (CallStmt proc) =
  emit $ Quad (Ir.Call, Just proc, Nothing, Nothing)

-- 读
genStmt (ReadStmt vars) =
  mapM_ (\var -> emit $ Quad (Ir.Read, Just var, Nothing, Nothing)) vars

-- 写
genStmt (WriteStmt exprs) = do
  mapM_ (\expr -> do
      val <- genExpr expr
      emit $ Quad (Ir.Write, Just val, Nothing, Nothing)
    ) exprs

-- 复合语句
genStmt (CompoundStmt stmts) =
  mapM_ genStmt stmts

-- 条件语句支持 OddCond
genStmt (CondStmt (OddCond expr) thenStmt) = do
  val <- genExpr expr
  label <- newLabel
  emit $ Quad (Ir.Test, Just val, Nothing, Just (show label))  -- 假设 Test 表示奇偶测试
  genStmt thenStmt
  emit $ Quad (Ir.Label, Nothing, Nothing, Just (show label))  -- 跳转到标签

-- 循环语句支持 OddCond
genStmt (LoopStmt (OddCond expr) doStmt) = do
  start <- newLabel
  loop <- newLabel
  end <- newLabel

  emit $ Quad (Ir.Label, Just (show start), Nothing, Nothing)

  val <- genExpr expr
  emit $ Quad (Ir.Test, Just val, Nothing, Just (show loop))
  emit $ Quad (Ir.JumpEq, Nothing, Nothing, Just (show end))

  emit $ Quad (Ir.Label, Just (show loop), Nothing, Nothing)
  genStmt doStmt
  emit $ Quad (Ir.Jump, Just (show start), Nothing, Nothing)

  emit $ Quad (Ir.Label, Just (show end), Nothing, Nothing)

-- 空语句
genStmt NullStmt = return ()

-- 表达式生成：Expr -> 四元式
genExpr :: Expr -> Codegen String
-- (Expr _ item []) 表示只有一个项，没有操作符
genExpr (Expr _ item []) = genItem item
-- (Expr _ i1 ((op,i2):_)) 忽略第一个sign，匹配之后的项和op+项，将操作存入temp
genExpr (Expr _ i1 ((op,i2):_)) = do
  v1 <- genItem i1
  v2 <- genItem i2
  temp <- newTemp
  -- 根据操作符生成四元式
  emit $ Quad (transOp op, Just v1, Just v2, Just temp)
  return temp
  where
    transOp Ast.Add = Ir.Add
    transOp Ast.Sub = Ir.Sub

-- 生成四元组：Item -> 四元式
genItem :: Item -> Codegen String
-- 如果只有一个因子，没有操作符，则生成因子
genItem (Item f []) = genFactor f
-- 如果有多个因子和操作符，则生成四元式
genItem (Item f1 ((op,f2):_)) = do
  v1 <- genFactor f1 
  v2 <- genFactor f2
  temp <- newTemp
  emit $ Quad (transOp op, Just v1, Just v2, Just temp)
  return temp
  where
    transOp Ast.Mul = Ir.Mul
    transOp Ast.Div = Ir.Div

-- 因子生成：Factor -> 四元式
genFactor :: Factor -> Codegen String
genFactor (Ast.Identifier name) = return name
genFactor (Ast.Number val) = return $ show val
genFactor (Ast.ParenedExpr expr) = genExpr expr