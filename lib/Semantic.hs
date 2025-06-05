-- 符号表和类型定义
module Semantic where

import Ast
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import SymbolTable

-- 中间代码指令
data Instruction
  = LIT Int -- 将常数压入栈顶
  | LOD Int Int -- 将变量值压入栈顶 (level, addr)
  | STO Int Int -- 将栈顶值存入变量 (level, addr)
  | CAL Int Int -- 调用过程 (level, addr)
  | INT Int -- 为过程分配数据空间
  | JMP Int -- 无条件跳转
  | JPC Int -- 条件跳转（栈顶为0时跳转）
  | OPR Int -- 运算指令
  | RED Int Int -- 读指令 (level, addr)
  | WRT -- 写指令
  | RET -- 返回指令
  deriving (Show, Eq)

-- 运算指令操作码
data OprCode
  = OPR_RET
  | OPR_NEG
  | OPR_ADD
  | OPR_SUB
  | OPR_MUL
  | OPR_DIV
  | OPR_ODD
  | OPR_EQU
  | OPR_NEQ
  | OPR_LSS
  | OPR_LEQ
  | OPR_GTR
  | OPR_GEQ
  deriving (Show, Eq, Enum)

-- 语义分析状态
data SemanticState = SemanticState
  { symbolTable :: SymbolTable,
    currentLevel :: Int,
    currentAddr :: Int,
    instructions :: [Instruction],
    nextInstrAddr :: Int
  }
  deriving (Show)

-- 语义分析错误类型
data SemanticError
  = UndefinedIdentifier Ident
  | RedefinedIdentifier Ident
  | TypeMismatch String
  | InvalidOperation String
  deriving (Show, Eq)

type SemanticAnalyzer = StateT SemanticState (Either SemanticError)

-- 初始化语义分析状态
initSemanticState :: SemanticState
initSemanticState = SemanticState Map.empty 0 3 [] 0

-- 生成指令
emit :: Instruction -> SemanticAnalyzer ()
emit instr = do
  state <- get
  put
    state
      { instructions = instructions state ++ [instr],
        nextInstrAddr = nextInstrAddr state + 1
      }

-- 查找符号
lookupSymbol :: Ident -> SemanticAnalyzer (Maybe SymbolEntry)
lookupSymbol ident = do
  state <- get
  return $ Map.lookup ident (symbolTable state)

-- 插入符号
insertSymbol :: Ident -> SymbolEntry -> SemanticAnalyzer ()
insertSymbol ident entry = do
  state <- get
  case Map.lookup ident (symbolTable state) of
    Just _ -> throwError (RedefinedIdentifier ident)
    Nothing -> put state {symbolTable = Map.insert ident entry (symbolTable state)}

-- 分析程序
analyzeProgram :: Program -> Either SemanticError [Instruction]
analyzeProgram (Program subprog) = do
  (_, finalState) <- runStateT (analyzeSubprogram subprog >> emit (OPR (fromEnum OPR_RET))) initSemanticState
  return $ instructions finalState

-- 分析子程序
analyzeSubprogram :: Subprogram -> SemanticAnalyzer ()
analyzeSubprogram subprog = do
  state <- get
  let savedAddr = currentAddr state

  -- 预留空间指令位置
  state <- get
  let jmpAddr = nextInstrAddr state
  emit (JMP 0) -- 占位符，稍后回填

  -- 分析声明
  maybe (return ()) analyzeConstDecl (constDecl subprog)
  maybe (return ()) analyzeVarDecl (varDecl subprog)
  maybe (return ()) analyzeProcDecl (procDecl subprog)

  -- 回填跳转地址
  state' <- get
  let jumpAddr = nextInstrAddr state'
  let instrs = instructions state'
  let newInstrs = take jmpAddr instrs ++ [JMP jumpAddr] ++ drop (jmpAddr + 1) instrs
  put state' {instructions = newInstrs}

  -- 分配数据空间
  state' <- get
  let dataSpace = currentAddr state' -- FIXED: Access currentAddr as a field
  emit (INT dataSpace)

  -- 分析语句
  analyzeStmt (stmt subprog)

-- 分析常量声明
analyzeConstDecl :: ConstDecl -> SemanticAnalyzer ()
analyzeConstDecl (ConstDecl constDefis) = mapM_ analyzeConstDefi constDefis

analyzeConstDefi :: ConstDefi -> SemanticAnalyzer ()
analyzeConstDefi (ConstDefi ident val) = do
  insertSymbol ident (ConstEntry val)

-- 分析变量声明
analyzeVarDecl :: VarDecl -> SemanticAnalyzer ()
analyzeVarDecl (VarDecl idents) = mapM_ analyzeVarIdent idents

analyzeVarIdent :: Ident -> SemanticAnalyzer ()
analyzeVarIdent ident = do
  state <- get
  let level = currentLevel state
  let addr = currentAddr state
  insertSymbol ident (VarEntry level addr)
  put state {currentAddr = addr + 1}

-- 分析过程声明
analyzeProcDecl :: ProcDecl -> SemanticAnalyzer ()
analyzeProcDecl (ProcDecl header subprog moreProcs) = do
  analyzeProcHeader header
  mapM_ analyzeProcDecl moreProcs
  analyzeSubprogram subprog
  emit (OPR (fromEnum OPR_RET))

analyzeProcHeader :: ProcHeader -> SemanticAnalyzer ()
analyzeProcHeader (ProcHeader ident) = do
  state <- get
  let level = currentLevel state
  let addr = nextInstrAddr state
  insertSymbol ident (ProcEntry level addr)
  put state {currentLevel = level + 1, currentAddr = 3}

-- 分析语句
analyzeStmt :: Stmt -> SemanticAnalyzer ()
analyzeStmt stmt = case stmt of
  AssignStmt ident expr -> do
    entry <- lookupSymbol ident
    case entry of
      Just (VarEntry level addr) -> do
        analyzeExpr expr
        emit (STO level addr)
      Just (ConstEntry _) -> throwError (TypeMismatch "Cannot assign to constant")
      Nothing -> throwError (UndefinedIdentifier ident)
      _ -> throwError (TypeMismatch "Invalid assignment target")
  CondStmt cond stmt -> do
    analyzeCond cond
    state <- get
    let jpcAddr = nextInstrAddr state
    emit (JPC 0) -- 占位符
    analyzeStmt stmt
    state' <- get
    let jumpAddr = nextInstrAddr state'
    -- 回填条件跳转地址
    let instrs = instructions state'
    let newInstrs = take jpcAddr instrs ++ [JPC jumpAddr] ++ drop (jpcAddr + 1) instrs
    put state' {instructions = newInstrs}
  LoopStmt cond stmt -> do
    state <- get
    let loopStart = nextInstrAddr state
    analyzeCond cond
    state' <- get
    let jpcAddr = nextInstrAddr state'
    emit (JPC 0) -- 占位符
    analyzeStmt stmt
    emit (JMP loopStart)
    state'' <- get
    let jumpAddr = nextInstrAddr state''
    -- 回填条件跳转地址
    let instrs = instructions state''
    let newInstrs = take jpcAddr instrs ++ [JPC jumpAddr] ++ drop (jpcAddr + 1) instrs
    put state'' {instructions = newInstrs}
  CallStmt ident -> do
    entry <- lookupSymbol ident
    case entry of
      Just (ProcEntry level addr) -> emit (CAL level addr)
      Nothing -> throwError (UndefinedIdentifier ident)
      _ -> throwError (TypeMismatch "Not a procedure")
  ReadStmt idents -> mapM_ analyzeReadIdent idents
  WriteStmt exprs -> do
    mapM_ analyzeExpr exprs
    mapM_ (\_ -> emit WRT) exprs
  CompoundStmt stmts -> mapM_ analyzeStmt stmts
  NullStmt -> return ()

-- 分析读语句中的标识符
analyzeReadIdent :: Ident -> SemanticAnalyzer ()
analyzeReadIdent ident = do
  entry <- lookupSymbol ident
  case entry of
    Just (VarEntry level addr) -> emit (RED level addr)
    Nothing -> throwError (UndefinedIdentifier ident)
    _ -> throwError (TypeMismatch "Cannot read into non-variable")

-- 分析条件
analyzeCond :: Cond -> SemanticAnalyzer ()
analyzeCond cond = case cond of
  Cond expr1 op expr2 -> do
    analyzeExpr expr1
    analyzeExpr expr2
    case op of
      Eq -> emit (OPR (fromEnum OPR_EQU))
      NotEq -> emit (OPR (fromEnum OPR_NEQ))
      LessThan -> emit (OPR (fromEnum OPR_LSS))
      LessEqThan -> emit (OPR (fromEnum OPR_LEQ))
      GreaterThan -> emit (OPR (fromEnum OPR_GTR))
      GreaterEqThan -> emit (OPR (fromEnum OPR_GEQ))
  OddCond expr -> do
    analyzeExpr expr
    emit (OPR (fromEnum OPR_ODD))

-- 分析表达式
analyzeExpr :: Expr -> SemanticAnalyzer ()
analyzeExpr (Expr maybeSign item items) = do
  analyzeItem item
  case maybeSign of
    Just Negative -> emit (OPR (fromEnum OPR_NEG))
    _ -> return ()
  mapM_ analyzeAddSubItem items
  where
    analyzeAddSubItem (op, item) = do
      analyzeItem item
      case op of
        Add -> emit (OPR (fromEnum OPR_ADD))
        Sub -> emit (OPR (fromEnum OPR_SUB))

-- 分析项
analyzeItem :: Item -> SemanticAnalyzer ()
analyzeItem (Item factor factors) = do
  analyzeFactor factor
  mapM_ analyzeMulDivFactor factors
  where
    analyzeMulDivFactor (op, factor) = do
      analyzeFactor factor
      case op of
        Mul -> emit (OPR (fromEnum OPR_MUL))
        Div -> emit (OPR (fromEnum OPR_DIV))

-- 分析因子
analyzeFactor :: Factor -> SemanticAnalyzer ()
analyzeFactor factor = case factor of
  Identifier ident -> do
    entry <- lookupSymbol ident
    case entry of
      Just (ConstEntry val) -> emit (LIT val)
      Just (VarEntry level addr) -> emit (LOD level addr)
      Nothing -> throwError (UndefinedIdentifier ident)
      _ -> throwError (TypeMismatch "Invalid factor")
  Number val -> emit (LIT val)
  ParenedExpr expr -> analyzeExpr expr

-- 虚拟机状态
data VMState = VMState
  { stack :: [Int],
    base :: Int,
    top :: Int,
    pc :: Int,
    code :: [Instruction],
    running :: Bool,
    output :: [String]
  }
  deriving (Show)

-- 初始化虚拟机状态
initVMState :: [Instruction] -> VMState
initVMState instrs = VMState [] 0 0 0 instrs True []

-- 虚拟机运行
runVM :: [Instruction] -> IO [String]
runVM instrs = do
  finalState <- runVMLoop (initVMState instrs)
  return $ reverse $ output finalState

runVMLoop :: VMState -> IO VMState
runVMLoop state
  | not (running state) = return state
  | pc state >= length (code state) = return state {running = False}
  | otherwise = do
      newState <- executeInstruction state
      runVMLoop newState

-- 执行指令
executeInstruction :: VMState -> IO VMState
executeInstruction state = do
  let instr = code state !! pc state
  case instr of
    LIT val ->
      return
        state
          { stack = val : stack state,
            top = top state + 1,
            pc = pc state + 1
          }
    LOD level addr -> do
      let baseAddr = findBase (base state) level (stack state)
      let val = stack state !! (baseAddr + addr)
      return
        state
          { stack = val : stack state,
            top = top state + 1,
            pc = pc state + 1
          }
    STO level addr -> do
      let baseAddr = findBase (base state) level (stack state)
      let val = head (stack state)
      let newStack =
            take (baseAddr + addr) (stack state)
              ++ [val]
              ++ drop (baseAddr + addr + 1) (stack state)
      return
        state
          { stack = tail (stack state),
            top = top state - 1,
            pc = pc state + 1
          }
    CAL level addr ->
      return
        state
          { stack = (pc state + 1) : base state : stack state,
            base = top state,
            top = top state + 2,
            pc = addr
          }
    INT space ->
      return
        state
          { stack = replicate space 0 ++ stack state,
            top = top state + space,
            pc = pc state + 1
          }
    JMP addr -> return state {pc = addr}
    JPC addr ->
      if head (stack state) == 0
        then
          return
            state
              { stack = tail (stack state),
                top = top state - 1,
                pc = addr
              }
        else
          return
            state
              { stack = tail (stack state),
                top = top state - 1,
                pc = pc state + 1
              }
    OPR op -> executeOPR state op
    RED level addr -> do
      putStr "Input: "
      input <- getLine
      let val = read input :: Int
      let baseAddr = findBase (base state) level (stack state)
      let newStack =
            take (baseAddr + addr) (stack state)
              ++ [val]
              ++ drop (baseAddr + addr + 1) (stack state)
      return
        state
          { stack = newStack,
            pc = pc state + 1
          }
    WRT -> do
      let val = head (stack state)
      let outStr = show val
      putStrLn outStr
      return
        state
          { stack = tail (stack state),
            top = top state - 1,
            pc = pc state + 1,
            output = outStr : output state
          }
    RET -> do
      let retAddr = stack state !! (base state)
      let oldBase = stack state !! (base state - 1)
      return
        state
          { stack = drop (base state) (stack state),
            base = oldBase,
            top = base state,
            pc = retAddr
          }

-- 执行运算指令
executeOPR :: VMState -> Int -> IO VMState
executeOPR state op
  | op == fromEnum OPR_RET = return state {running = False}
  | op == fromEnum OPR_NEG = do
      let val = head (stack state)
      return
        state
          { stack = (-val) : tail (stack state),
            pc = pc state + 1
          }
  | op == fromEnum OPR_ADD = do
      let val1 = head (stack state)
      let val2 = head (tail (stack state))
      return
        state
          { stack = (val2 + val1) : drop 2 (stack state),
            top = top state - 1,
            pc = pc state + 1
          }
  | op == fromEnum OPR_SUB = do
      let val1 = head (stack state)
      let val2 = head (tail (stack state))
      return
        state
          { stack = (val2 - val1) : drop 2 (stack state),
            top = top state - 1,
            pc = pc state + 1
          }
  | op == fromEnum OPR_MUL = do
      let val1 = head (stack state)
      let val2 = head (tail (stack state))
      return
        state
          { stack = (val2 * val1) : drop 2 (stack state),
            top = top state - 1,
            pc = pc state + 1
          }
  | op == fromEnum OPR_DIV = do
      let val1 = head (stack state)
      let val2 = head (tail (stack state))
      return
        state
          { stack = (val2 `div` val1) : drop 2 (stack state),
            top = top state - 1,
            pc = pc state + 1
          }
  | op == fromEnum OPR_ODD = do
      let val = head (stack state)
      let result = if val `mod` 2 == 1 then 1 else 0
      return
        state
          { stack = result : tail (stack state),
            pc = pc state + 1
          }
  | op == fromEnum OPR_EQU = do
      let val1 = head (stack state)
      let val2 = head (tail (stack state))
      let result = if val2 == val1 then 1 else 0
      return
        state
          { stack = result : drop 2 (stack state),
            top = top state - 1,
            pc = pc state + 1
          }
  | op == fromEnum OPR_NEQ = do
      let val1 = head (stack state)
      let val2 = head (tail (stack state))
      let result = if val2 /= val1 then 1 else 0
      return
        state
          { stack = result : drop 2 (stack state),
            top = top state - 1,
            pc = pc state + 1
          }
  | op == fromEnum OPR_LSS = do
      let val1 = head (stack state)
      let val2 = head (tail (stack state))
      let result = if val2 < val1 then 1 else 0
      return
        state
          { stack = result : drop 2 (stack state),
            top = top state - 1,
            pc = pc state + 1
          }
  | op == fromEnum OPR_LEQ = do
      let val1 = head (stack state)
      let val2 = head (tail (stack state))
      let result = if val2 <= val1 then 1 else 0
      return
        state
          { stack = result : drop 2 (stack state),
            top = top state - 1,
            pc = pc state + 1
          }
  | op == fromEnum OPR_GTR = do
      let val1 = head (stack state)
      let val2 = head (tail (stack state))
      let result = if val2 > val1 then 1 else 0
      return
        state
          { stack = result : drop 2 (stack state),
            top = top state - 1,
            pc = pc state + 1
          }
  | op == fromEnum OPR_GEQ = do
      let val1 = head (stack state)
      let val2 = head (tail (stack state))
      let result = if val2 >= val1 then 1 else 0
      return
        state
          { stack = result : drop 2 (stack state),
            top = top state - 1,
            pc = pc state + 1
          }
  | otherwise = return state {pc = pc state + 1}

-- 查找基址
findBase :: Int -> Int -> [Int] -> Int
findBase base 0 _ = base
findBase base level stack = findBase (stack !! (base - 1)) (level - 1) stack

-- 编译和运行 PL/0 程序
compilePL0 :: Program -> IO ()
compilePL0 program = do
  case analyzeProgram program of
    Left err -> putStrLn $ "Semantic Error: " ++ show err
    Right instructions -> do
      putStrLn "Generated Instructions:"
      mapM_ (putStrLn . show) instructions
      putStrLn "\nRunning Program:"
      _ <- runVM instructions
      return ()

-- 示例程序
exampleProgram :: Program
exampleProgram =
  Program $
    Subprogram
      { constDecl = Just $ ConstDecl [ConstDefi "n" 10],
        varDecl = Just $ VarDecl ["x", "y"],
        procDecl = Nothing,
        stmt =
          CompoundStmt
            []
      }

-- 测试函数
testPL0 :: IO ()
testPL0 = compilePL0 exampleProgram
