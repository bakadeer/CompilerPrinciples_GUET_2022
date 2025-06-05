module Parser
  ( parseProgram,
    ParseResult (..),
    ParseError (..),
  )
where

import Ast
  ( AddSubOp (..),
    Ast,
    Cond (..),
    ConstDecl (..),
    ConstDefi (..),
    Expr (..),
    Factor (..),
    Item (..),
    MulDivOp (..),
    ProcDecl (..),
    ProcHeader (..),
    Program (..),
    RelaOp (..),
    Sign (..),
    Stmt (..),
    Subprogram (..),
    VarDecl (..),
  )
import qualified Ast
import Control.Monad.Except
import Control.Monad.State
import Lexer
import Token (Token (..))
import qualified Token

-- 解析错误类型
data ParseError = ParseError
  { errorPos :: AlexPosn,
    errorMsg :: String,
    expectedTokens :: [Token],
    actualToken :: Maybe Token
  }
  deriving (Show, Eq)

-- 解析结果类型
data ParseResult = ParseResult
  { parseTree :: Maybe Ast,
    parseErrors :: [ParseError]
  }
  deriving (Show)

-- 解析器状态
data ParseState = ParseState
  { tokens :: [SpannedToken],
    errors :: [ParseError],
    currentPos :: AlexPosn
  }
  deriving (Show)

-- 解析器单子
type Parser = StateT ParseState (Either String)

-- 初始化解析状态
initParseState :: [SpannedToken] -> ParseState
initParseState ts = ParseState ts [] (AlexPn 0 1 1)

-- 运行解析器
runParser :: Parser (Maybe Ast) -> [SpannedToken] -> ParseResult
runParser parser ts =
  case runStateT parser (initParseState ts) of
    Left err -> ParseResult Nothing [ParseError (AlexPn 0 1 1) err [] Nothing]
    Right (result, state) -> ParseResult result (errors state)

-- 主解析函数
parseProgram :: [SpannedToken] -> ParseResult
parseProgram tokens = runParser program tokens

-- 获取当前token
getCurrentToken :: Parser (Maybe SpannedToken)
getCurrentToken = do
  state <- get
  case tokens state of
    [] -> return Nothing
    (t : _) -> return (Just t)

-- 消费一个token
consumeToken :: Parser (Maybe SpannedToken)
consumeToken = do
  state <- get
  case tokens state of
    [] -> return Nothing
    (t : ts) -> do
      put state {tokens = ts, currentPos = spanOf t}
      return (Just t)

-- 检查当前token是否匹配期望的token
checkToken :: Token -> Parser Bool
checkToken expected = do
  current <- getCurrentToken
  case current of
    Nothing -> return False
    Just (SpannedToken _ tok) -> return (tok == expected)

-- 期望特定token，如果不匹配则报错
expectToken :: Token -> String -> Parser Bool
expectToken expected desc = do
  current <- getCurrentToken
  case current of
    Nothing -> do
      reportError ("Expected " ++ desc ++ " but reached end of input") [expected] Nothing
      return False
    Just st@(SpannedToken pos tok) ->
      if tok == expected
        then do
          consumeToken
          return True
        else do
          reportError ("Expected " ++ desc ++ " but found " ++ show tok) [expected] (Just tok)
          return False

-- 报告错误
reportError :: String -> [Token] -> Maybe Token -> Parser ()
reportError msg expected actual = do
  state <- get
  let err = ParseError (currentPos state) msg expected actual
  put state {errors = err : errors state}

-- 跳过到下一个合适的同步点
synchronize :: [Token] -> Parser ()
synchronize syncTokens = do
  current <- getCurrentToken
  case current of
    Nothing -> return ()
    Just (SpannedToken _ tok) ->
      if tok `elem` syncTokens
        then return ()
        else do
          consumeToken
          synchronize syncTokens

-- 语法规则实现

-- Program ::= Subprogram '.'
program :: Parser (Maybe Program)
program = do
  subprog <- parseSubprogram
  case subprog of
    Nothing -> return Nothing
    Just sp -> do
      success <- expectToken Dot "'.'"
      if success
        then return (Just (Program sp))
        else do
          synchronize []
          return (Just (Program sp))

-- Subprogram ::= [ConstDecl] [VarDecl] [ProcDecl] Statement
parseSubprogram :: Parser (Maybe Subprogram)
parseSubprogram = do
  constD <- optConstDecl
  varD <- optVarDecl
  procD <- optProcDecl
  st <- statement
  case st of
    Nothing -> return Nothing
    Just stmt -> return (Just (Subprogram constD varD procD stmt))

-- 可选的常量声明
optConstDecl :: Parser (Maybe ConstDecl)
optConstDecl = do
  isConst <- checkToken Const
  if isConst
    then parseConstDecl
    else return Nothing

-- ConstDecl ::= 'const' ConstDefi {',' ConstDefi} ';'
parseConstDecl :: Parser (Maybe ConstDecl)
parseConstDecl = do
  expectToken Const "'const'"
  defi <- constDefi
  case defi of
    Nothing -> do
      synchronize [Semicolon, Var, Procedure, Begin, If, While, Call, Read, Write, Token.Identifier ""]
      return Nothing
    Just def -> do
      defis <- moreConstDefis [def]
      expectToken Semicolon "';'"
      return (Just (ConstDecl defis))

-- 更多常量定义
moreConstDefis :: [ConstDefi] -> Parser [ConstDefi]
moreConstDefis acc = do
  isComma <- checkToken Comma
  if isComma
    then do
      consumeToken
      defi <- constDefi
      case defi of
        Nothing -> return acc
        Just def -> moreConstDefis (acc ++ [def])
    else return acc

-- ConstDefi ::= Identifier '=' Number
constDefi :: Parser (Maybe ConstDefi)
constDefi = do
  current <- getCurrentToken
  case current of
    Just (SpannedToken _ (Token.Identifier name)) -> do
      consumeToken
      success <- expectToken Token.Eq "'='"
      if success
        then do
          numToken <- getCurrentToken
          case numToken of
            Just (SpannedToken _ (Integer val)) -> do
              consumeToken
              return (Just (ConstDefi name val))
            _ -> do
              reportError "Expected number" [Integer 0] Nothing
              return Nothing
        else return Nothing
    _ -> do
      reportError "Expected identifier" [Token.Identifier ""] Nothing
      return Nothing

-- 可选的变量声明
optVarDecl :: Parser (Maybe VarDecl)
optVarDecl = do
  isVar <- checkToken Var
  if isVar
    then parseVarDecl
    else return Nothing

-- VarDecl ::= 'var' Identifier {',' Identifier} ';'
parseVarDecl :: Parser (Maybe VarDecl)
parseVarDecl = do
  expectToken Var "'var'"
  current <- getCurrentToken
  case current of
    Just (SpannedToken _ (Token.Identifier name)) -> do
      consumeToken
      names <- moreIdentifiers [name]
      expectToken Semicolon "';'"
      return (Just (VarDecl names))
    _ -> do
      reportError "Expected identifier" [Token.Identifier ""] Nothing
      synchronize [Semicolon, Procedure, Begin, If, While, Call, Read, Write, Token.Identifier ""]
      return Nothing

-- 更多标识符
moreIdentifiers :: [String] -> Parser [String]
moreIdentifiers acc = do
  isComma <- checkToken Comma
  if isComma
    then do
      consumeToken
      current <- getCurrentToken
      case current of
        Just (SpannedToken _ (Token.Identifier name)) -> do
          consumeToken
          moreIdentifiers (acc ++ [name])
        _ -> do
          reportError "Expected identifier" [Token.Identifier ""] Nothing
          return acc
    else return acc

-- 可选的过程声明
optProcDecl :: Parser (Maybe ProcDecl)
optProcDecl = do
  isProc <- checkToken Procedure
  if isProc
    then parseProcDecl
    else return Nothing

-- ProcDecl ::= ProcHeader Subprogram ';' {ProcDecl}
parseProcDecl :: Parser (Maybe ProcDecl)
parseProcDecl = do
  header <- procHeader
  case header of
    Nothing -> return Nothing
    Just hdr -> do
      subprog <- parseSubprogram
      case subprog of
        Nothing -> return Nothing
        Just sp -> do
          expectToken Semicolon "';'"
          moreProcs <- moreProcDecls
          return (Just (ProcDecl hdr sp moreProcs))

-- 更多过程声明
moreProcDecls :: Parser [ProcDecl]
moreProcDecls = do
  isProc <- checkToken Procedure
  if isProc
    then do
      proc <- parseProcDecl
      case proc of
        Nothing -> return []
        Just p -> do
          rest <- moreProcDecls
          return (p : rest)
    else return []

-- ProcHeader ::= 'procedure' Identifier ';'
procHeader :: Parser (Maybe ProcHeader)
procHeader = do
  expectToken Procedure "'procedure'"
  current <- getCurrentToken
  case current of
    Just (SpannedToken _ (Token.Identifier name)) -> do
      consumeToken
      expectToken Semicolon "';'"
      return (Just (ProcHeader name))
    _ -> do
      reportError "Expected identifier" [Token.Identifier ""] Nothing
      return Nothing

-- Statement的解析
statement :: Parser (Maybe Stmt)
statement = do
  current <- getCurrentToken
  case current of
    Nothing -> return (Just NullStmt)
    Just (SpannedToken _ tok) ->
      case tok of
        Token.Identifier name -> assignStatement name
        Token.If -> condStatement
        Token.While -> loopStatement
        Token.Call -> callStatement
        Token.Read -> readStatement
        Token.Write -> writeStatement
        Token.Begin -> compoundStatement
        _ -> return (Just NullStmt)

-- 赋值语句
assignStatement :: String -> Parser (Maybe Stmt)
assignStatement name = do
  consumeToken -- consume identifier
  success <- expectToken ColonEq "':='"
  if success
    then do
      expr <- expression
      case expr of
        Nothing -> return Nothing
        Just e -> return (Just (AssignStmt name e))
    else return Nothing

-- 条件语句
condStatement :: Parser (Maybe Stmt)
condStatement = do
  expectToken If "'if'"
  cond <- condition
  case cond of
    Nothing -> return Nothing
    Just c -> do
      expectToken Then "'then'"
      stmt <- statement
      case stmt of
        Nothing -> return Nothing
        Just s -> return (Just (CondStmt c s))

-- 循环语句
loopStatement :: Parser (Maybe Stmt)
loopStatement = do
  expectToken While "'while'"
  cond <- condition
  case cond of
    Nothing -> return Nothing
    Just c -> do
      expectToken Do "'do'"
      stmt <- statement
      case stmt of
        Nothing -> return Nothing
        Just s -> return (Just (LoopStmt c s))

-- 调用语句
callStatement :: Parser (Maybe Stmt)
callStatement = do
  expectToken Call "'call'"
  current <- getCurrentToken
  case current of
    Just (SpannedToken _ (Token.Identifier name)) -> do
      consumeToken
      return (Just (CallStmt name))
    _ -> do
      reportError "Expected identifier" [Token.Identifier ""] Nothing
      return Nothing

-- 读语句
readStatement :: Parser (Maybe Stmt)
readStatement = do
  expectToken Read "'read'"
  expectToken LeftParen "'('"
  current <- getCurrentToken
  case current of
    Just (SpannedToken _ (Token.Identifier name)) -> do
      consumeToken
      names <- moreIdentifiers [name]
      expectToken RightParen "')'"
      return (Just (ReadStmt names))
    _ -> do
      reportError "Expected identifier" [Token.Identifier ""] Nothing
      return Nothing

-- 写语句
writeStatement :: Parser (Maybe Stmt)
writeStatement = do
  expectToken Write "'write'"
  expectToken LeftParen "'('"
  expr <- expression
  case expr of
    Nothing -> return Nothing
    Just e -> do
      exprs <- moreExpressions [e]
      expectToken RightParen "')'"
      return (Just (WriteStmt exprs))

-- 更多表达式
moreExpressions :: [Expr] -> Parser [Expr]
moreExpressions acc = do
  isComma <- checkToken Comma
  if isComma
    then do
      consumeToken
      expr <- expression
      case expr of
        Nothing -> return acc
        Just e -> moreExpressions (acc ++ [e])
    else return acc

-- 复合语句
compoundStatement :: Parser (Maybe Stmt)
compoundStatement = do
  expectToken Begin "'begin'"
  stmt <- statement
  case stmt of
    Nothing -> return Nothing
    Just s -> do
      stmts <- moreStatements [s]
      expectToken End "'end'"
      return (Just (CompoundStmt stmts))

-- 更多语句
moreStatements :: [Stmt] -> Parser [Stmt]
moreStatements acc = do
  isSemi <- checkToken Semicolon
  if isSemi
    then do
      consumeToken
      stmt <- statement
      case stmt of
        Nothing -> return acc
        Just s -> moreStatements (acc ++ [s])
    else return acc

-- 条件表达式解析
condition :: Parser (Maybe Cond)
condition = do
  isOdd <- checkToken Odd
  if isOdd
    then do
      consumeToken
      expr <- expression
      case expr of
        Nothing -> return Nothing
        Just e -> return (Just (OddCond e))
    else do
      expr1 <- expression
      case expr1 of
        Nothing -> return Nothing
        Just e1 -> do
          op <- relationOp
          case op of
            Nothing -> return Nothing
            Just relOp -> do
              expr2 <- expression
              case expr2 of
                Nothing -> return Nothing
                Just e2 -> return (Just (Cond e1 relOp e2))

-- 关系运算符
relationOp :: Parser (Maybe RelaOp)
relationOp = do
  current <- getCurrentToken
  case current of
    Just (SpannedToken _ tok) -> do
      case tok of
        Token.Eq -> do consumeToken; return (Just Ast.Eq)
        Token.Number -> do consumeToken; return (Just NotEq)
        Token.LessThan -> do consumeToken; return (Just Ast.LessThan)
        Token.LessEqThan -> do consumeToken; return (Just Ast.LessEqThan)
        Token.GreaterThan -> do consumeToken; return (Just Ast.GreaterThan)
        Token.GreaterEqThan -> do consumeToken; return (Just Ast.GreaterEqThan)
        _ -> do
          reportError "Expected relation operator" [Token.Eq, Token.Number, Token.LessThan] (Just tok)
          return Nothing
    Nothing -> do
      reportError "Expected relation operator" [Token.Eq, Token.Number, Token.LessThan] Nothing
      return Nothing

-- 表达式解析
expression :: Parser (Maybe Expr)
expression = do
  sign <- optSign
  item <- term
  case item of
    Nothing -> return Nothing
    Just it -> do
      items <- moreTerms []
      return (Just (Expr sign it items))

-- 可选符号
optSign :: Parser (Maybe Sign)
optSign = do
  current <- getCurrentToken
  case current of
    Just (SpannedToken _ Plus) -> do
      consumeToken
      return (Just Positive)
    Just (SpannedToken _ Minus) -> do
      consumeToken
      return (Just Negative)
    _ -> return Nothing

-- 更多项
moreTerms :: [(AddSubOp, Item)] -> Parser [(AddSubOp, Item)]
moreTerms acc = do
  current <- getCurrentToken
  case current of
    Just (SpannedToken _ Plus) -> do
      consumeToken
      item <- term
      case item of
        Nothing -> return acc
        Just it -> moreTerms (acc ++ [(Add, it)])
    Just (SpannedToken _ Minus) -> do
      consumeToken
      item <- term
      case item of
        Nothing -> return acc
        Just it -> moreTerms (acc ++ [(Sub, it)])
    _ -> return acc

-- 项
term :: Parser (Maybe Item)
term = do
  fact <- factor
  case fact of
    Nothing -> return Nothing
    Just f -> do
      facts <- moreFactors []
      return (Just (Item f facts))

-- 更多因子
moreFactors :: [(MulDivOp, Factor)] -> Parser [(MulDivOp, Factor)]
moreFactors acc = do
  current <- getCurrentToken
  case current of
    Just (SpannedToken _ Star) -> do
      consumeToken
      fact <- factor
      case fact of
        Nothing -> return acc
        Just f -> moreFactors (acc ++ [(Mul, f)])
    Just (SpannedToken _ Slash) -> do
      consumeToken
      fact <- factor
      case fact of
        Nothing -> return acc
        Just f -> moreFactors (acc ++ [(Div, f)])
    _ -> return acc

-- 因子
factor :: Parser (Maybe Factor)
factor = do
  current <- getCurrentToken
  case current of
    Just (SpannedToken _ (Token.Identifier name)) -> do
      consumeToken
      return (Just (Ast.Identifier name))
    Just (SpannedToken _ (Integer val)) -> do
      consumeToken
      return (Just (Ast.Number val))
    Just (SpannedToken _ LeftParen) -> do
      consumeToken
      expr <- expression
      case expr of
        Nothing -> return Nothing
        Just e -> do
          expectToken RightParen "')'"
          return (Just (ParenedExpr e))
    _ -> do
      reportError "Expected identifier, number, or '('" [Token.Identifier "", Integer 0, LeftParen] Nothing
      return Nothing
