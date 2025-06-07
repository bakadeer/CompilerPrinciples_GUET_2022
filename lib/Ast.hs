module Ast where

import Data.List (intercalate)

type Ident = String

type Val = Int

type Ast = Program

newtype Program = Program Subprogram deriving (Show, Eq)

data Subprogram = Subprogram
  { constDecl :: Maybe ConstDecl,
    varDecl :: Maybe VarDecl,
    procDecl :: Maybe ProcDecl,
    stmt :: Stmt
  }
  deriving (Show, Eq)

newtype ConstDecl = ConstDecl [ConstDefi] deriving (Show, Eq)

data ConstDefi = ConstDefi Ident Val deriving (Show, Eq)

newtype VarDecl = VarDecl [Ident] deriving (Show, Eq)

data ProcDecl = ProcDecl
  { header :: ProcHeader,
    subprogram :: Subprogram,
    moreProcs :: [ProcDecl]
  }
  deriving (Show, Eq)

newtype ProcHeader = ProcHeader Ident deriving (Show, Eq)

data Stmt
  = AssignStmt Ident Expr
  | CondStmt Cond Stmt
  | LoopStmt Cond Stmt
  | CallStmt Ident
  | ReadStmt [Ident]
  | WriteStmt [Expr]
  | CompoundStmt [Stmt]
  | NullStmt
  deriving (Show, Eq)

data Cond
  = Cond Expr RelaOp Expr
  | OddCond Expr
  deriving (Show, Eq)

data Expr = Expr (Maybe Sign) Item [(AddSubOp, Item)] deriving (Show, Eq)

data Sign = Positive | Negative deriving (Show, Eq)

data Item = Item Factor [(MulDivOp, Factor)] deriving (Show, Eq)

data Factor = Identifier Ident | Number Val | ParenedExpr Expr deriving (Show, Eq)

data AddSubOp = Add | Sub deriving (Show, Eq)

data MulDivOp = Mul | Div deriving (Show, Eq)

data RelaOp = Eq | NotEq | LessThan | LessEqThan | GreaterThan | GreaterEqThan deriving (Show, Eq)

-- Pretty printing functions
prettyPrint :: Ast -> String
prettyPrint (Program subprog) = prettySubprogram 0 subprog

indent :: Int -> String
indent n = replicate (n * 2) ' '

prettySubprogram :: Int -> Subprogram -> String
prettySubprogram level subprog =
  let parts =
        filter
          (not . null)
          [ maybe "" (prettyConstDecl level) (constDecl subprog),
            maybe "" (prettyVarDecl level) (varDecl subprog),
            maybe "" (prettyProcDecl level) (procDecl subprog),
            prettyStmt level (stmt subprog)
          ]
   in intercalate "\n" parts

prettyConstDecl :: Int -> ConstDecl -> String
prettyConstDecl level (ConstDecl defis) =
  indent level ++ "const " ++ intercalate ", " (map prettyConstDefi defis) ++ ";"

prettyConstDefi :: ConstDefi -> String
prettyConstDefi (ConstDefi ident val) = ident ++ " = " ++ show val

prettyVarDecl :: Int -> VarDecl -> String
prettyVarDecl level (VarDecl idents) =
  indent level ++ "var " ++ intercalate ", " idents ++ ";"

prettyProcDecl :: Int -> ProcDecl -> String
prettyProcDecl level procDecl =
  let procStr =
        prettyProcHeader level (header procDecl)
          ++ "\n"
          ++ prettySubprogram (level + 1) (subprogram procDecl)
          ++ ";"
      moreStr = concatMap (\p -> "\n" ++ prettyProcDecl level p) (moreProcs procDecl)
   in procStr ++ moreStr

prettyProcHeader :: Int -> ProcHeader -> String
prettyProcHeader level (ProcHeader ident) =
  indent level ++ "procedure " ++ ident ++ ";"

prettyStmt :: Int -> Stmt -> String
prettyStmt level stmt = case stmt of
  AssignStmt ident expr ->
    indent level ++ ident ++ " := " ++ prettyExpr expr
  CondStmt cond s ->
    indent level
      ++ "if "
      ++ prettyCond cond
      ++ " then\n"
      ++ prettyStmt (level + 1) s
  LoopStmt cond s ->
    indent level
      ++ "while "
      ++ prettyCond cond
      ++ " do\n"
      ++ prettyStmt (level + 1) s
  CallStmt ident ->
    indent level ++ "call " ++ ident
  ReadStmt idents ->
    indent level ++ "read(" ++ intercalate ", " idents ++ ")"
  WriteStmt exprs ->
    indent level ++ "write(" ++ intercalate ", " (map prettyExpr exprs) ++ ")"
  CompoundStmt stmts ->
    indent level
      ++ "begin\n"
      ++ intercalate ";\n" (map (prettyStmt (level + 1)) stmts)
      ++ (if null stmts then "" else "\n")
      ++ indent level
      ++ "end"
  NullStmt ->
    indent level ++ "skip"

prettyCond :: Cond -> String
prettyCond cond = case cond of
  Cond expr1 op expr2 ->
    prettyExpr expr1 ++ " " ++ prettyRelaOp op ++ " " ++ prettyExpr expr2
  OddCond expr ->
    "odd " ++ prettyExpr expr

prettyExpr :: Expr -> String
prettyExpr (Expr maybeSign item items) =
  let signStr = maybe "" prettySign maybeSign
      itemStr = prettyItem item
      itemsStr = concatMap (\(op, i) -> " " ++ prettyAddSubOp op ++ " " ++ prettyItem i) items
   in signStr ++ itemStr ++ itemsStr

prettySign :: Sign -> String
prettySign Positive = "+"
prettySign Negative = "-"

prettyItem :: Item -> String
prettyItem (Item factor factors) =
  let factorStr = prettyFactor factor
      factorsStr = concatMap (\(op, f) -> " " ++ prettyMulDivOp op ++ " " ++ prettyFactor f) factors
   in factorStr ++ factorsStr

prettyFactor :: Factor -> String
prettyFactor factor = case factor of
  Identifier ident -> ident
  Number val -> show val
  ParenedExpr expr -> "(" ++ prettyExpr expr ++ ")"

prettyAddSubOp :: AddSubOp -> String
prettyAddSubOp Add = "+"
prettyAddSubOp Sub = "-"

prettyMulDivOp :: MulDivOp -> String
prettyMulDivOp Mul = "*"
prettyMulDivOp Div = "/"

prettyRelaOp :: RelaOp -> String
prettyRelaOp Eq = "="
prettyRelaOp NotEq = "<>"
prettyRelaOp LessThan = "<"
prettyRelaOp LessEqThan = "<="
prettyRelaOp GreaterThan = ">"
prettyRelaOp GreaterEqThan = ">="
