module Ast where

type Ident = String
type Val = Int

type Ast = Program

newtype Program = Program Subprogram deriving (Show, Eq)

data Subprogram = Subprogram { constDecl :: Maybe ConstDecl
                             , varDecl :: Maybe VarDecl
                             , procDecl :: Maybe ProcDecl
                             , stmt :: Stmt
                             } deriving (Show, Eq)

newtype ConstDecl = ConstDecl [ConstDefi] deriving (Show, Eq)

data ConstDefi = ConstDefi Ident Val deriving (Show, Eq)

newtype VarDecl = VarDecl [Ident] deriving (Show, Eq)

data ProcDecl = ProcDecl { header :: ProcHeader
                         , subprogram :: Subprogram
                         , moreProcs :: [ProcDecl]
                         } deriving (Show, Eq)

newtype ProcHeader = ProcHeader Ident deriving (Show, Eq)

data Stmt = AssignStmt Ident Expr
          | CondStmt Cond Stmt
          | LoopStmt Cond Stmt
          | CallStmt Ident
          | ReadStmt [Ident]
          | WriteStmt [Expr]
          | CompoundStmt [Stmt]
          | NullStmt
          deriving (Show, Eq)

data Cond = Cond Expr RelaOp Expr
          | OddCond Expr
          deriving (Show, Eq)

data Expr = Expr (Maybe Sign) Item [(AddSubOp, Item)] deriving (Show, Eq)

data Sign = Positive | Negative deriving (Show, Eq)

data Item = Item Factor [(MulDivOp, Factor)] deriving (Show, Eq)

data Factor = Identifier Ident | Number Val | ParenedExpr Expr deriving (Show, Eq)

data AddSubOp = Add | Sub deriving (Show, Eq)

data MulDivOp = Mul | Div deriving (Show, Eq)

data RelaOp = Eq | NotEq | LessThan | LessEqThan | GreaterThan | GreaterEqThan deriving (Show, Eq)
