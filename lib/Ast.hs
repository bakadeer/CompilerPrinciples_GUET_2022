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

-- 常量声明 是一个常量列表
newtype ConstDecl = ConstDecl [ConstDefi] deriving (Show, Eq)

-- 单个常量定义 Ident(常量名) Val(常量值)
data ConstDefi = ConstDefi Ident Val deriving (Show, Eq)

-- 变量声明 是一个Ident列表 
newtype VarDecl = VarDecl [Ident] deriving (Show, Eq)

-- 过程声明，名，子程序，更多过程声明列表
data ProcDecl = ProcDecl { header :: ProcHeader
                         , subprogram :: Subprogram
                         , moreProcs :: [ProcDecl]
                         } deriving (Show, Eq)

-- 子程序名
newtype ProcHeader = ProcHeader Ident deriving (Show, Eq)

-- 语句 枚举构造器
data Stmt = AssignStmt Ident Expr   --赋值语句，由标识符和表达式组成
          | CondStmt Cond Stmt      --条件语句，由条件表达式和语句组成，语句就是acc之后要运行的
          | LoopStmt Cond Stmt      --循环语句，由条件表达式和语句组成
          | CallStmt Ident          --调用语句，由标识符组成
          | ReadStmt [Ident]        --读入语句，由标识符列表组成
          | WriteStmt [Expr]        --输出语句，由表达式列表组成
          | CompoundStmt [Stmt]     --复合语句，由语句列表组成
          | NullStmt                --空语句
          deriving (Show, Eq)

-- 条件
data Cond = Cond Expr RelaOp Expr
          | OddCond Expr
          deriving (Show, Eq)

-- 表达式,由一个可能的正负号+项、一个或多个项和运算符组成
data Expr = Expr (Maybe Sign) Item [(AddSubOp, Item)] deriving (Show, Eq)

-- 符号
data Sign = Positive | Negative deriving (Show, Eq)

-- 项，由符号和因子组成
data Item = Item Factor [(MulDivOp, Factor)] deriving (Show, Eq)

-- 因子，标识符，数字字面量，括号内表达式，是最小的单位
data Factor = Identifier Ident | Number Val | ParenedExpr Expr deriving (Show, Eq)

-- 加减操作符
data AddSubOp = Add | Sub deriving (Show, Eq)

-- 乘除操作符
data MulDivOp = Mul | Div deriving (Show, Eq)

-- 关系操作符
data RelaOp = Eq | NotEq | LessThan | LessEqThan | GreaterThan | GreaterEqThan deriving (Show, Eq)
