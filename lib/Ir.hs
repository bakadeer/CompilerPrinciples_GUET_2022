-- Ir.hs
module Ir where

-- 四元式操作符
data Op = Add | Sub | Mul | Div | Assign 
        | Eq | Neq | Lt | Le | Gt | Ge 
        | JumpEq | JumpNeq | JumpLe | JumpLt | JumpGt | JumpGe
        | Jump | Call | Ret | Label
        | Write | Read
        | Const | Var | Procedure
        | SysStart | SysEnd
        | Test
  deriving (Eq, Show)


-- 四元式结构：(操作符, 操作数1, 操作数2, 结果)
newtype Quad = Quad { getQuad :: (Op, Maybe String, Maybe String, Maybe String) }
  deriving (Eq)

-- 显示四元式
instance Show Quad  where
  show (Quad (op, a1, a2, res)) =
    let opStr = case op of
                  Add -> "+"
                  Sub -> "-"
                  Mul -> "*"
                  Div -> "/"
                  Assign -> "="
                  JumpEq -> "j="
                  JumpNeq -> "j#"
                  JumpLe -> "j<="
                  Jump -> "j"
                  Call -> "call"
                  Ret -> "ret"
                  Write -> "write"
                  Read -> "read"
                  Const -> "const"
                  Var -> "var"
                  Procedure -> "procedure"
                  SysStart -> "syss"
                  SysEnd -> "syse"
                  _ -> show op
        a1Str = maybe "_" id a1
        a2Str = maybe "_" id a2
        resStr = maybe "_" id res
    in  "(" ++ opStr ++ "," ++ a1Str ++ "," ++ a2Str ++ "," ++ resStr ++ ")"

showOpt :: Show a => Maybe a -> String
showOpt Nothing = "_"
showOpt (Just x) = show x