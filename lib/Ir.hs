-- Ir.hs
module Ir where

-- 四元式操作符
data Op = Add | Sub | Mul | Div | Assign | Eq | Neq | Lt | Le | Gt | Ge 
        | IfFalseJump | Jump | Call | Label | Write | Read
  deriving (Eq, Show)

-- 四元式结构：(操作符, 操作数1, 操作数2, 结果)
newtype Quad = Quad { getQuad :: (Op, Maybe String, Maybe String, Maybe String) }
  deriving (Eq)

-- 显示四元式
instance Show Quad where
  show (Quad (op, a1, a2, res)) =
    let opStr = case op of
                  Add -> "+"
                  Sub -> "-"
                  Mul -> "*"
                  Div -> "/"
                  Assign -> ":="
                  Eq -> "="
                  Neq -> "#"
                  Lt -> "<"
                  Le -> "<="
                  Gt -> ">"
                  Ge -> ">="
                  IfFalseJump -> "if_false_jump"
                  Jump -> "jump"
                  Call -> "call"
                  Label -> "label"
                  Write -> "write"
                  Read -> "read"
    in "[" ++ opStr ++ ", " ++ showOpt a1 ++ ", " ++ showOpt a2 ++ ", " ++ showOpt res ++ "]"

showOpt :: Show a => Maybe a -> String
showOpt Nothing = "_"
showOpt (Just x) = show x