{
module Calc.Lexer (Token(..), alexScanTokens) where
}

%wrapper "basic"

tokens :-

  $white+       ; -- 忽略空白字符
  [0-9]+        { \s -> TokenNum (read s) } -- 匹配整数
  "+"           { \_ -> TokenPlus } -- 加号
  "*"           { \_ -> TokenMult } -- 乘号

{
-- 定义 Token 数据类型
data Token
  = TokenNum Int
  | TokenPlus
  | TokenMult
  deriving (Show, Eq)
}
