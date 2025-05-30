{
module Calc.Lexer where
import Data.Char (isDigit, isSpace)
}

%wrapper "basic"

tokens :-
  $white+       ; -- 忽略空白字符
  [0-9]+        { \s -> TokenNum (read s) }
  "+"           { \_ -> TokenPlus }
  "-"           { \_ -> TokenMinus }
  "*"           { \_ -> TokenMul }
  "/"           { \_ -> TokenDiv }
  "("           { \_ -> TokenLParen }
  ")"           { \_ -> TokenRParen }

{
data Token
  = TokenNum Int
  | TokenPlus
  | TokenMinus
  | TokenMul
  | TokenDiv
  | TokenLParen
  | TokenRParen
  deriving (Show, Eq)
}
