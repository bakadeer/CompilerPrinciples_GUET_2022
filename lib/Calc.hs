module Calc (main) where

import Calc.Lexer
import Calc.Parser
import Data.Char (isSpace)

-- 评估表达式
eval :: Expr -> Int
eval (Num n)       = n
eval (Add e1 e2)   = eval e1 + eval e2
eval (Sub e1 e2)   = eval e1 - eval e2
eval (Mul e1 e2)   = eval e1 * eval e2
eval (Div e1 e2)   = eval e1 `div` eval e2
eval (Neg e)       = -(eval e)

-- 解析并计算
calculate :: String -> Int
calculate input = eval . calc $ alexScanTokens input

main :: IO ()
main = do
  putStrLn "Enter an arithmetic expression (e.g., 2 + 3 * 4):"
  input <- getLine
  let result = calculate input
  putStrLn $ "Result: " ++ show result
