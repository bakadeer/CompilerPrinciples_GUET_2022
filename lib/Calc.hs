module Calc (main) where

import Calc.Parser (parseCalc)

main :: IO ()
main = do
  input <- getLine
  let result = parseCalc input
  print result
