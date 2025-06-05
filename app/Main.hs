module Main where

import Lexer (alexScanTokens)
import Parser (parseProgram)

printList :: (Show a) => [a] -> IO ()
printList xs = mapM_ print xs

main :: IO ()
main = do
  inputs <- getContents
  let toks = alexScanTokens inputs
  printList toks
  let result = parseProgram toks
  putStrLn $ show result
