module Main where

import Ast
import Lexer (alexScanTokens)
import Parser (ParseResult (ParseResult, parseTree), parseProgram)
import Semantic (compilePL0, testPL0)

printList :: (Show a) => [a] -> IO ()
printList xs = mapM_ print xs

getParseTree :: ParseResult -> Maybe Ast
getParseTree result = parseTree result

main :: IO ()
main = do
  inputs <- getContents
  let toks = alexScanTokens inputs
  printList toks
  let result = parseProgram toks
  putStrLn $ show result

  
  

