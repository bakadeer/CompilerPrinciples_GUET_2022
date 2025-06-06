module Main where

import Ast
import Compiler
import Lexer (scanTokens)
import Parser (ParseResult, parseProgram, parseTree)
import TokenPrinter (printTokens)

printList :: [String] -> IO ()
printList xs = mapM_ putStrLn xs

getParseTree :: ParseResult -> Maybe Ast
getParseTree result = parseTree result

main :: IO ()
main = do
  inputs <- getContents
  let toks =  scanTokens inputs
  printList $ printTokens toks
  let res = getParseTree $ parseProgram toks
  putStrLn $ show res
