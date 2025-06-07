module Main where

import Ast
import Compiler
import Lexer (alexScanTokens)
import Parser (ParseResult, parseProgram, parseTree)
import TokenPrinter (printTokens)

printList :: [String] -> IO ()
printList xs = mapM_ putStrLn xs

getParseTree :: ParseResult -> Maybe Ast
getParseTree result = parseTree result

main :: IO ()
main = do
  inputs <- getContents
  let toks = alexScanTokens inputs
  printList $ printTokens toks
  let res = parseProgram toks
  putStrLn $ show res
  let ast = fmap prettyPrint $ getParseTree res
  maybe (putStrLn "No string provided") putStrLn ast
