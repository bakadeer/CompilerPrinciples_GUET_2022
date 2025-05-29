module Main where

import qualified CppLexer (main)
import qualified GCContent (main)

processInput :: String -> IO ()
processInput "task1.1" = GCContent.main
processInput "task1.2" = CppLexer.main
processInput _ = putStrLn "the task is not support"

main :: IO ()
main = do
  readFile "meta/task.info" >>= putStr
  input <- getLine
  processInput input
