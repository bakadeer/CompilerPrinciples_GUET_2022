module Main where

import qualified Calc (main)
import qualified CppLexer (main)
import qualified GCContent (main)

processInput :: String -> IO ()
processInput "task1.1" = GCContent.main
processInput "task1.2" = CppLexer.main
processInput "task1.3" = Calc.main
processInput _ = putStrLn "the task is not support"

main :: IO ()
main = do
  readFile "meta/task.info" >>= putStr
  input <- getLine
  processInput input
