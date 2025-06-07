module Main where

import Compiler (compile)
import Data.List (intercalate)
import qualified Data.Map as Map

main :: IO ()
main = do
  src <- getContents
  let (quads, _, table) = compile src False
  putStrLn "Symbol Table:"
  mapM_ print (Map.toList table)
  putStrLn "Generated Quadruples:"
  mapM_ print quads