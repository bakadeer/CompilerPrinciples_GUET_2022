module Main where

import Compiler (compile)
import Data.List (intercalate)

main :: IO ()
main = do
  src <- getContents
  let (quads, _) = compile src False
  putStrLn "Generated Quadruples:"
  mapM_ print quads