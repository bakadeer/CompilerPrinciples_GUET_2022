module Main where

import Compiler (compile)

main :: IO ()
main = do
  src <- getContents
  print $ compile src False
