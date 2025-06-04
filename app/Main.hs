module Main where

import Compiler (compile)

main :: IO ()
main = do
  src <- getContents
  mapM_ putStrLn $ compile src False
