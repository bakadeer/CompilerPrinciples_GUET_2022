module Main where

import qualified GCContent (main)
import qualified MyLib (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
  GCContent.main
