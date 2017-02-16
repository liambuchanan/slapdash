module Main where

import           System.Environment
import Lib (readExpr)

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
