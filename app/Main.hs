module Main where

import           Parser             (readExpr)
import           System.Environment

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
