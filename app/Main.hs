module Main where

import           Slapdash.Parser    (readExpr)
import           System.Environment

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
