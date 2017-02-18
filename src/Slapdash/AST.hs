module Slapdash.AST (LispVal(..)) where

data LispVal = Atom String
             | Bool Bool
             | Character Char
             | DottedList [LispVal] LispVal
             | Float Double
             | List [LispVal]
             | Number Integer
             | String String
  deriving (Eq, Show)
