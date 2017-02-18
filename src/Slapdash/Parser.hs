module Slapdash.Parser (readExpr, parseExpr) where

import           Slapdash.Internal.Parser
import           Text.ParserCombinators.Parsec (parse)

readExpr :: String -> String
readExpr input =
  case parse parseExpr "slapdash" input of
    Left err -> "No match: " ++ show err
    Right _  -> "Found value"
