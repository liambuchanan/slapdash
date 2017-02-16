module Lib (readExpr) where

import           Text.ParserCombinators.Parsec (Parser, anyChar, between, char,
                                                choice, digit, letter, many,
                                                many1, noneOf, oneOf, parse,
                                                skipMany1, space, string, try,
                                                (<|>))

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

escCode :: Parser Char
escCode = char '\\' >> (choice . map (\(c, r) -> char c >> return r))
                         [ ('\'', '\'')
                         , ('"', '"')
                         , ('\\', '\\')
                         , ('f', '\f')
                         , ('n', '\n')
                         , ('r', '\r')
                         , ('t', '\t')
                         ]

parseCharacter :: Parser LispVal
parseCharacter = fmap Character (between (char '\'') (char '\'') (escCode <|> noneOf "\""))

parseString :: Parser LispVal
parseString = fmap String (between (char '"') (char '"') (many (escCode <|> noneOf "\"")))

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $
    case atom of
      "#t" -> Bool True
      "#f" -> Bool False
      _    -> Atom atom

parseNumber :: Parser LispVal
-- TODO parse for #b -> binary, #o -> octal, #d -> decimal, -> #x -> hex
parseNumber = fmap (Number . read) (many1 digit)

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

readExpr :: String -> String
readExpr input =
  case parse parseExpr "lisp" input of
    Left err  -> "No match: " ++ show err
    Right val -> "Found value"
