module Internal.Parser where

import           Data.Char                     (digitToInt)
import           Numeric                       (readHex, readInt, readOct)
import           Text.ParserCombinators.Parsec (Parser, anyChar, between, char,
                                                choice, digit, letter, many,
                                                many1, noneOf, oneOf, parse,
                                                skipMany1, space, string, try,
                                                (<|>))

data LispVal = Atom String
             | Bool Bool
             | Character Char
             | DottedList [LispVal] LispVal
             | List [LispVal]
             | Number Integer
             | String String
  deriving (Eq, Show)

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

escCode :: Parser Char
escCode =
  char '\\' >>
  (choice . map (\(c, r) -> char c >> return r))
    [('\'', '\''), ('"', '"'), ('\\', '\\'), ('f', '\f'), ('n', '\n'), ('r', '\r'), ('t', '\t')]

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return (Atom atom)

parseBool :: Parser LispVal
parseBool = parseBool' 't' True <|> parseBool' 'f' False
  where
    parseBool' c r = try (string ['#', c]) >> return (Bool r)

parseCharacter :: Parser LispVal
parseCharacter = fmap Character (between (char '\'') (char '\'') (escCode <|> noneOf "\'"))

-- parseDottedList :: Parser LispVal parseList :: Parser LispVal
parseNumber :: Parser LispVal
-- TODO parse for #b -> binary, #o -> octal, #d -> decimal, -> #x -> hex readInt 2 (flip elem "01")
-- digitToInt readOct readHex
parseNumber = parseBinary <|> parseDecimal1 <|> parseDecimal2
  where
    parseDecimal1 = (fmap (Number . read) (many1 digit))
    parseDecimal2 = try (string "#d") >> parseDecimal1
    parseBinary = do
      try (string "#b")
      n <- many1 (oneOf "01")
      return (Number ((fst . head) (readInt 2 (flip elem "01") digitToInt n)))

parseString :: Parser LispVal
parseString = fmap String (between (char '"') (char '"') (many (escCode <|> noneOf "\"")))

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> parseNumber
            <|> parseBool

readExpr :: String -> String
readExpr input =
  case parse parseExpr "slapdash" input of
    Left err  -> "No match: " ++ show err
    Right val -> "Found value"
