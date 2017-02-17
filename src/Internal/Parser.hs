module Internal.Parser where

-- TODO complex numbers are ratios
import           Data.Char                     (digitToInt)
import           Numeric                       (readHex, readInt, readOct)
import           Text.ParserCombinators.Parsec (Parser, anyChar, between, char,
                                                choice, digit, endBy, hexDigit,
                                                letter, many, many1, noneOf,
                                                octDigit, oneOf, parse, sepBy,
                                                skipMany1, space, string, try,
                                                (<|>))

data LispVal = Atom String
             | Bool Bool
             | Character Char
             | DottedList [LispVal] LispVal
             | Float Double
             | List [LispVal]
             | Number Integer
             | String String
  deriving (Eq, Show)

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

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
parseCharacter = string "#\\" >> (parseSpecial <|> parseCharacter')
  where
    parseSpecial = (choice . map (\(s, r) -> try (string s) >> return (Character r)))
                     [ ("newline", '\n')
                     , ("linefeed", '\n')
                     , ("space", ' ')
                     , ("tab", '\t')
                     , ("return", '\r')
                     ]
    parseCharacter' = Character <$> anyChar

parseDottedList :: Parser LispVal
parseDottedList = DottedList <$> endBy parseExpr spaces <*> (char '.' >> spaces >> parseExpr)

parseFloat :: Parser LispVal
parseFloat = do
  pre <- many1 digit
  char '.'
  post <- many1 digit
  return (Float (read (pre ++ "." ++ post)))

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseNumber :: Parser LispVal
parseNumber = parseBinary <|> parseDecimal1 <|> parseDecimal2 <|> parseOctal <|> parseHexadecimal
  where
    parseDecimal1 = fmap (Number . read) (many1 digit)
    parseDecimal2 = try (string "#d") >> parseDecimal1
    parseBinary = do
      try (string "#b")
      n <- many1 (oneOf "01")
      return (Number ((fst . head) (readInt 2 (`elem` "01") digitToInt n)))
    parseOctal = do
      try (string "#o")
      n <- many1 octDigit
      return (Number ((fst . head) (readOct n)))
    parseHexadecimal = do
      try (string "#h")
      n <- many1 hexDigit
      return (Number ((fst . head) (readHex n)))

parseString :: Parser LispVal
parseString = String <$> between (char '"') (char '"') (many (escCode <|> noneOf "\""))

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> try parseBool
            <|> try parseCharacter
            <|> parseFloat
            <|> try parseNumber
            <|> parseString

readExpr :: String -> String
readExpr input =
  case parse parseExpr "slapdash" input of
    Left err  -> "No match: " ++ show err
    Right val -> "Found value"
