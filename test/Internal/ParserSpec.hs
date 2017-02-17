module Internal.ParserSpec where

import           Data.Either                   (isLeft)
import           Internal.Parser
import           Test.Hspec                    (describe, hspec, it, shouldBe,
                                                shouldSatisfy)
import           Text.ParserCombinators.Parsec (parse)

spec = do
  describe "Internal.Parser.parseAtom" $ do
    it "should parse aaaaa to Atom aaaaa" $ do
      parse parseAtom "" "aaaaa" `shouldBe` (Right (Atom "aaaaa"))
    it "should parse a$$$$ to Atom a$$$$" $ do
      parse parseAtom "" "a$$$$" `shouldBe` (Right (Atom "a$$$$"))
    it "should parse a$123 to Atom a$123" $ do
      parse parseAtom "" "a$123" `shouldBe` (Right (Atom "a$123"))
    it "should parse $$$$$ to Atom $$$$" $ do
      parse parseAtom "" "$$$$$" `shouldBe` (Right (Atom "$$$$$"))
    it "should parse $a123 to Atom $a123" $ do
      parse parseAtom "" "$a123" `shouldBe` (Right (Atom "$a123"))
    it "should not parse to Atom with leading space" $ do
      parse parseAtom "" " asdasd" `shouldSatisfy` isLeft

  describe "Internal.Parser.parseBool" $ do
    it "should parse #t to Bool True" $ do
      parse parseBool "" "#t" `shouldBe` (Right (Bool True))
    it "should parse #f to Bool False" $ do
      parse parseBool "" "#f" `shouldBe` (Right (Bool False))
    it "should parse #x to error" $ do
      parse parseBool "" "#x" `shouldSatisfy` isLeft
    it "should parse x to error" $ do
      parse parseBool "" "x" `shouldSatisfy` isLeft

  describe "Internal.Parser.parseCharacter" $ do
    it "should parse a normal char" $ do
      parse parseCharacter "" "'a'" `shouldBe` (Right (Character 'a'))
    it "should parse an escaped single quote" $ do
      parse parseCharacter "" "'\\''" `shouldBe` (Right (Character '\''))
    it "should not parse multiple characters" $ do
      parse parseCharacter "" "'asdf'" `shouldSatisfy` isLeft

  describe "Internal.Parser.parseNumber" $ do
    it "should parse a decimal number" $ do
      parse parseNumber "" "123" `shouldBe` (Right (Number 123))
    it "should parse a decimal number prefixed with #d" $ do
      parse parseNumber "" "#d123" `shouldBe` (Right (Number 123))
    it "should parse a binary number" $ do
      parse parseNumber "" "#b1111011" `shouldBe` (Right (Number 123))
    it "should parse an octal number" $ do
      parse parseNumber "" "#o173" `shouldBe` (Right (Number 123))
    it "should parse a lowercase hexidecimal number" $ do
      parse parseNumber "" "#h7b" `shouldBe` (Right (Number 123))
    it "should parse an uppercase hexidecimal nuber" $ do
      parse parseNumber "" "#h7B" `shouldBe` (Right (Number 123))
