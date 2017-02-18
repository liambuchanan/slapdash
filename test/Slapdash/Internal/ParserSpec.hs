module Slapdash.Internal.ParserSpec where

import           Data.Either                   (isLeft)
import           Slapdash.AST
import           Slapdash.Internal.Parser
import           Test.Hspec                    (Spec, describe, it, shouldBe,
                                                shouldSatisfy)
import           Text.ParserCombinators.Parsec (parse)

spec :: Spec
spec = do
  describe "Internal.Parser.parseAtom" $ do
    it "should parse aaaaa to Atom aaaaa" $
      parse parseAtom "" "aaaaa" `shouldBe` Right (Atom "aaaaa")
    it "should parse a$$$$ to Atom a$$$$" $
      parse parseAtom "" "a$$$$" `shouldBe` Right (Atom "a$$$$")
    it "should parse a$123 to Atom a$123" $
      parse parseAtom "" "a$123" `shouldBe` Right (Atom "a$123")
    it "should parse $$$$$ to Atom $$$$" $
      parse parseAtom "" "$$$$$" `shouldBe` Right (Atom "$$$$$")
    it "should parse $a123 to Atom $a123" $
      parse parseAtom "" "$a123" `shouldBe` Right (Atom "$a123")
    it "should not parse to Atom with leading space" $
      parse parseAtom "" " asdasd" `shouldSatisfy` isLeft

  describe "Internal.Parser.parseBool" $ do
    it "should parse #t to Bool True" $
      parse parseBool "" "#t" `shouldBe` Right (Bool True)
    it "should parse #f to Bool False" $
      parse parseBool "" "#f" `shouldBe` Right (Bool False)
    it "should parse #x to error" $
      parse parseBool "" "#x" `shouldSatisfy` isLeft
    it "should parse x to error" $
      parse parseBool "" "x" `shouldSatisfy` isLeft

  describe "Internal.Parser.parseCharacter" $ do
    it "should parse a normal char" $
      parse parseCharacter "" "#\\a" `shouldBe` Right (Character 'a')
    it "should parse #\\newline as the newline char" $
      parse parseCharacter "" "#\\newline" `shouldBe` Right (Character '\n')

  describe "Internal.Parser.parseFloat" $
    it "should parse 1.1" $
      parse parseFloat "" "1.1" `shouldBe` Right (Float 1.1)

  describe "Internal.Parser.parseNumber" $ do
    it "should parse a decimal number" $
      parse parseNumber "" "123" `shouldBe` Right (Number 123)
    it "should parse a decimal number prefixed with #d" $
      parse parseNumber "" "#d123" `shouldBe` Right (Number 123)
    it "should parse a binary number" $
      parse parseNumber "" "#b1111011" `shouldBe` Right (Number 123)
    it "should parse an octal number" $
      parse parseNumber "" "#o173" `shouldBe` Right (Number 123)
    it "should parse a lowercase hexidecimal number" $
      parse parseNumber "" "#h7b" `shouldBe` Right (Number 123)
    it "should parse an uppercase hexidecimal nuber" $
      parse parseNumber "" "#h7B" `shouldBe` Right (Number 123)
