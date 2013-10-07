module ParserSpec where

import Core
import Parser

import Data.Map (empty, fromList)
import Test.Hspec
import Text.ParserCombinators.Parsec (runParser)

fromRight :: (Show a) => Either a b -> b
fromRight (Right x) = x
fromRight (Left err) = error $ "no right value" ++ show err

fromLeft :: (Show b) => Either a b -> a
fromLeft (Left x) = x
fromLeft (Right err) = error $ "no left value" ++ show err

parse p input = runParser p parserState "" input

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "addressValue" $ do
    it "parses a lowercase 3 digit hexadecimal string" $ do
      let result = fromRight $ parse addressValue "abc"
      result `shouldBe` 0xABC

    it "parses an uppercase 3 digit hexadecimal string" $ do
      let result = fromRight $ parse addressValue "ABC"
      result `shouldBe` 0xABC

    it "fails with less than 3 digits" $ do
      let result = show $ fromLeft $ parse addressValue "ff"
      result `shouldContain` "expecting address value"

    it "fails with more than 3 digits" $ do
      let result = show $ fromLeft $ parse addressValue "ffff"
      result `shouldContain` "expecting address value"

    it "fails with an invalid hexadecimal string" $ do
      let result = show $ fromLeft $ parse addressValue "zzz"
      result `shouldContain` "expecting address value"

  describe "dataValue" $ do
    it "parses a lowercase 2 digit hexadecimal string" $ do
      let result = fromRight $ parse dataValue "ab"
      result `shouldBe` 0xAB

    it "parses an uppercase 2 digit hexadecimal string" $ do
      let result = fromRight $ parse dataValue "AB"
      result `shouldBe` 0xAB

    it "fails with an invalid hexadecimal string" $ do
      let result = show $ fromLeft $ parse dataValue "zz"
      result `shouldContain` "expecting data value"

    it "fails with less than 2 digits" $ do
      let result = show $ fromLeft $ parse dataValue "f"
      result `shouldContain` "expecting data value"

    it "fails with more than 2 digits" $ do
      let result = show $ fromLeft $ parse dataValue "fff"
      result `shouldContain` "expecting data value"

  describe "register" $ do
    it "parses a lowercase register name" $ do
      let result = fromRight $ parse register "s0"
      result `shouldBe` Register0

    it "parses an uppercase register name" $ do
      let result = fromRight $ parse register "S0"
      result `shouldBe` Register0

    it "fails with an invalid register name" $ do
      let result = show $ fromLeft $ parse register "sz"
      result `shouldContain` "expecting register"

  describe "statement" $ do
    it "parses a constant directive" $ do
      let result = fromRight $ parse statement "CONSTANT foo, ff"
      result `shouldBe` (ConstantDirective "foo" 0xFF)

    it "parses a nullary instruction" $ do
      let result = fromRight $ parse statement "RETURN"
      result `shouldBe` (NullaryInstruction "return")

    it "parses a unary instruction" $ do
      let result = fromRight $ parse statement "CALL fff"
      result `shouldBe` (UnaryInstruction "call" (AddressOperand 0xFFF))

    it "parses a binary instruction" $ do
      let result = fromRight $ parse statement "LOAD s0, s1"
      result `shouldBe` (BinaryInstruction "load" (RegisterOperand Register0) (RegisterOperand Register1))

  describe "statements" $ do
    it "updates the label map" $ do
      let result = fromRight $ parse statements "foo: CALL bar\nbar: RETURN"
      let instructions = [ (UnaryInstruction "call" (LabelOperand "bar"))
                         , (NullaryInstruction "return") ]
      let labelMap = fromList [("foo", 0), ("bar", 1)]
      result `shouldBe` (instructions, empty, labelMap)

    it "updates the constant map" $ do
      let result = fromRight $ parse statements "CONSTANT foo, 00\nCONSTANT bar, 01"
      let directives = [ (ConstantDirective "foo" 0x00)
                       , (ConstantDirective "bar" 0x01) ]
      let constantMap = fromList [("foo", 0), ("bar", 1)]
      result `shouldBe` (directives, constantMap, empty)
