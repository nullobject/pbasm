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
    it "parses a hexadecimal value" $ do
      fromRight (parse addressValue "001") `shouldBe` 0x001
      fromRight (parse addressValue "fff") `shouldBe` 0xFFF

    it "parses a decimal value" $ do
      fromRight (parse addressValue "1'd") `shouldBe` 0x001
      fromRight (parse addressValue "4095'd") `shouldBe` 0xFFF

    it "parses a binary value" $ do
      fromRight (parse addressValue "1'b") `shouldBe` 0x001
      fromRight (parse addressValue "111111111111'b") `shouldBe` 0xFFF

    it "fails with an invalid value" $ do
      let result = show $ fromLeft $ parse addressValue "lol"
      result `shouldContain` "expecting address value"

  describe "dataValue" $ do
    it "parses a hexadecimal value" $ do
      fromRight (parse dataValue "01") `shouldBe` 0x01
      fromRight (parse dataValue "ff") `shouldBe` 0xFF

    it "parses a decimal value" $ do
      fromRight (parse dataValue "001'd") `shouldBe` 0x01
      fromRight (parse dataValue "255'd") `shouldBe` 0xFF

    it "parses a binary value" $ do
      fromRight (parse addressValue "1'b") `shouldBe` 0x01
      fromRight (parse addressValue "11111111'b") `shouldBe` 0xFF

    it "fails with an invalid value" $ do
      let result = show $ fromLeft $ parse dataValue "lol"
      result `shouldContain` "expecting data value"

  describe "register" $ do
    it "parses a register name" $ do
      fromRight (parse register "s0") `shouldBe` Register0
      fromRight (parse register "S0") `shouldBe` Register0

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
      let instructions = [ (UnaryInstruction "call" (IdentifierOperand "bar"))
                         , (NullaryInstruction "return") ]
      let labelMap = fromList [("foo", 0), ("bar", 1)]
      result `shouldBe` (instructions, empty, labelMap)

    it "updates the constant map" $ do
      let result = fromRight $ parse statements "CONSTANT foo, 00\nCONSTANT bar, 01"
      let directives = [ (ConstantDirective "foo" 0x00)
                       , (ConstantDirective "bar" 0x01) ]
      let constantMap = fromList [("foo", 0), ("bar", 1)]
      result `shouldBe` (directives, constantMap, empty)
