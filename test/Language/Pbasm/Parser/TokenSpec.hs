module Language.Pbasm.Parser.TokenSpec where

import Test.Hspec
import Text.Parsec.Error (ParseError)
import Text.ParserCombinators.Parsec (CharParser, runParser)

import Language.Pbasm.Core
import Language.Pbasm.Parser.Token

fromRight :: (Show a) => Either a b -> b
fromRight (Right x) = x
fromRight (Left err) = error $ "no right value" ++ show err

fromLeft :: (Show b) => Either a b -> a
fromLeft (Left x) = x
fromLeft (Right err) = error $ "no left value" ++ show err

parse :: CharParser [Char] a -> [Char] -> Either ParseError a
parse p = runParser p "" ""

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "value" $ do
    it "parses a hexadecimal value" $ do
      fromRight (parse value "01") `shouldBe` 0x01
      fromRight (parse value "ff") `shouldBe` 0xFF

    it "parses a decimal value" $ do
      fromRight (parse value "001'd") `shouldBe` 0x01
      fromRight (parse value "255'd") `shouldBe` 0xFF

    it "parses a binary value" $ do
      fromRight (parse value "1'b") `shouldBe` 0x01
      fromRight (parse value "11111111'b") `shouldBe` 0xFF

    it "parses an ASCII character" $ do
      fromRight (parse value "\"A\"") `shouldBe` 0x41
      fromRight (parse value "\"Z\"") `shouldBe` 0x5A
      fromRight (parse value "\"a\"") `shouldBe` 0x61
      fromRight (parse value "\"z\"") `shouldBe` 0x7A

    it "fails with an invalid value" $ do
      let result = show $ fromLeft $ parse value "lol"
      result `shouldContain` "expecting value"

  describe "register" $ do
    it "parses a register" $ do
      fromRight (parse register "s0") `shouldBe` Register0
      fromRight (parse register "S0") `shouldBe` Register0

    it "fails with an invalid register" $ do
      let result = show $ fromLeft $ parse register "sz"
      result `shouldContain` "expecting register"

  describe "pointer" $ do
    it "parses a pointer" $ do
      fromRight (parse pointer "(s0)") `shouldBe` Register0

    it "fails with an invalid pointer" $ do
      let result = show $ fromLeft $ parse pointer "(sz)"
      result `shouldContain` "expecting pointer"

  describe "condition" $ do
    it "parses a condition" $ do
      fromRight (parse condition "z") `shouldBe` ZeroCondition
      fromRight (parse condition "nz") `shouldBe` NotZeroCondition
      fromRight (parse condition "c") `shouldBe` CarryCondition
      fromRight (parse condition "nc") `shouldBe` NotCarryCondition

    it "fails with an invalid condition" $ do
      let result = show $ fromLeft $ parse condition "lol"
      result `shouldContain` "expecting condition"

  describe "operand" $ do
    it "parses a identifier operand" $ do
      fromRight (parse operand "foo") `shouldBe` IdentifierOperand "foo" Nothing

    it "parses a inverted identifier operand" $ do
      fromRight (parse operand "~foo") `shouldBe` IdentifierOperand "foo" (Just InvertModifier)

    it "parses a lower identifier operand" $ do
      fromRight (parse operand "foo'lower") `shouldBe` IdentifierOperand "foo" (Just LowerModifier)

    it "parses a upper identifier operand" $ do
      fromRight (parse operand "foo'upper") `shouldBe` IdentifierOperand "foo" (Just UpperModifier)

    it "parses a register operand" $ do
      fromRight (parse operand "s0") `shouldBe` RegisterOperand Register0
      fromRight (parse operand "(s0)") `shouldBe` RegisterOperand Register0

    it "parses a condition operand" $ do
      fromRight (parse operand "z") `shouldBe` ConditionOperand ZeroCondition

    it "parses a value operand" $ do
      fromRight (parse operand "ff") `shouldBe` ValueOperand 0xFF
