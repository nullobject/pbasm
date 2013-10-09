module Parser.TokenSpec where

import Core
import Parser.Token

import Test.Hspec
import Text.ParserCombinators.Parsec (runParser)

fromRight :: (Show a) => Either a b -> b
fromRight (Right x) = x
fromRight (Left err) = error $ "no right value" ++ show err

fromLeft :: (Show b) => Either a b -> a
fromLeft (Left x) = x
fromLeft (Right err) = error $ "no left value" ++ show err

parse p input = runParser p "" "" input

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

    it "fails with an invalid value" $ do
      let result = show $ fromLeft $ parse value "lol"
      result `shouldContain` "expecting value"

  describe "register" $ do
    it "parses a register name" $ do
      fromRight (parse register "s0") `shouldBe` Register0
      fromRight (parse register "S0") `shouldBe` Register0

    it "fails with an invalid register name" $ do
      let result = show $ fromLeft $ parse register "sz"
      result `shouldContain` "expecting register"

  describe "pointer" $ do
    it "parses a pointer" $ do
      fromRight (parse pointer "(s0)") `shouldBe` Register0

    it "fails with an invalid pointer" $ do
      let result = show $ fromLeft $ parse pointer "(sz)"
      result `shouldContain` "expecting pointer"

  describe "operand" $ do
    it "parses a identifier operand" $ do
      fromRight (parse operand "foo") `shouldBe` IdentifierOperand "foo"

    it "parses a register operand" $ do
      fromRight (parse operand "s0") `shouldBe` RegisterOperand Register0
      fromRight (parse operand "(s0)") `shouldBe` RegisterOperand Register0

    it "parses a value operand" $ do
      fromRight (parse operand "ff") `shouldBe` ValueOperand 0xFF
