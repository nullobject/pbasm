module ParserSpec where

import Core
import Parser

import Data.Map (fromList)
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
  describe "address" $ do
    it "parses a lowercase 3 digit hexadecimal string" $ do
      let result = fromRight $ parse address "abc"
      result `shouldBe` 0xABC

    it "parses an uppercase 3 digit hexadecimal string" $ do
      let result = fromRight $ parse address "ABC"
      result `shouldBe` 0xABC

    it "fails with less than 3 digits" $ do
      let result = show $ fromLeft $ parse address "ff"
      result `shouldContain` "expecting address"

    it "fails with more than 3 digits" $ do
      let result = show $ fromLeft $ parse address "ffff"
      result `shouldContain` "expecting address"

    it "fails with an invalid hexadecimal string" $ do
      let result = show $ fromLeft $ parse address "zzz"
      result `shouldContain` "expecting address"

  describe "constant" $ do
    it "parses a lowercase 2 digit hexadecimal string" $ do
      let result = fromRight $ parse constant "ab"
      result `shouldBe` 0xAB

    it "parses an uppercase 2 digit hexadecimal string" $ do
      let result = fromRight $ parse constant "AB"
      result `shouldBe` 0xAB

    it "fails with an invalid hexadecimal string" $ do
      let result = show $ fromLeft $ parse constant "zz"
      result `shouldContain` "expecting constant"

    it "fails with less than 2 digits" $ do
      let result = show $ fromLeft $ parse constant "f"
      result `shouldContain` "expecting constant"

    it "fails with more than 2 digits" $ do
      let result = show $ fromLeft $ parse constant "fff"
      result `shouldContain` "expecting constant"

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

  describe "label" $ do
    it "parses a label" $ do
      let result = fromRight $ parse label "foo:"
      result `shouldBe` "foo"

  describe "statement" $ do
    it "parses statement" $ do
      let result = fromRight $ parse statement "foo: LOAD s0, s1"
      result `shouldBe` (BinaryInstruction "load" (RegisterOperand Register0) (RegisterOperand Register1))

  describe "statements" $ do
    it "parses statements" $ do
      let result = fromRight $ parse statements "LOAD s0, s1\nfoo: CALL bar\nbar: RETURN"
      let instructions = [ (BinaryInstruction "load" (RegisterOperand Register0) (RegisterOperand Register1))
                         , (UnaryInstruction "call" (LabelOperand "bar"))
                         , (NullaryInstruction "return") ]
      let labelMap = fromList [("foo", 1), ("bar", 2)]
      result `shouldBe` (instructions, labelMap)
