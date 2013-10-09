module ParserSpec where

import Core
import Parser
import Parser.State (parserState)

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
  describe "directive" $ do
    it "parses a constant directive" $ do
      fromRight (parse directive "CONSTANT foo, ff")
        `shouldBe`
        ConstantDirective "foo" 0xFF

  describe "instruction" $ do
    it "parses a binary instruction with 2 register operands" $ do
      fromRight (parse instruction "LOAD s0, s1")
        `shouldBe`
        BinaryInstruction "load" (RegisterOperand Register0) (RegisterOperand Register1)

    it "parses a binary instruction with a register and a value operand" $ do
      fromRight (parse instruction "LOAD s0, ff")
        `shouldBe`
        BinaryInstruction "load" (RegisterOperand Register0) (ValueOperand 0xFF)

    it "parses a binary instruction with a register and an identifier operand" $ do
      fromRight (parse instruction "LOAD s0, foo")
        `shouldBe`
        BinaryInstruction "load" (RegisterOperand Register0) (IdentifierOperand "foo")

    it "parses a unary instruction with a value operand" $ do
      fromRight (parse instruction "CALL fff")
        `shouldBe`
        UnaryInstruction "call" (ValueOperand 0xFFF)

    it "parses a unary instruction with an identifier operand" $ do
      fromRight (parse instruction "CALL foo")
        `shouldBe`
        UnaryInstruction "call" (IdentifierOperand "foo")

    it "parses a nullary instruction" $ do
      fromRight (parse instruction "RETURN")
        `shouldBe`
        NullaryInstruction "return"

  describe "statements" $ do
    it "updates the label map" $ do
      let instructions = [(UnaryInstruction "call" (IdentifierOperand "bar")), (NullaryInstruction "return")]
      let labelMap = fromList [("foo", 0), ("bar", 1)]

      fromRight (parse statements "foo: CALL bar\nbar: RETURN")
        `shouldBe`
        (instructions, empty, labelMap)

    it "updates the constant map" $ do
      let directives = [(ConstantDirective "foo" 0x00), (ConstantDirective "bar" 0x01)]
      let constantMap = fromList [("foo", 0), ("bar", 1)]

      fromRight (parse statements "CONSTANT foo, 00\nCONSTANT bar, 01")
        `shouldBe`
        (directives, constantMap, empty)
