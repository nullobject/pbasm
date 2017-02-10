module Language.Pbasm.ParserSpec where

import Control.Exception (throw)
import Data.Map (empty, fromList)
import Test.Hspec
import Text.ParserCombinators.Parsec (CharParser, runParser)

import Language.Pbasm.Core
import Language.Pbasm.Parser
import Language.Pbasm.Parser.State (State, parserState)

parse :: CharParser State a -> [Char] -> IO a
parse p input = case runParser p parserState "" input of
  Right x -> return x
  Left e  -> throw $ ParserException $ show e

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "statement" $ do
    it "parses a constant directive" $ do
      parse statement "CONSTANT foo, ff"
        `shouldReturn`
        ConstantDirective "foo" 0xFF

    it "parses a binary instruction with 2 register operands" $ do
      parse statement "LOAD s0, s1"
        `shouldReturn`
        BinaryInstruction "load" (RegisterOperand Register0) (RegisterOperand Register1)

    it "parses a binary instruction with a dereferenced register operand" $ do
      parse statement "INPUT s0, (s1)"
        `shouldReturn`
        BinaryInstruction "input" (RegisterOperand Register0) (RegisterOperand Register1)

    it "parses a binary instruction with a register and a value operand" $ do
      parse statement "LOAD s0, ff"
        `shouldReturn`
        BinaryInstruction "load" (RegisterOperand Register0) (ValueOperand 0xFF)

    it "parses a binary instruction with a register and an identifier operand" $ do
      parse statement "LOAD s0, foo"
        `shouldReturn`
        BinaryInstruction "load" (RegisterOperand Register0) (IdentifierOperand "foo" Nothing)

    it "parses a unary instruction with a value operand" $ do
      parse statement "CALL fff"
        `shouldReturn`
        UnaryInstruction "call" (ValueOperand 0xFFF)

    it "parses a unary instruction with an identifier operand" $ do
      parse statement "CALL foo"
        `shouldReturn`
        UnaryInstruction "call" (IdentifierOperand "foo" Nothing)

    it "parses a nullary instruction" $ do
      parse statement "RETURN"
        `shouldReturn`
        NullaryInstruction "return"

    it "raises a parser error with an unknown statement" $ do
      parse statement "LOL"
        `shouldThrow`
        isParserError

  describe "statements" $ do
    it "updates the label map" $ do
      let instructions = [(UnaryInstruction "call" (IdentifierOperand "bar" Nothing)), (NullaryInstruction "return")]
      let labelMap = fromList [("foo", 0), ("bar", 1)]

      parse statements "foo: CALL bar\nbar: RETURN"
        `shouldReturn`
        (instructions, empty, labelMap)

    it "updates the constant map" $ do
      let directives = [(ConstantDirective "foo" 0x00), (ConstantDirective "bar" 0x01)]
      let constantMap = fromList [("foo", 0), ("bar", 1)]

      parse statements "CONSTANT foo, 00\nCONSTANT bar, 01"
        `shouldReturn`
        (directives, constantMap, empty)
