module AssemblerSpec where

import Assembler
import Core

import Data.Map (fromList)
import Test.Hspec
import Text.ParserCombinators.Parsec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "assemble" $ do
    let map = fromList [("foo", Address 1), ("bar", Address 2)]

    it "assembles a 'LOAD sX, sY' instruction" $ do
      let result = assemble (BinaryInstruction "load" (RegisterOperand Register0) (RegisterOperand Register1)) map
      result `shouldBe` 0x00010

    it "assembles a 'LOAD sX, kk' instruction" $ do
      let result = assemble (BinaryInstruction "load" (RegisterOperand Register0) (ConstantOperand (Constant 0xFF))) map
      result `shouldBe` 0x010FF

    it "assembles a 'AND sX, sY' instruction" $ do
      let result = assemble (BinaryInstruction "and" (RegisterOperand Register0) (RegisterOperand Register1)) map
      result `shouldBe` 0x02010

    it "assembles a 'AND sX, kk' instruction" $ do
      let result = assemble (BinaryInstruction "and" (RegisterOperand Register0) (ConstantOperand (Constant 0xFF))) map
      result `shouldBe` 0x030FF

    it "assembles a 'OR sX, sY' instruction" $ do
      let result = assemble (BinaryInstruction "or" (RegisterOperand Register0) (RegisterOperand Register1)) map
      result `shouldBe` 0x04010

    it "assembles a 'OR sX, kk' instruction" $ do
      let result = assemble (BinaryInstruction "or" (RegisterOperand Register0) (ConstantOperand (Constant 0xFF))) map
      result `shouldBe` 0x050FF

    it "assembles a 'XOR sX, sY' instruction" $ do
      let result = assemble (BinaryInstruction "xor" (RegisterOperand Register0) (RegisterOperand Register1)) map
      result `shouldBe` 0x06010

    it "assembles a 'XOR sX, kk' instruction" $ do
      let result = assemble (BinaryInstruction "xor" (RegisterOperand Register0) (ConstantOperand (Constant 0xFF))) map
      result `shouldBe` 0x070FF

    it "assembles a 'SL0 sX' instruction" $ do
      let result = assemble (UnaryInstruction "sl0" (RegisterOperand Register0)) map
      result `shouldBe` 0x14006

    it "assembles a 'SL1 sX' instruction" $ do
      let result = assemble (UnaryInstruction "sl1" (RegisterOperand Register0)) map
      result `shouldBe` 0x14007

    it "assembles a 'CALL aaa' instruction" $ do
      let result = assemble (UnaryInstruction "call" (AddressOperand (Address 0xFFF))) map
      result `shouldBe` 0x20FFF

    it "assembles a 'CALL label' instruction" $ do
      let result = assemble (UnaryInstruction "call" (LabelOperand "foo")) map
      result `shouldBe` 0x20001
