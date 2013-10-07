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
  describe "runAssembler" $ do
    let labelMap = fromList [("foo", AddressValue 1), ("bar", AddressValue 2)]

    it "assembles a 'LOAD sX, sY' instruction" $ do
      let result = runAssembler [(BinaryInstruction "load" (RegisterOperand Register0) (RegisterOperand Register1))] labelMap
      result `shouldBe` [0x00010]

    it "assembles a 'LOAD sX, kk' instruction" $ do
      let result = runAssembler [(BinaryInstruction "load" (RegisterOperand Register0) (DataOperand 0xFF))] labelMap
      result `shouldBe` [0x010FF]

    it "assembles a 'AND sX, sY' instruction" $ do
      let result = runAssembler [(BinaryInstruction "and" (RegisterOperand Register0) (RegisterOperand Register1))] labelMap
      result `shouldBe` [0x02010]

    it "assembles a 'AND sX, kk' instruction" $ do
      let result = runAssembler [(BinaryInstruction "and" (RegisterOperand Register0) (DataOperand 0xFF))] labelMap
      result `shouldBe` [0x030FF]

    it "assembles a 'OR sX, sY' instruction" $ do
      let result = runAssembler [(BinaryInstruction "or" (RegisterOperand Register0) (RegisterOperand Register1))] labelMap
      result `shouldBe` [0x04010]

    it "assembles a 'OR sX, kk' instruction" $ do
      let result = runAssembler [(BinaryInstruction "or" (RegisterOperand Register0) (DataOperand 0xFF))] labelMap
      result `shouldBe` [0x050FF]

    it "assembles a 'XOR sX, sY' instruction" $ do
      let result = runAssembler [(BinaryInstruction "xor" (RegisterOperand Register0) (RegisterOperand Register1))] labelMap
      result `shouldBe` [0x06010]

    it "assembles a 'XOR sX, kk' instruction" $ do
      let result = runAssembler [(BinaryInstruction "xor" (RegisterOperand Register0) (DataOperand 0xFF))] labelMap
      result `shouldBe` [0x070FF]

    it "assembles a 'SL0 sX' instruction" $ do
      let result = runAssembler [(UnaryInstruction "sl0" (RegisterOperand Register0))] labelMap
      result `shouldBe` [0x14006]

    it "assembles a 'SL1 sX' instruction" $ do
      let result = runAssembler [(UnaryInstruction "sl1" (RegisterOperand Register0))] labelMap
      result `shouldBe` [0x14007]

    it "assembles a 'CALL aaa' instruction" $ do
      let result = runAssembler [(UnaryInstruction "call" (AddressOperand 0xFFF))] labelMap
      result `shouldBe` [0x20FFF]

    it "assembles a 'CALL label' instruction" $ do
      let result = runAssembler [(UnaryInstruction "call" (LabelOperand "foo"))] labelMap
      result `shouldBe` [0x20001]
