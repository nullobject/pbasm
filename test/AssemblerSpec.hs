module AssemblerSpec where

import Assembler
import Core

import Test.Hspec
import Text.ParserCombinators.Parsec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "assemble" $ do
    it "assembles a 'LOAD sX, sY' instruction" $ do
      let result = assemble $ LoadInstruction (RegisterOperand Register0) (RegisterOperand Register1)
      result `shouldBe` 0x00010

    it "assembles a 'LOAD sX, kk' instruction" $ do
      let result = assemble $ LoadInstruction (RegisterOperand Register0) (ConstantOperand (Constant 0xFF))
      result `shouldBe` 0x010FF

    it "assembles a 'AND sX, sY' instruction" $ do
      let result = assemble $ AndInstruction (RegisterOperand Register0) (RegisterOperand Register1)
      result `shouldBe` 0x02010

    it "assembles a 'AND sX, kk' instruction" $ do
      let result = assemble $ AndInstruction (RegisterOperand Register0) (ConstantOperand (Constant 0xFF))
      result `shouldBe` 0x030FF

    it "assembles a 'OR sX, sY' instruction" $ do
      let result = assemble $ OrInstruction (RegisterOperand Register0) (RegisterOperand Register1)
      result `shouldBe` 0x04010

    it "assembles a 'OR sX, kk' instruction" $ do
      let result = assemble $ OrInstruction (RegisterOperand Register0) (ConstantOperand (Constant 0xFF))
      result `shouldBe` 0x050FF

    it "assembles a 'XOR sX, sY' instruction" $ do
      let result = assemble $ XorInstruction (RegisterOperand Register0) (RegisterOperand Register1)
      result `shouldBe` 0x06010

    it "assembles a 'XOR sX, kk' instruction" $ do
      let result = assemble $ XorInstruction (RegisterOperand Register0) (ConstantOperand (Constant 0xFF))
      result `shouldBe` 0x070FF

    it "assembles a 'SL0 sX' instruction" $ do
      let result = assemble $ ShiftLeft0Instruction (RegisterOperand Register0)
      result `shouldBe` 0x14006

    it "assembles a 'SL1 sX' instruction" $ do
      let result = assemble $ ShiftLeft1Instruction (RegisterOperand Register0)
      result `shouldBe` 0x14007

    it "assembles a 'CALL aaa' instruction" $ do
      let result = assemble $ CallInstruction (AddressOperand (Address 0xFFF))
      result `shouldBe` 0x20FFF
