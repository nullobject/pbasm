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
    it "assembles a 'LOAD sX, sY' instruction" $ do
      run (BinaryInstruction "load" (RegisterOperand Register0) (RegisterOperand Register1)) `shouldBe` [0x00010]

    it "assembles a 'LOAD sX, kk' instruction" $ do
      run (BinaryInstruction "load" (RegisterOperand Register0) (DataOperand 0xFF)) `shouldBe` [0x010FF]
      run (BinaryInstruction "load" (RegisterOperand Register0) (IdentifierOperand "lorem")) `shouldBe` [0x01001]

    it "assembles a 'AND sX, sY' instruction" $ do
      run (BinaryInstruction "and" (RegisterOperand Register0) (RegisterOperand Register1)) `shouldBe` [0x02010]

    it "assembles a 'AND sX, kk' instruction" $ do
      run (BinaryInstruction "and" (RegisterOperand Register0) (DataOperand 0xFF)) `shouldBe` [0x030FF]
      run (BinaryInstruction "and" (RegisterOperand Register0) (IdentifierOperand "lorem")) `shouldBe` [0x03001]

    it "assembles a 'OR sX, sY' instruction" $ do
      run (BinaryInstruction "or" (RegisterOperand Register0) (RegisterOperand Register1)) `shouldBe` [0x04010]

    it "assembles a 'OR sX, kk' instruction" $ do
      run (BinaryInstruction "or" (RegisterOperand Register0) (DataOperand 0xFF)) `shouldBe` [0x050FF]
      run (BinaryInstruction "or" (RegisterOperand Register0) (IdentifierOperand "lorem")) `shouldBe` [0x05001]

    it "assembles a 'XOR sX, sY' instruction" $ do
      run (BinaryInstruction "xor" (RegisterOperand Register0) (RegisterOperand Register1)) `shouldBe` [0x06010]

    it "assembles a 'XOR sX, kk' instruction" $ do
      run (BinaryInstruction "xor" (RegisterOperand Register0) (DataOperand 0xFF)) `shouldBe` [0x070FF]
      run (BinaryInstruction "xor" (RegisterOperand Register0) (IdentifierOperand "lorem")) `shouldBe` [0x07001]

    it "assembles a 'ADD sX, sY' instruction" $ do
      run (BinaryInstruction "add" (RegisterOperand Register0) (RegisterOperand Register1)) `shouldBe` [0x10010]

    it "assembles a 'ADD sX, kk' instruction" $ do
      run (BinaryInstruction "add" (RegisterOperand Register0) (DataOperand 0xFF)) `shouldBe` [0x110FF]
      run (BinaryInstruction "add" (RegisterOperand Register0) (IdentifierOperand "lorem")) `shouldBe` [0x11001]

    it "assembles a 'ADDCY sX, sY' instruction" $ do
      run (BinaryInstruction "addcy" (RegisterOperand Register0) (RegisterOperand Register1)) `shouldBe` [0x12010]

    it "assembles a 'ADDCY sX, kk' instruction" $ do
      run (BinaryInstruction "addcy" (RegisterOperand Register0) (DataOperand 0xFF)) `shouldBe` [0x130FF]
      run (BinaryInstruction "addcy" (RegisterOperand Register0) (IdentifierOperand "lorem")) `shouldBe` [0x13001]

    it "assembles a 'SUB sX, sY' instruction" $ do
      run (BinaryInstruction "sub" (RegisterOperand Register0) (RegisterOperand Register1)) `shouldBe` [0x18010]

    it "assembles a 'SUB sX, kk' instruction" $ do
      run (BinaryInstruction "sub" (RegisterOperand Register0) (DataOperand 0xFF)) `shouldBe` [0x190FF]
      run (BinaryInstruction "sub" (RegisterOperand Register0) (IdentifierOperand "lorem")) `shouldBe` [0x19001]

    it "assembles a 'SUBCY sX, sY' instruction" $ do
      run (BinaryInstruction "subcy" (RegisterOperand Register0) (RegisterOperand Register1)) `shouldBe` [0x1A010]

    it "assembles a 'SUBCY sX, kk' instruction" $ do
      run (BinaryInstruction "subcy" (RegisterOperand Register0) (DataOperand 0xFF)) `shouldBe` [0x1B0FF]
      run (BinaryInstruction "subcy" (RegisterOperand Register0) (IdentifierOperand "lorem")) `shouldBe` [0x1B001]

    it "assembles a 'SL0 sX' instruction" $ do
      run (UnaryInstruction "sl0" (RegisterOperand Register0)) `shouldBe` [0x14006]

    it "assembles a 'SL1 sX' instruction" $ do
      run (UnaryInstruction "sl1" (RegisterOperand Register0)) `shouldBe` [0x14007]

    it "assembles a 'CALL aaa' instruction" $ do
      run (UnaryInstruction "call" (AddressOperand 0xFFF)) `shouldBe` [0x20FFF]

    it "assembles a 'CALL aaa' instruction with a label" $ do
      run (UnaryInstruction "call" (IdentifierOperand "foo")) `shouldBe` [0x20001]

    it "assembles a 'RETURN' instruction" $ do
      run (NullaryInstruction "return") `shouldBe` [0x25000]

    where
      run i       = runAssembler [i] constantMap labelMap
      constantMap = fromList [("lorem", DataValue 1), ("ipsum", DataValue 2)]
      labelMap    = fromList [("foo", AddressValue 1), ("bar", AddressValue 2)]
