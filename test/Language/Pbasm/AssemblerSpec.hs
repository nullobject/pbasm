module Language.Pbasm.AssemblerSpec where

import Data.Map (empty, fromList)
import Test.Hspec

import Language.Pbasm.Assembler
import Language.Pbasm.Core

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "assemblerState" $ do
    it "parses returns an empty assembler state" $ do
      assemblerState
        `shouldBe`
        State { stateLabelMap    = empty
              , stateConstantMap = empty }

  describe "runAssembler" $ do
    context "with a unary instruction" $ do
      it "replaces an identifier operand with the value" $ do
        assemble (UnaryInstruction "call" (IdentifierOperand "foo" Nothing)) `shouldReturn` [0x20001]

    context "with a binary instruction" $ do
      it "replaces an identifier operand with the value" $ do
        assemble (BinaryInstruction "load" (RegisterOperand Register0) (IdentifierOperand "lorem" Nothing)) `shouldReturn` [0x01001]

    context "register loading" $ do
      it "assembles a 'LOAD sX, sY' instruction" $ do
        assemble (BinaryInstruction "load" (RegisterOperand Register0) (RegisterOperand Register1)) `shouldReturn` [0x00010]

      it "assembles a 'LOAD sX, kk' instruction" $ do
        assemble (BinaryInstruction "load" (RegisterOperand Register0) (ValueOperand 0xFF)) `shouldReturn` [0x010FF]

    context "logical" $ do
      it "assembles a 'AND sX, sY' instruction" $ do
        assemble (BinaryInstruction "and" (RegisterOperand Register0) (RegisterOperand Register1)) `shouldReturn` [0x02010]

      it "assembles a 'AND sX, kk' instruction" $ do
        assemble (BinaryInstruction "and" (RegisterOperand Register0) (ValueOperand 0xFF)) `shouldReturn` [0x030FF]

      it "assembles a 'OR sX, sY' instruction" $ do
        assemble (BinaryInstruction "or" (RegisterOperand Register0) (RegisterOperand Register1)) `shouldReturn` [0x04010]

      it "assembles a 'OR sX, kk' instruction" $ do
        assemble (BinaryInstruction "or" (RegisterOperand Register0) (ValueOperand 0xFF)) `shouldReturn` [0x050FF]

      it "assembles a 'XOR sX, sY' instruction" $ do
        assemble (BinaryInstruction "xor" (RegisterOperand Register0) (RegisterOperand Register1)) `shouldReturn` [0x06010]

      it "assembles a 'XOR sX, kk' instruction" $ do
        assemble (BinaryInstruction "xor" (RegisterOperand Register0) (ValueOperand 0xFF)) `shouldReturn` [0x070FF]

    context "arithmetic" $ do
      it "assembles a 'ADD sX, sY' instruction" $ do
        assemble (BinaryInstruction "add" (RegisterOperand Register0) (RegisterOperand Register1)) `shouldReturn` [0x10010]

      it "assembles a 'ADD sX, kk' instruction" $ do
        assemble (BinaryInstruction "add" (RegisterOperand Register0) (ValueOperand 0xFF)) `shouldReturn` [0x110FF]

      it "assembles a 'ADDCY sX, sY' instruction" $ do
        assemble (BinaryInstruction "addcy" (RegisterOperand Register0) (RegisterOperand Register1)) `shouldReturn` [0x12010]

      it "assembles a 'ADDCY sX, kk' instruction" $ do
        assemble (BinaryInstruction "addcy" (RegisterOperand Register0) (ValueOperand 0xFF)) `shouldReturn` [0x130FF]

      it "assembles a 'SUB sX, sY' instruction" $ do
        assemble (BinaryInstruction "sub" (RegisterOperand Register0) (RegisterOperand Register1)) `shouldReturn` [0x18010]

      it "assembles a 'SUB sX, kk' instruction" $ do
        assemble (BinaryInstruction "sub" (RegisterOperand Register0) (ValueOperand 0xFF)) `shouldReturn` [0x190FF]

      it "assembles a 'SUBCY sX, sY' instruction" $ do
        assemble (BinaryInstruction "subcy" (RegisterOperand Register0) (RegisterOperand Register1)) `shouldReturn` [0x1A010]

      it "assembles a 'SUBCY sX, kk' instruction" $ do
        assemble (BinaryInstruction "subcy" (RegisterOperand Register0) (ValueOperand 0xFF)) `shouldReturn` [0x1B0FF]

    context "test and compare" $ do
      it "assembles a 'TEST sX, sY' instruction" $ do
        assemble (BinaryInstruction "test" (RegisterOperand Register0) (RegisterOperand Register1)) `shouldReturn` [0x0C010]

      it "assembles a 'TEST sX, kk' instruction" $ do
        assemble (BinaryInstruction "test" (RegisterOperand Register0) (ValueOperand 0xFF)) `shouldReturn` [0x0D0FF]

      it "assembles a 'TESTCY sX, sY' instruction" $ do
        assemble (BinaryInstruction "testcy" (RegisterOperand Register0) (RegisterOperand Register1)) `shouldReturn` [0x0E010]

      it "assembles a 'TESTCY sX, kk' instruction" $ do
        assemble (BinaryInstruction "testcy" (RegisterOperand Register0) (ValueOperand 0xFF)) `shouldReturn` [0x0F0FF]

      it "assembles a 'COMPARE sX, sY' instruction" $ do
        assemble (BinaryInstruction "compare" (RegisterOperand Register0) (RegisterOperand Register1)) `shouldReturn` [0x1C010]

      it "assembles a 'COMPARE sX, kk' instruction" $ do
        assemble (BinaryInstruction "compare" (RegisterOperand Register0) (ValueOperand 0xFF)) `shouldReturn` [0x1D0FF]

      it "assembles a 'COMPARECY sX, sY' instruction" $ do
        assemble (BinaryInstruction "comparecy" (RegisterOperand Register0) (RegisterOperand Register1)) `shouldReturn` [0x1E010]

      it "assembles a 'COMPARECY sX, kk' instruction" $ do
        assemble (BinaryInstruction "comparecy" (RegisterOperand Register0) (ValueOperand 0xFF)) `shouldReturn` [0x1F0FF]

    context "input and output" $ do
      it "assembles a 'INPUT sX, (sY)' instruction" $ do
        assemble (BinaryInstruction "input" (RegisterOperand Register0) (RegisterOperand Register0)) `shouldReturn` [0x08000]

      it "assembles a 'INPUT sX, pp' instruction" $ do
        assemble (BinaryInstruction "input" (RegisterOperand Register0) (ValueOperand 0xFF)) `shouldReturn` [0x090FF]

      it "assembles a 'OUTPUT sX, (sY)' instruction" $ do
        assemble (BinaryInstruction "output" (RegisterOperand Register0) (RegisterOperand Register0)) `shouldReturn` [0x2C000]

      it "assembles a 'OUTPUT sX, pp' instruction" $ do
        assemble (BinaryInstruction "output" (RegisterOperand Register0) (ValueOperand 0xFF)) `shouldReturn` [0x2D0FF]

      it "assembles a 'OUTPUTK kk, p instruction" $ do
        assemble (BinaryInstruction "outputk" (ValueOperand 0xFF) (ValueOperand 0xF)) `shouldReturn` [0x2BFFF]

    context "scratch pad memory" $ do
      it "assembles a STORE sX, (sY)' instruction" $ do
        assemble (BinaryInstruction "store" (RegisterOperand Register2) (RegisterOperand Register1)) `shouldReturn` [0x2E210]

      it "assembles a STORE sX, sY' instruction" $ do
        assemble (BinaryInstruction "store" (RegisterOperand Register2) (ValueOperand 0xFF)) `shouldReturn` [0x2F2FF]

      it "assembles a FETCH sX, (sY)' instruction" $ do
        assemble (BinaryInstruction "fetch" (RegisterOperand Register2) (RegisterOperand Register1)) `shouldReturn` [0x0A210]

      it "assembles a FETCH sX, sY' instruction" $ do
        assemble (BinaryInstruction "fetch" (RegisterOperand Register2) (ValueOperand 0xFF)) `shouldReturn` [0x0B2FF]

    context "shift and rotate" $ do
      it "assembles a 'SL0 sX' instruction" $ do
        assemble (UnaryInstruction "sl0" (RegisterOperand Register0)) `shouldReturn` [0x14006]

      it "assembles a 'SL1 sX' instruction" $ do
        assemble (UnaryInstruction "sl1" (RegisterOperand Register0)) `shouldReturn` [0x14007]

      it "assembles a 'SLX sX' instruction" $ do
        assemble (UnaryInstruction "slx" (RegisterOperand Register0)) `shouldReturn` [0x14004]

      it "assembles a 'SLA sX' instruction" $ do
        assemble (UnaryInstruction "sla" (RegisterOperand Register0)) `shouldReturn` [0x14000]

      it "assembles a 'RL sX' instruction" $ do
        assemble (UnaryInstruction "rl" (RegisterOperand Register0)) `shouldReturn` [0x14002]

      it "assembles a 'SR0 sX' instruction" $ do
        assemble (UnaryInstruction "sr0" (RegisterOperand Register0)) `shouldReturn` [0x1400E]

      it "assembles a 'SR1 sX' instruction" $ do
        assemble (UnaryInstruction "sr1" (RegisterOperand Register0)) `shouldReturn` [0x1400F]

      it "assembles a 'SRX sX' instruction" $ do
        assemble (UnaryInstruction "srx" (RegisterOperand Register0)) `shouldReturn` [0x1400A]

      it "assembles a 'SRA sX' instruction" $ do
        assemble (UnaryInstruction "sra" (RegisterOperand Register0)) `shouldReturn` [0x14008]

      it "assembles a 'RR sX' instruction" $ do
        assemble (UnaryInstruction "rr" (RegisterOperand Register0)) `shouldReturn` [0x1400C]

    context "jump" $ do
      it "assembles a 'JUMP aaa' instruction" $ do
        assemble (UnaryInstruction "jump" (ValueOperand 0xFFF)) `shouldReturn` [0x22FFF]

      it "assembles a 'JUMP Z, aaa' instruction" $ do
        assemble (BinaryInstruction "jump" (ConditionOperand ZeroCondition) (ValueOperand 0xFFF)) `shouldReturn` [0x32FFF]

      it "assembles a 'JUMP NZ, aaa' instruction" $ do
        assemble (BinaryInstruction "jump" (ConditionOperand NotZeroCondition) (ValueOperand 0xFFF)) `shouldReturn` [0x36FFF]

      it "assembles a 'JUMP C, aaa' instruction" $ do
        assemble (BinaryInstruction "jump" (ConditionOperand CarryCondition) (ValueOperand 0xFFF)) `shouldReturn` [0x3AFFF]

      it "assembles a 'JUMP NC, aaa' instruction" $ do
        assemble (BinaryInstruction "jump" (ConditionOperand NotCarryCondition) (ValueOperand 0xFFF)) `shouldReturn` [0x3EFFF]

    context "subroutines" $ do
      it "assembles a 'CALL aaa' instruction" $ do
        assemble (UnaryInstruction "call" (ValueOperand 0xFFF)) `shouldReturn` [0x20FFF]

      it "assembles a 'RETURN' instruction" $ do
        assemble (NullaryInstruction "return") `shouldReturn` [0x25000]

      it "assembles a 'RETURN Z' instruction" $ do
        assemble (UnaryInstruction "return" (ConditionOperand ZeroCondition)) `shouldReturn` [0x31000]

      it "assembles a 'RETURN NZ' instruction" $ do
        assemble (UnaryInstruction "return" (ConditionOperand NotZeroCondition)) `shouldReturn` [0x35000]

      it "assembles a 'RETURN C instruction" $ do
        assemble (UnaryInstruction "return" (ConditionOperand CarryCondition)) `shouldReturn` [0x39000]

    it "ignores unknown statements" $ do
      assemble (NullaryInstruction "lol") `shouldReturn` []

    where assemble instruction = runAssembler ([instruction], constantMap, labelMap)
          constantMap          = fromList [("lorem", Value 0x01), ("ipsum", Value 0x02)]
          labelMap             = fromList [("foo", Value 0x001), ("bar", Value 0x002), ("baz", Value 0xFFF)]
