module AssemblerSpec where

import Assembler
import Core

import Data.Map (empty, fromList)
import Test.Hspec
import Text.ParserCombinators.Parsec (runParser)

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
    context "register loading" $ do
      it "assembles a 'LOAD sX, sY' instructi[MaMon" $ do
        assemble (BinaryInstruction "load" (RegisterOperand Register0) (RegisterOperand Register1)) `shouldReturn` [0x00010]

      it "assembles a 'LOAD sX, kk' instruction" $ do
        assemble (BinaryInstruction "load" (RegisterOperand Register0) (ValueOperand 0xFF)) `shouldReturn` [0x010FF]
        assemble (BinaryInstruction "load" (RegisterOperand Register0) (IdentifierOperand "lorem")) `shouldReturn` [0x01001]

    context "logical" $ do
      it "assembles a 'AND sX, sY' instruction" $ do
        assemble (BinaryInstruction "and" (RegisterOperand Register0) (RegisterOperand Register1)) `shouldReturn` [0x02010]

      it "assembles a 'AND sX, kk' instruction" $ do
        assemble (BinaryInstruction "and" (RegisterOperand Register0) (ValueOperand 0xFF)) `shouldReturn` [0x030FF]
        assemble (BinaryInstruction "and" (RegisterOperand Register0) (IdentifierOperand "lorem")) `shouldReturn` [0x03001]

      it "assembles a 'OR sX, sY' instruction" $ do
        assemble (BinaryInstruction "or" (RegisterOperand Register0) (RegisterOperand Register1)) `shouldReturn` [0x04010]

      it "assembles a 'OR sX, kk' instruction" $ do
        assemble (BinaryInstruction "or" (RegisterOperand Register0) (ValueOperand 0xFF)) `shouldReturn` [0x050FF]
        assemble (BinaryInstruction "or" (RegisterOperand Register0) (IdentifierOperand "lorem")) `shouldReturn` [0x05001]

      it "assembles a 'XOR sX, sY' instruction" $ do
        assemble (BinaryInstruction "xor" (RegisterOperand Register0) (RegisterOperand Register1)) `shouldReturn` [0x06010]

      it "assembles a 'XOR sX, kk' instruction" $ do
        assemble (BinaryInstruction "xor" (RegisterOperand Register0) (ValueOperand 0xFF)) `shouldReturn` [0x070FF]
        assemble (BinaryInstruction "xor" (RegisterOperand Register0) (IdentifierOperand "lorem")) `shouldReturn` [0x07001]

    context "arithmetic" $ do
      it "assembles a 'ADD sX, sY' instruction" $ do
        assemble (BinaryInstruction "add" (RegisterOperand Register0) (RegisterOperand Register1)) `shouldReturn` [0x10010]

      it "assembles a 'ADD sX, kk' instruction" $ do
        assemble (BinaryInstruction "add" (RegisterOperand Register0) (ValueOperand 0xFF)) `shouldReturn` [0x110FF]
        assemble (BinaryInstruction "add" (RegisterOperand Register0) (IdentifierOperand "lorem")) `shouldReturn` [0x11001]

      it "assembles a 'ADDCY sX, sY' instruction" $ do
        assemble (BinaryInstruction "addcy" (RegisterOperand Register0) (RegisterOperand Register1)) `shouldReturn` [0x12010]

      it "assembles a 'ADDCY sX, kk' instruction" $ do
        assemble (BinaryInstruction "addcy" (RegisterOperand Register0) (ValueOperand 0xFF)) `shouldReturn` [0x130FF]
        assemble (BinaryInstruction "addcy" (RegisterOperand Register0) (IdentifierOperand "lorem")) `shouldReturn` [0x13001]

      it "assembles a 'SUB sX, sY' instruction" $ do
        assemble (BinaryInstruction "sub" (RegisterOperand Register0) (RegisterOperand Register1)) `shouldReturn` [0x18010]

      it "assembles a 'SUB sX, kk' instruction" $ do
        assemble (BinaryInstruction "sub" (RegisterOperand Register0) (ValueOperand 0xFF)) `shouldReturn` [0x190FF]
        assemble (BinaryInstruction "sub" (RegisterOperand Register0) (IdentifierOperand "lorem")) `shouldReturn` [0x19001]

      it "assembles a 'SUBCY sX, sY' instruction" $ do
        assemble (BinaryInstruction "subcy" (RegisterOperand Register0) (RegisterOperand Register1)) `shouldReturn` [0x1A010]

      it "assembles a 'SUBCY sX, kk' instruction" $ do
        assemble (BinaryInstruction "subcy" (RegisterOperand Register0) (ValueOperand 0xFF)) `shouldReturn` [0x1B0FF]
        assemble (BinaryInstruction "subcy" (RegisterOperand Register0) (IdentifierOperand "lorem")) `shouldReturn` [0x1B001]

    context "test and compare" $ do
      it "assembles a 'TEST sX, sY' instruction" $ do
        assemble (BinaryInstruction "test" (RegisterOperand Register0) (RegisterOperand Register1)) `shouldReturn` [0x0C010]

      it "assembles a 'TEST sX, kk' instruction" $ do
        assemble (BinaryInstruction "test" (RegisterOperand Register0) (ValueOperand 0xFF)) `shouldReturn` [0x0D0FF]
        assemble (BinaryInstruction "test" (RegisterOperand Register0) (IdentifierOperand "lorem")) `shouldReturn` [0x0D001]

      it "assembles a 'TESTCY sX, sY' instruction" $ do
        assemble (BinaryInstruction "testcy" (RegisterOperand Register0) (RegisterOperand Register1)) `shouldReturn` [0x0E010]

      it "assembles a 'TESTCY sX, kk' instruction" $ do
        assemble (BinaryInstruction "testcy" (RegisterOperand Register0) (ValueOperand 0xFF)) `shouldReturn` [0x0F0FF]
        assemble (BinaryInstruction "testcy" (RegisterOperand Register0) (IdentifierOperand "lorem")) `shouldReturn` [0x0F001]

      it "assembles a 'COMPARE sX, sY' instruction" $ do
        assemble (BinaryInstruction "compare" (RegisterOperand Register0) (RegisterOperand Register1)) `shouldReturn` [0x1C010]

      it "assembles a 'COMPARE sX, kk' instruction" $ do
        assemble (BinaryInstruction "compare" (RegisterOperand Register0) (ValueOperand 0xFF)) `shouldReturn` [0x1D0FF]
        assemble (BinaryInstruction "compare" (RegisterOperand Register0) (IdentifierOperand "lorem")) `shouldReturn` [0x1D001]

      it "assembles a 'COMPARECY sX, sY' instruction" $ do
        assemble (BinaryInstruction "comparecy" (RegisterOperand Register0) (RegisterOperand Register1)) `shouldReturn` [0x1E010]

      it "assembles a 'COMPARECY sX, kk' instruction" $ do
        assemble (BinaryInstruction "comparecy" (RegisterOperand Register0) (ValueOperand 0xFF)) `shouldReturn` [0x1F0FF]
        assemble (BinaryInstruction "comparecy" (RegisterOperand Register0) (IdentifierOperand "lorem")) `shouldReturn` [0x1F001]

    context "input and output" $ do
      it "assembles a 'INPUT sX, (sY)' instruction" $ do
        assemble (BinaryInstruction "input" (RegisterOperand Register0) (RegisterOperand Register0)) `shouldReturn` [0x08000]

      it "assembles a 'INPUT sX, pp' instruction" $ do
        assemble (BinaryInstruction "input" (RegisterOperand Register0) (ValueOperand 0xFF)) `shouldReturn` [0x090FF]
        assemble (BinaryInstruction "input" (RegisterOperand Register0) (IdentifierOperand "lorem")) `shouldReturn` [0x09001]

      it "assembles a 'OUTPUT sX, (sY)' instruction" $ do
        assemble (BinaryInstruction "output" (RegisterOperand Register0) (RegisterOperand Register0)) `shouldReturn` [0x2C000]

      it "assembles a 'OUTPUT sX, pp' instruction" $ do
        assemble (BinaryInstruction "output" (RegisterOperand Register0) (ValueOperand 0xFF)) `shouldReturn` [0x2D0FF]
        assemble (BinaryInstruction "output" (RegisterOperand Register0) (IdentifierOperand "lorem")) `shouldReturn` [0x2D001]

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
        assemble (UnaryInstruction "jump" (IdentifierOperand "foo")) `shouldReturn` [0x22001]

      it "assembles a 'JUMP Z, aaa' instruction" $ do
        assemble (BinaryInstruction "jump" (ConditionOperand ZeroCondition) (ValueOperand 0xFFF)) `shouldReturn` [0x32FFF]
        assemble (BinaryInstruction "jump" (ConditionOperand ZeroCondition) (IdentifierOperand "foo")) `shouldReturn` [0x32001]

      it "assembles a 'JUMP NZ, aaa' instruction" $ do
        assemble (BinaryInstruction "jump" (ConditionOperand NotZeroCondition) (ValueOperand 0xFFF)) `shouldReturn` [0x36FFF]
        assemble (BinaryInstruction "jump" (ConditionOperand NotZeroCondition) (IdentifierOperand "foo")) `shouldReturn` [0x36001]

      it "assembles a 'JUMP C, aaa' instruction" $ do
        assemble (BinaryInstruction "jump" (ConditionOperand CarryCondition) (ValueOperand 0xFFF)) `shouldReturn` [0x3AFFF]
        assemble (BinaryInstruction "jump" (ConditionOperand CarryCondition) (IdentifierOperand "foo")) `shouldReturn` [0x3A001]

      it "assembles a 'JUMP NC, aaa' instruction" $ do
        assemble (BinaryInstruction "jump" (ConditionOperand NotCarryCondition) (ValueOperand 0xFFF)) `shouldReturn` [0x3EFFF]
        assemble (BinaryInstruction "jump" (ConditionOperand NotCarryCondition) (IdentifierOperand "foo")) `shouldReturn` [0x3E001]

    context "subroutines" $ do
      it "assembles a 'CALL aaa' instruction" $ do
        assemble (UnaryInstruction "call" (ValueOperand 0xFFF)) `shouldReturn` [0x20FFF]
        assemble (UnaryInstruction "call" (IdentifierOperand "foo")) `shouldReturn` [0x20001]

      it "assembles a 'RETURN' instruction" $ do
        assemble (NullaryInstruction "return") `shouldReturn` [0x25000]

    it "ignores unknown statements" $ do
      assemble (NullaryInstruction "lol") `shouldReturn` []

    where assemble instruction = runAssembler ([instruction], constantMap, labelMap)
          constantMap          = fromList [("lorem", Value 1), ("ipsum", Value 2)]
          labelMap             = fromList [("foo", Value 1), ("bar", Value 2)]
