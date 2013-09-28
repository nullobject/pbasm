-- This module defines the assember which is used to transform an AST into
-- machine code.
module Assembler (
    assemble
  , runAssembler
  ) where

import Core

import Data.Bits
import Data.Word

fromAddress :: Address -> Word32
fromAddress (Address a) = fromIntegral a

fromConstant :: Constant -> Word32
fromConstant (Constant c) = fromIntegral c

fromRegister :: Register -> Word32
fromRegister = fromIntegral . fromEnum

foo op x y = (op `shiftL` 12) + (fromRegister x `shiftL` 8) + (fromRegister y `shiftL` 4)
bar op x y = (op `shiftL` 12) + (fromRegister x `shiftL` 8) + (fromConstant y)
baz op x   = (op `shiftL` 12) + (fromAddress x)

assemble :: Instruction -> Word32
assemble (LoadInstruction (RegisterOperand x) (RegisterOperand y)) = foo 0x00 x y
assemble (LoadInstruction (RegisterOperand x) (ConstantOperand y)) = bar 0x01 x y
assemble (AndInstruction (RegisterOperand x) (RegisterOperand y))  = foo 0x02 x y
assemble (AndInstruction (RegisterOperand x) (ConstantOperand y))  = bar 0x03 x y
assemble (OrInstruction (RegisterOperand x) (RegisterOperand y))   = foo 0x04 x y
assemble (OrInstruction (RegisterOperand x) (ConstantOperand y))   = bar 0x05 x y
assemble (XorInstruction (RegisterOperand x) (RegisterOperand y))  = foo 0x06 x y
assemble (XorInstruction (RegisterOperand x) (ConstantOperand y))  = bar 0x07 x y
assemble (ShiftLeft0Instruction (RegisterOperand x))               = (0x14 `shiftL` 12) + (fromRegister x `shiftL` 8) + 0x06
assemble (CallInstruction (AddressOperand x))                      = baz 0x20 x
assemble _                                                         = 0x00000

runAssembler :: [Instruction] -> [Word32]
runAssembler = map assemble
