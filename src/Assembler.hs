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

pack :: Word32 -> Operand -> Word32 -> Word32
pack op (RegisterOperand x) y = (op `shiftL` 12) + (fromRegister x `shiftL` 8) + y
pack op (AddressOperand  x) y = (op `shiftL` 12) + (fromAddress x)

assemble :: Statement -> Word32
assemble (BinaryInstruction "load" x (RegisterOperand y)) = pack 0x00 x (fromRegister y `shiftL` 4)
assemble (BinaryInstruction "load" x (ConstantOperand y)) = pack 0x01 x (fromConstant y)
assemble (BinaryInstruction "and"  x (RegisterOperand y)) = pack 0x02 x (fromRegister y `shiftL` 4)
assemble (BinaryInstruction "and"  x (ConstantOperand y)) = pack 0x03 x (fromConstant y)
assemble (BinaryInstruction "or"   x (RegisterOperand y)) = pack 0x04 x (fromRegister y `shiftL` 4)
assemble (BinaryInstruction "or"   x (ConstantOperand y)) = pack 0x05 x (fromConstant y)
assemble (BinaryInstruction "xor"  x (RegisterOperand y)) = pack 0x06 x (fromRegister y `shiftL` 4)
assemble (BinaryInstruction "xor"  x (ConstantOperand y)) = pack 0x07 x (fromConstant y)

assemble (UnaryInstruction "sl0"  x) = pack 0x14 x 0x06
assemble (UnaryInstruction "sl1"  x) = pack 0x14 x 0x07
assemble (UnaryInstruction "call" x) = pack 0x20 x 0x00

assemble _ = 0x00000

runAssembler :: [Statement] -> [Word32]
runAssembler = map assemble
