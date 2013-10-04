-- This module defines the assember which is used to transform an AST into
-- machine code.
module Assembler (
    assemble
  , runAssembler
  ) where

import Core

import Data.Bits
import Data.Map ((!))
import Data.Word

fromAddress :: Address -> Word32
fromAddress (Address a) = fromIntegral a

fromConstant :: Constant -> Word32
fromConstant (Constant c) = fromIntegral c

fromRegister :: Register -> Word32
fromRegister = fromIntegral . fromEnum

pack :: Word32 -> Operand -> Word32 -> LabelMap -> Word32
pack op (RegisterOperand x) y _ = (op `shiftL` 12) + (fromRegister x `shiftL` 8) + y
pack op (AddressOperand x) y _ = (op `shiftL` 12) + (fromAddress x)
pack op (LabelOperand x) y labelMap = (op `shiftL` 12) + (fromAddress $ labelMap ! x)

assemble :: Statement -> LabelMap -> Word32
assemble (BinaryInstruction "load" x (RegisterOperand y)) labelMap = pack 0x00 x (fromRegister y `shiftL` 4) labelMap
assemble (BinaryInstruction "load" x (ConstantOperand y)) labelMap = pack 0x01 x (fromConstant y) labelMap
assemble (BinaryInstruction "and"  x (RegisterOperand y)) labelMap = pack 0x02 x (fromRegister y `shiftL` 4) labelMap
assemble (BinaryInstruction "and"  x (ConstantOperand y)) labelMap = pack 0x03 x (fromConstant y) labelMap
assemble (BinaryInstruction "or"   x (RegisterOperand y)) labelMap = pack 0x04 x (fromRegister y `shiftL` 4) labelMap
assemble (BinaryInstruction "or"   x (ConstantOperand y)) labelMap = pack 0x05 x (fromConstant y) labelMap
assemble (BinaryInstruction "xor"  x (RegisterOperand y)) labelMap = pack 0x06 x (fromRegister y `shiftL` 4) labelMap
assemble (BinaryInstruction "xor"  x (ConstantOperand y)) labelMap = pack 0x07 x (fromConstant y) labelMap

assemble (UnaryInstruction "sl0"  x) labelMap = pack 0x14 x 0x06 labelMap
assemble (UnaryInstruction "sl1"  x) labelMap = pack 0x14 x 0x07 labelMap
assemble (UnaryInstruction "call" x) labelMap = pack 0x20 x 0x00 labelMap

assemble _ _ = 0x00000

runAssembler :: [Statement] -> LabelMap -> [Word32]
runAssembler statements labelMap = map (\statement -> assemble statement labelMap) statements
