-- This module defines the assember which is used to transform an AST into
-- machine code.
module Assembler (
    runAssembler
  ) where

import Core

import Control.Monad.Reader
import Data.Bits
import Data.Map ((!))
import Data.Word

type AssemblerState a = Reader LabelMap a

fromAddress :: AddressValue -> Word32
fromAddress (AddressValue a) = fromIntegral a

fromData :: DataValue -> Word32
fromData (DataValue c) = fromIntegral c

fromRegister :: Register -> Word32
fromRegister = fromIntegral . fromEnum

-- Packs the given instruction into binary format.
pack :: Word32 -> Operand -> Operand -> AssemblerState Word32

pack op (RegisterOperand x) (RegisterOperand y) = do
  return $ (op `shiftL` 12) .|. (fromRegister x `shiftL` 8) .|. (fromRegister y `shiftL` 4)

pack op (RegisterOperand x) (DataOperand y) = do
  return $ (op `shiftL` 12) .|. (fromRegister x `shiftL` 8) .|. (fromData y)

pack op (AddressOperand x) _ = do
  return $ (op `shiftL` 12) .|. (fromAddress x)

pack op (LabelOperand x) _ = do
  labelMap <- ask
  return $ (op `shiftL` 12) .|. (fromAddress $ labelMap ! x)

-- Assembles the given statement.
assemble :: Statement -> AssemblerState Word32

assemble (BinaryInstruction "load" x y@(RegisterOperand _)) = pack 0x00 x y
assemble (BinaryInstruction "load" x y@(DataOperand _))     = pack 0x01 x y
assemble (BinaryInstruction "and"  x y@(RegisterOperand _)) = pack 0x02 x y
assemble (BinaryInstruction "and"  x y@(DataOperand _))     = pack 0x03 x y
assemble (BinaryInstruction "or"   x y@(RegisterOperand _)) = pack 0x04 x y
assemble (BinaryInstruction "or"   x y@(DataOperand _))     = pack 0x05 x y
assemble (BinaryInstruction "xor"  x y@(RegisterOperand _)) = pack 0x06 x y
assemble (BinaryInstruction "xor"  x y@(DataOperand _))     = pack 0x07 x y

assemble (UnaryInstruction "sl0"  x) = pack 0x14 x (DataOperand 0x06)
assemble (UnaryInstruction "sl1"  x) = pack 0x14 x (DataOperand 0x07)
assemble (UnaryInstruction "call" x) = pack 0x20 x (DataOperand 0x00)

assemble _ = return 0x00000

-- Assembles the given statements into binary format.
runAssembler :: [Statement] -> LabelMap -> [Word32]
runAssembler statements labelMap = runReader (mapM assemble statements) labelMap
