-- This module defines the assember which is used to transform an AST into
-- machine code.
module Assembler (
    runAssembler
  ) where

import Core

import Control.Monad.Reader
import Data.Bits
import Data.Map ((!))
import Data.Maybe
import Data.Word

data AssemblerState = AssemblerState {
    assemblerStateLabelMap    :: LabelMap
  , assemblerStateConstantMap :: ConstantMap
  } deriving (Show)

type AssemblerReader a = Reader AssemblerState a

fromAddress :: AddressValue -> Word32
fromAddress (AddressValue a) = fromIntegral a

fromData :: DataValue -> Word32
fromData (DataValue c) = fromIntegral c

fromRegister :: Register -> Word32
fromRegister = fromIntegral . fromEnum

-- Packs the given instruction into binary format.
pack :: Word32 -> Operand -> Operand -> AssemblerReader (Maybe Word32)

pack op (RegisterOperand x) (RegisterOperand y) =
  return $ Just $ (op `shiftL` 12) .|. (fromRegister x `shiftL` 8) .|. (fromRegister y `shiftL` 4)

pack op (RegisterOperand x) (DataOperand y) =
  return $ Just $ (op `shiftL` 12) .|. (fromRegister x `shiftL` 8) .|. (fromData y)

pack op (RegisterOperand x) (IdentifierOperand y) = do
  constantMap <- asks assemblerStateConstantMap
  return $ Just $ (op `shiftL` 12) .|. (fromRegister x `shiftL` 8) .|. (fromData $ constantMap ! y)

pack op (AddressOperand x) _ =
  return $ Just $ (op `shiftL` 12) .|. (fromAddress x)

pack op (IdentifierOperand x) _ = do
  labelMap <- asks assemblerStateLabelMap
  return $ Just $ (op `shiftL` 12) .|. (fromAddress $ labelMap ! x)

pack op _ _ = return $ Just $ op `shiftL` 12

-- Assembles the given statement.
assemble :: Statement -> AssemblerReader (Maybe Word32)

-- Register loading
assemble (BinaryInstruction "load" x y@(RegisterOperand _)) = pack 0x00 x y
assemble (BinaryInstruction "load" x y)                     = pack 0x01 x y

-- Logical
assemble (BinaryInstruction "and" x y@(RegisterOperand _)) = pack 0x02 x y
assemble (BinaryInstruction "and" x y)                     = pack 0x03 x y
assemble (BinaryInstruction "or"  x y@(RegisterOperand _)) = pack 0x04 x y
assemble (BinaryInstruction "or"  x y)                     = pack 0x05 x y
assemble (BinaryInstruction "xor" x y@(RegisterOperand _)) = pack 0x06 x y
assemble (BinaryInstruction "xor" x y)                     = pack 0x07 x y

-- Arithmetic
assemble (BinaryInstruction "add"   x y@(RegisterOperand _)) = pack 0x10 x y
assemble (BinaryInstruction "add"   x y)                     = pack 0x11 x y
assemble (BinaryInstruction "addcy" x y@(RegisterOperand _)) = pack 0x12 x y
assemble (BinaryInstruction "addcy" x y)                     = pack 0x13 x y
assemble (BinaryInstruction "sub"   x y@(RegisterOperand _)) = pack 0x18 x y
assemble (BinaryInstruction "sub"   x y)                     = pack 0x19 x y
assemble (BinaryInstruction "subcy" x y@(RegisterOperand _)) = pack 0x1A x y
assemble (BinaryInstruction "subcy" x y)                     = pack 0x1B x y

-- Test and compare
-- TODO

-- IO
-- TODO

-- Shift and rotate
assemble (UnaryInstruction "sl0" x) = pack 0x14 x (DataOperand 0x06)
assemble (UnaryInstruction "sl1" x) = pack 0x14 x (DataOperand 0x07)

-- Jump
-- TODO

-- Subroutines
assemble (UnaryInstruction "call" x)   = pack 0x20 x (DataOperand 0x00)
assemble (NullaryInstruction "return") = pack 0x25 (DataOperand 0x00) (DataOperand 0x00)

-- Default
assemble _ = return Nothing

-- Assembles the given statements into binary format.
runAssembler :: [Statement] -> ConstantMap -> LabelMap -> [Word32]
runAssembler statements constantMap labelMap = catMaybes $ runReader (mapM assemble statements) assemblerState
  where assemblerState = AssemblerState { assemblerStateConstantMap = constantMap
                                        , assemblerStateLabelMap    = labelMap }
