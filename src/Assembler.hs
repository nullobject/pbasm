-- This module defines the assember which is used to transform an AST into
-- machine code.
module Assembler
  ( State (..)
  , assemblerState
  , runAssembler
  ) where

import Core

import Control.Monad.Reader
import Data.Bits
import Data.Map ((!), empty)
import Data.Maybe
import Data.Word

data State = State
  { stateLabelMap    :: LabelMap
  , stateConstantMap :: ConstantMap
  } deriving (Eq, Show)

type AssemblerReader a = Reader State a

assemblerState :: State
assemblerState = State
  { stateLabelMap    = empty
  , stateConstantMap = empty
  }

fromValue :: Value -> Word32
fromValue (Value c) = fromIntegral c

fromRegister :: Register -> Word32
fromRegister = fromIntegral . fromEnum

-- Packs the given instruction into binary format.
pack :: Word32 -> Operand -> Operand -> AssemblerReader (Maybe Word32)

pack op (RegisterOperand x) (RegisterOperand y) =
  return $ Just $ (op `shiftL` 12) .|. (fromRegister x `shiftL` 8) .|. (fromRegister y `shiftL` 4)

pack op (RegisterOperand x) (ValueOperand y) =
  return $ Just $ (op `shiftL` 12) .|. (fromRegister x `shiftL` 8) .|. (fromValue y)

pack op (RegisterOperand x) (IdentifierOperand y) = do
  constantMap <- asks stateConstantMap
  return $ Just $ (op `shiftL` 12) .|. (fromRegister x `shiftL` 8) .|. (fromValue $ constantMap ! y)

pack op (ValueOperand x) _ =
  return $ Just $ (op `shiftL` 12) .|. (fromValue x)

pack op (IdentifierOperand x) _ = do
  labelMap <- asks stateLabelMap
  return $ Just $ (op `shiftL` 12) .|. (fromValue $ labelMap ! x)

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
assemble (BinaryInstruction "test"      x y@(RegisterOperand _)) = pack 0x0C x y
assemble (BinaryInstruction "test"      x y)                     = pack 0x0D x y
assemble (BinaryInstruction "testcy"    x y@(RegisterOperand _)) = pack 0x0E x y
assemble (BinaryInstruction "testcy"    x y)                     = pack 0x0F x y
assemble (BinaryInstruction "compare"   x y@(RegisterOperand _)) = pack 0x1C x y
assemble (BinaryInstruction "compare"   x y)                     = pack 0x1D x y
assemble (BinaryInstruction "comparecy" x y@(RegisterOperand _)) = pack 0x1E x y
assemble (BinaryInstruction "comparecy" x y)                     = pack 0x1F x y

-- Input and output
assemble (BinaryInstruction "input"  x y@(RegisterOperand _)) = pack 0x08 x y
assemble (BinaryInstruction "input"  x y)                     = pack 0x09 x y
assemble (BinaryInstruction "output" x y@(RegisterOperand _)) = pack 0x2C x y
assemble (BinaryInstruction "output" x y)                     = pack 0x2D x y

-- Shift and rotate
assemble (UnaryInstruction "sl0" x) = pack 0x14 x (ValueOperand 0x06)
assemble (UnaryInstruction "sl1" x) = pack 0x14 x (ValueOperand 0x07)
assemble (UnaryInstruction "slx" x) = pack 0x14 x (ValueOperand 0x04)
assemble (UnaryInstruction "sla" x) = pack 0x14 x (ValueOperand 0x00)
assemble (UnaryInstruction "rl"  x) = pack 0x14 x (ValueOperand 0x02)
assemble (UnaryInstruction "sr0" x) = pack 0x14 x (ValueOperand 0x0E)
assemble (UnaryInstruction "sr1" x) = pack 0x14 x (ValueOperand 0x0F)
assemble (UnaryInstruction "srx" x) = pack 0x14 x (ValueOperand 0x0A)
assemble (UnaryInstruction "sra" x) = pack 0x14 x (ValueOperand 0x08)
assemble (UnaryInstruction "rr"  x) = pack 0x14 x (ValueOperand 0x0C)

-- Jump
-- TODO

-- Subroutines
assemble (UnaryInstruction "call" x)   = pack 0x20 x (ValueOperand 0x00)
assemble (NullaryInstruction "return") = pack 0x25 (ValueOperand 0x00) (ValueOperand 0x00)

-- Default
assemble _ = return Nothing

-- Assembles the given statements into binary format.
runAssembler :: [Statement] -> ConstantMap -> LabelMap -> [Word32]
runAssembler statements constantMap labelMap = catMaybes $ runReader (mapM assemble statements) state
  where state = assemblerState { stateLabelMap    = labelMap
                               , stateConstantMap = constantMap}
