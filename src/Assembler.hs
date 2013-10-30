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

lookupLabel :: Identifier -> AssemblerReader Word32
lookupLabel identifier = do
  labelMap <- asks stateLabelMap
  return $ fromValue $ labelMap ! identifier

lookupConstant :: Identifier -> AssemblerReader Word32
lookupConstant identifier = do
  constantMap <- asks stateConstantMap
  return $ fromValue $ constantMap ! identifier

-- Packs the given instruction into binary format.
pack :: Word32 -> Operand -> Operand -> AssemblerReader (Maybe Opcode)

pack op (RegisterOperand x) (RegisterOperand y) =
  return $ Just $ (op `shiftL` 12) .|. (fromRegister x `shiftL` 8) .|. (fromRegister y `shiftL` 4)

pack op (RegisterOperand x) (ValueOperand y) =
  return $ Just $ (op `shiftL` 12) .|. (fromRegister x `shiftL` 8) .|. (fromValue y)

pack op (RegisterOperand x) (IdentifierOperand y modifier)
  | modifier == Just InvertModifier = do
    value <- lookupConstant y
    return $ Just $ (op `shiftL` 12) .|. (fromRegister x `shiftL` 8) .|. (complement value .&. 0xFF)

  | modifier == Just LowerModifier = do
    value <- lookupLabel y
    return $ Just $ (op `shiftL` 12) .|. (fromRegister x `shiftL` 8) .|. (value .&. 0xFF)

  | modifier == Just UpperModifier = do
    value <- lookupLabel y
    return $ Just $ (op `shiftL` 12) .|. (fromRegister x `shiftL` 8) .|. ((value `shiftR` 8) .&. 0xF)

  | otherwise = do
    value <- lookupConstant y
    return $ Just $ (op `shiftL` 12) .|. (fromRegister x `shiftL` 8) .|. value

pack op (ValueOperand x) _ =
  return $ Just $ (op `shiftL` 12) .|. (fromValue x)

pack op (IdentifierOperand x _) _ = do
  value <- lookupLabel x
  return $ Just $ (op `shiftL` 12) .|. value

pack _ _ _ = return Nothing

-- Assembles the given statement into an opcode.
assemble :: Statement -> AssemblerReader (Maybe Opcode)

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
assemble (UnaryInstruction "jump" x) = pack 0x22 x (ValueOperand 0x00)
assemble (BinaryInstruction "jump" (ConditionOperand x) y)
  | x == ZeroCondition     = pack 0x32 y (ValueOperand 0x00)
  | x == NotZeroCondition  = pack 0x36 y (ValueOperand 0x00)
  | x == CarryCondition    = pack 0x3A y (ValueOperand 0x00)
  | x == NotCarryCondition = pack 0x3E y (ValueOperand 0x00)

-- Subroutines
assemble (UnaryInstruction "call" x)   = pack 0x20 x (ValueOperand 0x00)
assemble (NullaryInstruction "return") = pack 0x25 (ValueOperand 0x00) (ValueOperand 0x00)

-- Default
assemble _ = return Nothing

-- Assembles the given statements into opcodes.
runAssembler :: ParserResult -> IO [Opcode]
runAssembler (statements, constantMap, labelMap) = return opcodes
  where opcodes = catMaybes $ runReader (mapM assemble statements) state
        state   = assemblerState {stateLabelMap = labelMap, stateConstantMap = constantMap}
