-- This module defines the assember which is used to transform an AST into
-- machine code.
module Language.Pbasm.Assembler
  ( State (..),
    assemblerState,
    runAssembler,
  )
where

import Control.Monad.Reader (Reader, asks, runReader)
import Data.Bits (Bits (complement, shiftL, shiftR, (.&.), (.|.)))
import Data.Map (empty, (!))
import Data.Maybe (catMaybes)
import Data.Word (Word32)
import Language.Pbasm.Core
  ( Condition (..),
    ConstantMap,
    Identifier,
    LabelMap,
    Modifier (..),
    Opcode,
    Operand (..),
    ParserResult,
    Register,
    Statement (..),
    Value (..),
  )

data State = State
  { stateLabelMap :: LabelMap,
    stateConstantMap :: ConstantMap
  }
  deriving (Eq, Show)

type AssemblerReader a = Reader State a

assemblerState :: State
assemblerState =
  State
    { stateLabelMap = empty,
      stateConstantMap = empty
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

-- | Packs a unary instruction into binary format.
pack1 :: Word32 -> Operand -> AssemblerReader (Maybe Opcode)
pack1 op (ValueOperand x) =
  return $ Just $ (op `shiftL` 12) .|. fromValue x
pack1 op (IdentifierOperand x _) = do
  value <- lookupLabel x
  return $ Just $ (op `shiftL` 12) .|. value
pack1 _ _ = return Nothing

-- | Packs a binary instruction into binary format.
pack2 :: Word32 -> Operand -> Operand -> AssemblerReader (Maybe Opcode)
pack2 op (RegisterOperand x) (RegisterOperand y) =
  return $ Just $ (op `shiftL` 12) .|. (fromRegister x `shiftL` 8) .|. (fromRegister y `shiftL` 4)
pack2 op (RegisterOperand x) (ValueOperand y) =
  return $ Just $ (op `shiftL` 12) .|. (fromRegister x `shiftL` 8) .|. fromValue y
pack2 op (RegisterOperand x) (IdentifierOperand y modifier)
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
pack2 op (ValueOperand x) (ValueOperand y) =
  return $ Just $ (op `shiftL` 12) .|. (fromValue x `shiftL` 4) .|. (fromValue y .&. 0xF)
pack2 _ _ _ = return Nothing

-- | Assembles the given statement into an opcode.
assemble :: Statement -> AssemblerReader (Maybe Opcode)
-- Register loading
assemble (BinaryInstruction "load" x y@(RegisterOperand _)) = pack2 0x00 x y
assemble (BinaryInstruction "load" x y) = pack2 0x01 x y
assemble (BinaryInstruction "star" (RegisterOperand x) (RegisterOperand y)) =
  return $ Just $ (0x16 `shiftL` 12) .|. ((fromRegister x .&. 0xF) `shiftL` 8) .|. ((fromRegister y .&. 0xF) `shiftL` 4)
-- Logical
assemble (BinaryInstruction "and" x y@(RegisterOperand _)) = pack2 0x02 x y
assemble (BinaryInstruction "and" x y) = pack2 0x03 x y
assemble (BinaryInstruction "or" x y@(RegisterOperand _)) = pack2 0x04 x y
assemble (BinaryInstruction "or" x y) = pack2 0x05 x y
assemble (BinaryInstruction "xor" x y@(RegisterOperand _)) = pack2 0x06 x y
assemble (BinaryInstruction "xor" x y) = pack2 0x07 x y
-- Arithmetic
assemble (BinaryInstruction "add" x y@(RegisterOperand _)) = pack2 0x10 x y
assemble (BinaryInstruction "add" x y) = pack2 0x11 x y
assemble (BinaryInstruction "addcy" x y@(RegisterOperand _)) = pack2 0x12 x y
assemble (BinaryInstruction "addcy" x y) = pack2 0x13 x y
assemble (BinaryInstruction "sub" x y@(RegisterOperand _)) = pack2 0x18 x y
assemble (BinaryInstruction "sub" x y) = pack2 0x19 x y
assemble (BinaryInstruction "subcy" x y@(RegisterOperand _)) = pack2 0x1A x y
assemble (BinaryInstruction "subcy" x y) = pack2 0x1B x y
-- Test and compare
assemble (BinaryInstruction "test" x y@(RegisterOperand _)) = pack2 0x0C x y
assemble (BinaryInstruction "test" x y) = pack2 0x0D x y
assemble (BinaryInstruction "testcy" x y@(RegisterOperand _)) = pack2 0x0E x y
assemble (BinaryInstruction "testcy" x y) = pack2 0x0F x y
assemble (BinaryInstruction "compare" x y@(RegisterOperand _)) = pack2 0x1C x y
assemble (BinaryInstruction "compare" x y) = pack2 0x1D x y
assemble (BinaryInstruction "comparecy" x y@(RegisterOperand _)) = pack2 0x1E x y
assemble (BinaryInstruction "comparecy" x y) = pack2 0x1F x y
-- Register bank selection
assemble (UnaryInstruction "regbank" (ValueOperand x)) =
  return $ Just $ (0x37 `shiftL` 12) .|. ((fromValue x - 0xA) .&. 0x1)
-- Input and output
assemble (BinaryInstruction "input" x y@(RegisterOperand _)) = pack2 0x08 x y
assemble (BinaryInstruction "input" x y) = pack2 0x09 x y
assemble (BinaryInstruction "output" x y@(RegisterOperand _)) = pack2 0x2C x y
assemble (BinaryInstruction "output" x y) = pack2 0x2D x y
assemble (BinaryInstruction "outputk" x y) = pack2 0x2B x y
-- Scratch pad memory
assemble (BinaryInstruction "store" x y@(RegisterOperand _)) = pack2 0x2E x y
assemble (BinaryInstruction "store" x y) = pack2 0x2F x y
assemble (BinaryInstruction "fetch" x y@(RegisterOperand _)) = pack2 0x0A x y
assemble (BinaryInstruction "fetch" x y) = pack2 0x0B x y
-- Interrupt handling
assemble (NullaryInstruction "disable interrupt") = return $ Just 0x28000
assemble (NullaryInstruction "enable interrupt") = return $ Just 0x28001
assemble (NullaryInstruction "returni disable") = return $ Just 0x29000
assemble (NullaryInstruction "returni enable") = return $ Just 0x29001
-- Shift and rotate
assemble (UnaryInstruction "sl0" x) = pack2 0x14 x (ValueOperand 0x06)
assemble (UnaryInstruction "sl1" x) = pack2 0x14 x (ValueOperand 0x07)
assemble (UnaryInstruction "slx" x) = pack2 0x14 x (ValueOperand 0x04)
assemble (UnaryInstruction "sla" x) = pack2 0x14 x (ValueOperand 0x00)
assemble (UnaryInstruction "rl" x) = pack2 0x14 x (ValueOperand 0x02)
assemble (UnaryInstruction "sr0" x) = pack2 0x14 x (ValueOperand 0x0E)
assemble (UnaryInstruction "sr1" x) = pack2 0x14 x (ValueOperand 0x0F)
assemble (UnaryInstruction "srx" x) = pack2 0x14 x (ValueOperand 0x0A)
assemble (UnaryInstruction "sra" x) = pack2 0x14 x (ValueOperand 0x08)
assemble (UnaryInstruction "rr" x) = pack2 0x14 x (ValueOperand 0x0C)
-- Jump
assemble (UnaryInstruction "jump" x) = pack1 0x22 x
assemble (BinaryInstruction "jump" (ConditionOperand x) y)
  | x == ZeroCondition = pack1 0x32 y
  | x == NotZeroCondition = pack1 0x36 y
  | x == CarryCondition = pack1 0x3A y
  | x == NotCarryCondition = pack1 0x3E y
assemble (BinaryInstruction "jump@" (RegisterOperand x) (RegisterOperand y)) =
  return $ Just $ (0x26 `shiftL` 12) .|. ((fromRegister x .&. 0xF) `shiftL` 8) .|. ((fromRegister y .&. 0xF) `shiftL` 4)
-- Subroutines
assemble (UnaryInstruction "call" x) = pack1 0x20 x
assemble (BinaryInstruction "call" (ConditionOperand x) y)
  | x == ZeroCondition = pack1 0x30 y
  | x == NotZeroCondition = pack1 0x34 y
  | x == CarryCondition = pack1 0x38 y
  | x == NotCarryCondition = pack1 0x3C y
assemble (BinaryInstruction "call@" (RegisterOperand x) (RegisterOperand y)) =
  return $ Just $ (0x24 `shiftL` 12) .|. ((fromRegister x .&. 0xF) `shiftL` 8) .|. ((fromRegister y .&. 0xF) `shiftL` 4)
assemble (NullaryInstruction "return") = return $ Just 0x25000
assemble (UnaryInstruction "return" (ConditionOperand x))
  | x == ZeroCondition = return $ Just 0x31000
  | x == NotZeroCondition = return $ Just 0x35000
  | x == CarryCondition = return $ Just 0x39000
  | x == NotCarryCondition = return $ Just 0x3D000
assemble (BinaryInstruction "load&return" x y) = pack2 0x21 x y
-- Default
assemble _ = return Nothing

-- | Assembles the given statements into opcodes.
runAssembler :: ParserResult -> IO [Opcode]
runAssembler (statements, constantMap, labelMap) = return opcodes
  where
    opcodes = catMaybes $ runReader (mapM assemble statements) state
    state = assemblerState {stateLabelMap = labelMap, stateConstantMap = constantMap}
