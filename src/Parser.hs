{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Parser where

import Control.Applicative hiding (optional, (<|>))
import Data.Word
import Numeric (readHex)
import Text.ParserCombinators.Parsec

-- A 12-bit program address.
newtype Address = Address Word16 deriving (Eq, Show)

-- An 8-bit constant value.
newtype Constant = Constant Word8 deriving (Eq, Show)

-- The PicoBlaze has 16 general purpose 8-bit registers.
data Register =
    Register0 | Register1 | Register2 | Register3
  | Register4 | Register5 | Register6 | Register7
  | Register8 | Register9 | RegisterA | RegisterB
  | RegisterC | RegisterD | RegisterE | RegisterF
  deriving (Enum, Eq, Show)

data Operand =
    AddressOperand  Address
  | ConstantOperand Constant
  | RegisterOperand Register
  deriving (Eq, Show)

data Instruction =
    -- LOAD sX, sY
    -- LOAD sX, kk
    -- Loads a value into the first operand from the second operand.
    LoadInstruction Operand Operand

    -- AND sX, sY
    -- AND sX, kk
    -- Performs a bit-wise logical AND operation on the first and second operands.
  | AndInstruction Operand Operand

    -- OR sX, sY
    -- OR sX, kk
    -- Performs a bit-wise logical OR operation on the first and second operands.
  | OrInstruction Operand Operand

    -- XOR sX, sY
    -- XOR sX, kk
    -- Performs a bit-wise logical XOR operation on the first and second operands.
  | XorInstruction Operand Operand

    -- JUMP aaa
    -- Jumps to an address.
  | JumpInstruction Operand

    -- CALL aaa
    -- Calls a subroutine at an address.
  | CallInstruction Operand

    -- RETURN
    -- Completes a subroutine.
  | ReturnInstruction
  deriving (Eq, Show)

readHex' = fst . head . readHex

-- Parses a 12-bit address.
address :: CharParser () Address
address = decode <$> count 3 hexDigit
  where decode x = Address . toEnum $ readHex' x

-- Parses a 8-bit constant value.
constant :: CharParser () Constant
constant = decode <$> count 2 hexDigit
  where decode x = Constant . toEnum $ readHex' x

-- Parses a register name.
register :: CharParser () Register
register = char 's' *> (decode <$> hexDigit)
  where decode x = toEnum $ readHex' [x]

addressOperand :: CharParser () Operand
addressOperand = AddressOperand <$> address

constantOperand :: CharParser () Operand
constantOperand = ConstantOperand <$> constant

registerOperand :: CharParser () Operand
registerOperand = RegisterOperand <$> register

operand = choice [
    try addressOperand
  , try constantOperand
  , try registerOperand
  ]

operands1 p = p <$> operand
operands2 p = p <$> (operand <* spaces <* char ',' <* spaces) <*> operand

loadInstruction   = string "LOAD" *> spaces *> (operands2 LoadInstruction)
andInstruction    = string "AND"  *> spaces *> (operands2 AndInstruction)
orInstruction     = string "OR"   *> spaces *> (operands2 OrInstruction)
xorInstruction    = string "XOR"  *> spaces *> (operands2 XorInstruction)
callInstruction   = string "CALL" *> spaces *> (operands1 CallInstruction)
jumpInstruction   = string "JUMP" *> spaces *> (operands1 JumpInstruction)
returnInstruction = ReturnInstruction <$ string "RETURN"

instruction :: CharParser () Instruction
instruction = choice [
    loadInstruction
  , andInstruction
  , orInstruction
  , xorInstruction
  , callInstruction
  , jumpInstruction
  , returnInstruction
  ]

instructions :: CharParser () [Instruction]
instructions = sepEndBy instruction newline
