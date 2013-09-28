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

addressOperand = AddressOperand <$> address
constantOperand = ConstantOperand <$> constant
registerOperand = RegisterOperand <$> register

loadInstruction   = string "LOAD" *> spaces *> (LoadInstruction <$> (registerOperand <* char ',' <* spaces) <*> (try registerOperand <|> try constantOperand))
andInstruction    = string "AND" *> spaces *> (AndInstruction <$> (registerOperand <* char ',' <* spaces) <*> (try registerOperand <|> try constantOperand))
callInstruction   = string "CALL" *> spaces *> (CallInstruction <$> addressOperand)
jumpInstruction   = string "JUMP" *> spaces *> (JumpInstruction <$> addressOperand)
returnInstruction = ReturnInstruction <$ string "RETURN"

instruction :: CharParser () Instruction
instruction = loadInstruction
          <|> andInstruction
          <|> callInstruction
          <|> jumpInstruction
          <|> returnInstruction
