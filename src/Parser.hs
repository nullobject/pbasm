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

data Instruction =
    -- LOAD sX, sY
    -- Loads a value from the second register into the first register.
    LoadRegisterInstruction Register Register

    -- LOAD sX, kk
    -- Loads a constant value into a register.
  | LoadConstantInstruction Register Constant

    -- AND sX, sY
    -- Performs a bit-wise logical AND operation on the first and second registers.
  | AndRegisterInstruction Register Register

    -- AND sX, kk
    -- Performs a bit-wise logical AND operation on a register and a constant.
  | AndConstantInstruction Register Constant

    -- JUMP aaa
    -- Jumps to an address.
  | JumpInstruction Address

    -- CALL aaa
    -- Calls a subroutine at an address.
  | CallInstruction Address

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

-- Parses two register names (separated by a comma) into a pair.
registerPair :: CharParser () (Register, Register)
registerPair = (,) <$> register <*> (char ',' *> spaces *> register)

-- Parses a register name and a constant value (separated by a comma) into a pair.
registerConstantPair :: CharParser () (Register, Constant)
registerConstantPair = (,) <$> register <*> (char ',' *> spaces *> constant)

loadInstruction   = string "LOAD" *> spaces *> (try (uncurry LoadRegisterInstruction <$> registerPair) <|> try (uncurry LoadConstantInstruction <$> registerConstantPair))
andInstruction    = string "AND" *> spaces *> (try (uncurry AndRegisterInstruction <$> registerPair) <|> try (uncurry AndConstantInstruction <$> registerConstantPair))
callInstruction   = string "CALL" *> spaces *> (CallInstruction <$> address)
jumpInstruction   = string "JUMP" *> spaces *> (JumpInstruction <$> address)
returnInstruction = ReturnInstruction <$ string "RETURN"

instruction :: CharParser () Instruction
instruction = loadInstruction
          <|> andInstruction
          <|> callInstruction
          <|> jumpInstruction
          <|> returnInstruction
