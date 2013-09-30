{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

-- This module defines the parser which is used to parse the assembly source
-- code into an AST.
module Parser (
    address
  , constant
  , register
  , instructions
  , parsePsmFile
  ) where

import Core

import Control.Applicative hiding (optional, (<|>))
import Numeric (readHex)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language

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

loadInstruction       = string "LOAD" *> spaces *> (operands2 LoadInstruction)
andInstruction        = string "AND"  *> spaces *> (operands2 AndInstruction)
orInstruction         = string "OR"   *> spaces *> (operands2 OrInstruction)
xorInstruction        = string "XOR"  *> spaces *> (operands2 XorInstruction)
shiftLeft0Instruction = try (string "SL0" *> spaces *> (operands1 ShiftLeft0Instruction))
shiftLeft1Instruction = try (string "SL1" *> spaces *> (operands1 ShiftLeft1Instruction))
callInstruction       = string "CALL" *> spaces *> (operands1 CallInstruction)
jumpInstruction       = string "JUMP" *> spaces *> (operands1 JumpInstruction)
returnInstruction     = ReturnInstruction <$ string "RETURN"

instruction :: CharParser () Instruction
instruction = choice [
    loadInstruction
  , andInstruction
  , orInstruction
  , xorInstruction
  , shiftLeft0Instruction
  , shiftLeft1Instruction
  , callInstruction
  , jumpInstruction
  , returnInstruction
  ]

instructions :: CharParser () [Instruction]
instructions = sepEndBy instruction newline

psmDef :: LanguageDef s
psmDef = emptyDef {
    caseSensitive   = False
  , commentStart    = ""
  , commentEnd      = ""
  , commentLine     = ";"
  , nestedComments  = False
  , reservedNames   = directiveNames ++ instructionNames
  , reservedOpNames = ["~"]
  }
  where
    directiveNames   = ["constant", "string", "table", "address", "include"]
    instructionNames = ["load", "and", "or", "xor", "sl0", "sl1", "call", "jump", "return"]

psm :: TokenParser s
psm = makeTokenParser psmDef

parsePsmFile :: FilePath -> IO (Either ParseError [Instruction])
parsePsmFile filePath = do
  parseFromFile instructions filePath
