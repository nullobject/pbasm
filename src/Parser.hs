{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

-- This module defines the parser which is used to parse the assembly source
-- code into an AST.
module Parser where
-- module Parser (
--     address
--   , constant
--   , register
--   , instructions
--   , parsePsmFile
--   ) where

import Core

import Control.Applicative hiding (many, optional, (<|>))
import Numeric (readHex)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token (TokenParser)
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language

directiveNames          = ["constant", "string", "table", "address", "include"]
nullaryInstructionNames = ["return"]
unaryInstructionNames   = ["sl0", "sl1", "call", "jump"]
binaryInstructionNames  = ["load", "and", "or", "xor"]

readHex' = fst . head . readHex

-- Parses a 12-bit address.
address :: CharParser () Address
address = lexeme $ try (decode <$> count 3 hexDigit <?> "address")
  where decode x = Address . toEnum $ readHex' x

-- Parses a 8-bit constant value.
constant :: CharParser () Constant
constant = lexeme $ try (decode <$> count 2 hexDigit <?> "constant")
  where decode x = Constant . toEnum $ readHex' x

-- Parses a register name.
register :: CharParser () Register
register = lexeme $ try (oneOf "sS" *> (decode <$> hexDigit) <?> "register")
  where decode x = toEnum $ readHex' [x]

-- Parses an operand.
operand :: CharParser () Operand
operand = addressOperand <|> constantOperand <|> registerOperand <?> "operand"
  where
    addressOperand  = AddressOperand  <$> address
    constantOperand = ConstantOperand <$> constant
    registerOperand = RegisterOperand <$> register

nullaryInstruction :: String -> CharParser () Statement
nullaryInstruction name = NullaryInstruction name <$ reserved name

unaryInstruction :: String -> CharParser () Statement
unaryInstruction name = reserved name *> (UnaryInstruction name <$> operand)

binaryInstruction :: String -> CharParser () Statement
binaryInstruction name = reserved name *> (BinaryInstruction name <$> operand <*> (comma *> operand))

psmDef :: LanguageDef st
psmDef = emptyDef {
    Token.caseSensitive   = False
  , Token.commentStart    = ""
  , Token.commentEnd      = ""
  , Token.commentLine     = ";"
  , Token.nestedComments  = False
  , Token.reservedNames   = reservedNames
  , Token.reservedOpNames = ["~"]
  }
  where reservedNames = directiveNames ++ nullaryInstructionNames ++ unaryInstructionNames ++ binaryInstructionNames

psm :: TokenParser st
psm = Token.makeTokenParser psmDef

comma      = Token.comma psm
lexeme     = Token.lexeme psm
reserved   = Token.reserved psm
whiteSpace = Token.whiteSpace psm

statement :: CharParser () Statement
statement = choice $ nullaryInstructions ++ unaryInstructions ++ binaryInstructions
  where
    nullaryInstructions = map nullaryInstruction nullaryInstructionNames
    unaryInstructions   = map unaryInstruction unaryInstructionNames
    binaryInstructions  = map binaryInstruction binaryInstructionNames

statements :: CharParser () [Statement]
statements = whiteSpace *> many statement <* eof

parsePsmFile :: FilePath -> IO (Either ParseError [Statement])
parsePsmFile filePath = parseFromFile statements filePath
