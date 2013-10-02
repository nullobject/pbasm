{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

-- This module defines the parser which is used to parse the assembly source
-- code into an AST.
module Parser (
    address
  , constant
  , register
  , parsePsmFile
  ) where

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

-- Parses exactly n hexadecimal digits.
hexDigits :: (Enum a) => Int -> CharParser () a
hexDigits n = decode <$> count n hexDigit <* notFollowedBy hexDigit
  where decode x = toEnum . fst . head . readHex $ x

-- Parses a 12-bit address.
address :: CharParser () Address
address = (lexeme $ try $ Address <$> hexDigits 3) <?> "address"

-- Parses a 8-bit constant value.
constant :: CharParser () Constant
constant = (lexeme $ try $ Constant <$> hexDigits 2) <?> "constant"

-- Parses a register name.
register :: CharParser () Register
register = (lexeme $ try $ oneOf "sS" *> hexDigits 1) <?> "register"

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
