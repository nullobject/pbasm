{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

-- This module defines the parser which is used to parse the assembly source
-- code into an AST.
module Parser (
    address
  , constant
  , register
  , label
  , statement
  , statements
  , parsePsmFile
  , parserState
  ) where

import Core

import Control.Applicative hiding (many, optional, (<|>))
import qualified Data.Map as Map
import Numeric (readHex)
import Text.ParserCombinators.Parsec hiding (label)
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Token (TokenParser)
import qualified Text.ParserCombinators.Parsec.Token as Token

parserState = ParserState {
    parserStateAddress  = 0
  , parserStateLabelMap = Map.empty
  }

addLabel :: Label -> ParserState -> ParserState
addLabel l state = state { parserStateLabelMap = labelMap' }
  where
    a = parserStateAddress state
    labelMap  = parserStateLabelMap state
    labelMap' = Map.insert l a labelMap

incrementAddress :: ParserState -> ParserState
incrementAddress state = state { parserStateAddress = a' }
  where
    a  = parserStateAddress state
    a' = a + 1

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

colon      = Token.colon psm
comma      = Token.comma psm
lexeme     = Token.lexeme psm
label      = Token.identifier psm
reserved   = Token.reserved psm
whiteSpace = Token.whiteSpace psm

-- Parses exactly n hexadecimal digits.
hexDigits :: (Enum a) => Int -> CharParser ParserState a
hexDigits n = decode <$> count n hexDigit <* notFollowedBy (identLetter psmDef)
  where decode x = toEnum . fst . head . readHex $ x

-- Parses a 12-bit address.
address :: CharParser ParserState Address
address = (lexeme $ try $ Address <$> hexDigits 3) <?> "address"

-- Parses a 8-bit constant value.
constant :: CharParser ParserState Constant
constant = (lexeme $ try $ Constant <$> hexDigits 2) <?> "constant"

-- Parses a register name.
register :: CharParser ParserState Register
register = (lexeme $ try $ oneOf "sS" *> hexDigits 1) <?> "register"

-- Parses an operand.
operand :: CharParser ParserState Operand
operand = addressOperand <|> constantOperand <|> registerOperand <|> labelOperand <?> "operand"
  where
    addressOperand  = AddressOperand  <$> address
    constantOperand = ConstantOperand <$> constant
    labelOperand    = LabelOperand    <$> label
    registerOperand = RegisterOperand <$> register

-- Parses an instruction of arity 0.
nullaryInstruction :: String -> CharParser ParserState Statement
nullaryInstruction name = NullaryInstruction name <$ reserved name

-- Parses an instruction of arity 1.
unaryInstruction :: String -> CharParser ParserState Statement
unaryInstruction name = reserved name *> (UnaryInstruction name <$> operand)

-- Parses an instruction of arity 2.
binaryInstruction :: String -> CharParser ParserState Statement
binaryInstruction name = reserved name *> (BinaryInstruction name <$> operand <*> (comma *> operand))

-- Parses a statement and increments the program address.
statement :: CharParser ParserState Statement
statement = do
  l <- optionMaybe (label <* colon)
  updateState $ maybe id addLabel l
  i <- instruction
  updateState incrementAddress
  return i
  where
    nullaryInstructions = map nullaryInstruction nullaryInstructionNames
    unaryInstructions   = map unaryInstruction unaryInstructionNames
    binaryInstructions  = map binaryInstruction binaryInstructionNames
    instruction         = choice $ nullaryInstructions ++ unaryInstructions ++ binaryInstructions

-- Parses multiple statements and returns a label map.
statements :: CharParser ParserState ([Statement], LabelMap)
statements = whiteSpace *> ((,) <$> many statement <*> labelMap) <* eof
  where labelMap = parserStateLabelMap <$> getState

-- Parses a file, returning the statements and a label map.
parsePsmFile :: FilePath -> IO (Either ParseError ([Statement], LabelMap))
parsePsmFile filePath = runParser statements parserState filePath <$> readFile filePath
