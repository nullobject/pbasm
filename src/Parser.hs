{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

-- This module defines the parser which is used to parse the assembly source
-- code into an AST.
module Parser (
    addressValue
  , dataValue
  , register
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

data ParserState = ParserState {
    parserStateAddress     :: AddressValue
  , parserStateLabelMap    :: LabelMap
  , parserStateConstantMap :: ConstantMap
  } deriving (Show)

type ParserResult = ([Statement], ConstantMap, LabelMap)

parserState = ParserState {
    parserStateAddress     = 0
  , parserStateConstantMap = Map.empty
  , parserStateLabelMap    = Map.empty
  }

addConstant :: Statement -> ParserState -> ParserState
addConstant (ConstantDirective c d) state = state { parserStateConstantMap = constantMap' }
  where
    constantMap  = parserStateConstantMap state
    constantMap' = Map.insert c d constantMap

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
constant   = Token.identifier psm
label      = Token.identifier psm
lexeme     = Token.lexeme psm
reserved   = Token.reserved psm
whiteSpace = Token.whiteSpace psm

-- Parses exactly n hexadecimal digits.
hexDigits :: (Enum a) => Int -> CharParser ParserState a
hexDigits n = decode <$> count n hexDigit <* notFollowedBy (identLetter psmDef)
  where decode x = toEnum . fst . head . readHex $ x

-- Parses a 12-bit address value.
addressValue :: CharParser ParserState AddressValue
addressValue = (lexeme $ try $ AddressValue <$> hexDigits 3) <?> "address value"

-- Parses a 8-bit data value.
dataValue :: CharParser ParserState DataValue
dataValue = (lexeme $ try $ DataValue <$> hexDigits 2) <?> "data value"

-- Parses a register name.
register :: CharParser ParserState Register
register = (lexeme $ try $ oneOf "sS" *> hexDigits 1) <?> "register"

-- Parses an operand.
operand :: CharParser ParserState Operand
operand = addressOperand <|> dataOperand <|> registerOperand <|> labelOperand <?> "operand"
  where
    addressOperand  = AddressOperand  <$> addressValue
    dataOperand     = DataOperand     <$> dataValue
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

-- Parses a constant directive.
constantDirective :: CharParser ParserState Statement
constantDirective = do
  d <- reserved "constant" *> (ConstantDirective <$> constant <*> (comma *> dataValue))
  updateState . addConstant $ d
  return d

instruction :: CharParser ParserState Statement
instruction = do
    i <- choice $ nullaryInstructions ++ unaryInstructions ++ binaryInstructions
    updateState incrementAddress
    return i
  where
    nullaryInstructions = map nullaryInstruction nullaryInstructionNames
    unaryInstructions   = map unaryInstruction unaryInstructionNames
    binaryInstructions  = map binaryInstruction binaryInstructionNames

-- Parses a statement and increments the program address.
statement :: CharParser ParserState Statement
statement = do
  l <- optionMaybe (label <* colon)
  updateState $ maybe id addLabel l
  instruction <|> constantDirective

-- Parses multiple statements and returns a label map.
statements :: CharParser ParserState ParserResult
statements = whiteSpace *> ((,,) <$> many statement <*> constantMap <*> labelMap) <* eof
  where
    constantMap = parserStateConstantMap <$> getState
    labelMap    = parserStateLabelMap    <$> getState

-- Parses a file, returning the statements and a label map.
parsePsmFile :: FilePath -> IO (Either ParseError ParserResult)
parsePsmFile filePath = runParser statements parserState filePath <$> readFile filePath
