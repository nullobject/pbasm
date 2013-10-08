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
import Data.Char (digitToInt)
import qualified Data.Map as Map
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

addLabel :: Identifier -> ParserState -> ParserState
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
identifier = Token.identifier psm
lexeme     = Token.lexeme psm
reserved   = Token.reserved psm
whiteSpace = Token.whiteSpace psm

-- Parses a base-n number using the given digit parser.
number :: Int -> CharParser ParserState Char -> CharParser ParserState Int
number base baseDigit = do
  digits <- many1 baseDigit
  let n = foldl (\x d -> (base * x) + digitToInt d) 0 digits
  seq n (return n)

-- Parses a hexadecimal number.
hexadecimal :: (Enum a) => CharParser ParserState a
hexadecimal = try $ toEnum <$> number 16 hexDigit <* notFollowedBy (identLetter psmDef)

-- Parses a decimal number.
decimal :: (Enum a) => CharParser ParserState a
decimal = try $ toEnum <$> number 10 digit <* string "'" <* oneOf "dD" <* notFollowedBy (identLetter psmDef)

-- Parses a binary number.
binary :: (Enum a) => CharParser ParserState a
binary = try $ toEnum <$> number 2 binDigit <* string "'" <* oneOf "bB" <* notFollowedBy (identLetter psmDef)
  where binDigit = oneOf "01"

-- Parses an integer in hexadecimal, decimal, or binary format.
integer :: (Enum a) => CharParser ParserState a
integer = choice [hexadecimal, decimal, binary]

-- Parses a 12-bit address value.
addressValue :: CharParser ParserState AddressValue
addressValue = (lexeme $ AddressValue <$> integer) <?> "address value"

-- Parses a 8-bit data value.
dataValue :: CharParser ParserState DataValue
dataValue = (lexeme $ DataValue <$> integer) <?> "data value"

-- Parses a register name.
register :: CharParser ParserState Register
register = (lexeme $ try $ oneOf "sS" *> hexadecimal) <?> "register"

-- Parses an operand.
operand :: CharParser ParserState Operand
operand = addressOperand <|> dataOperand <|> registerOperand <|> identifierOperand <?> "operand"
  where
    addressOperand    = AddressOperand    <$> addressValue
    dataOperand       = DataOperand       <$> dataValue
    identifierOperand = IdentifierOperand <$> identifier
    registerOperand   = RegisterOperand   <$> register

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
  d <- reserved "constant" *> (ConstantDirective <$> identifier <*> (comma *> dataValue))
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

label :: CharParser ParserState Identifier
label = do
  l <- identifier <* colon
  updateState . addLabel $ l
  return l

-- Parses a statement and increments the program address.
statement :: CharParser ParserState Statement
statement = optional label *> (constantDirective <|> instruction)

-- Parses multiple statements and returns a label map.
statements :: CharParser ParserState ParserResult
statements = whiteSpace *> ((,,) <$> many statement <*> constantMap <*> labelMap) <* eof
  where
    constantMap = parserStateConstantMap <$> getState
    labelMap    = parserStateLabelMap    <$> getState

-- Parses a file, returning the statements and a label map.
parsePsmFile :: FilePath -> IO (Either ParseError ParserResult)
parsePsmFile filePath = runParser statements parserState filePath <$> readFile filePath
