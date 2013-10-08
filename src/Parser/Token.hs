-- This module defines the primitive token parsers.
module Parser.Token where

import Core

import Control.Applicative hiding (many, optional, (<|>))
import Data.Char (digitToInt)
import Text.ParserCombinators.Parsec hiding (label)
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Token (TokenParser)
import qualified Text.ParserCombinators.Parsec.Token as Token

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

binDigit = oneOf "01"

-- Parses a base-n number using the given digit parser.
number :: Int -> CharParser u Char -> CharParser u Int
number base baseDigit = do
  digits <- many1 baseDigit
  let n = foldl (\x d -> (base * x) + digitToInt d) 0 digits
  seq n (return n)

-- Parses a hexadecimal number.
hexadecimal :: (Enum a) => CharParser u a
hexadecimal = try $ toEnum <$> number 16 hexDigit <* notFollowedBy (identLetter psmDef)

-- Parses a decimal number.
decimal :: (Enum a) => CharParser u a
decimal = try $ toEnum <$> number 10 digit <* string "'" <* oneOf "dD" <* notFollowedBy (identLetter psmDef)

-- Parses a binary number.
binary :: (Enum a) => CharParser u a
binary = try $ toEnum <$> number 2 binDigit <* string "'" <* oneOf "bB" <* notFollowedBy (identLetter psmDef)

-- Parses an integer in hexadecimal, decimal, or binary format.
integer :: (Enum a) => CharParser u a
integer = hexadecimal <|> decimal <|> binary

-- Parses a 12-bit address value.
addressValue :: CharParser u AddressValue
addressValue = (lexeme $ AddressValue <$> integer) <?> "address value"

-- Parses a 8-bit data value.
dataValue :: CharParser u DataValue
dataValue = (lexeme $ DataValue <$> integer) <?> "data value"

-- Parses a register name.
register :: CharParser u Register
register = (lexeme $ try $ oneOf "sS" *> hexadecimal) <?> "register"

-- Parses an operand.
operand :: CharParser u Operand
operand = addressOperand <|> dataOperand <|> registerOperand <|> identifierOperand <?> "operand"
  where
    addressOperand    = AddressOperand    <$> addressValue
    dataOperand       = DataOperand       <$> dataValue
    identifierOperand = IdentifierOperand <$> identifier
    registerOperand   = RegisterOperand   <$> register

-- Parses an instruction of arity 0.
nullaryInstruction :: String -> CharParser u Statement
nullaryInstruction name = NullaryInstruction name <$ reserved name

-- Parses an instruction of arity 1.
unaryInstruction :: String -> CharParser u Statement
unaryInstruction name = reserved name *> (UnaryInstruction name <$> operand)

-- Parses an instruction of arity 2.
binaryInstruction :: String -> CharParser u Statement
binaryInstruction name = reserved name *> (BinaryInstruction name <$> operand <*> (comma *> operand))
