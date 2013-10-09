-- This module defines the primitive token parsers.
module Parser.Token
  ( colon
  , comma
  , identifier
  , lexeme
  , parens
  , reserved
  , whiteSpace

  , value
  , register
  , pointer
  , operand

  , nullaryInstructionNames
  , unaryInstructionNames
  , binaryInstructionNames
  ) where

import Core

import Control.Applicative hiding (many, optional, (<|>))
import Data.Char (digitToInt)
import Text.ParserCombinators.Parsec hiding (label)
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Token (TokenParser)
import qualified Text.ParserCombinators.Parsec.Token as Token

directiveNames          = ["constant", "string", "table", "address", "include"]
nullaryInstructionNames = ["return"]
unaryInstructionNames   = ["sl0", "sl1", "slx", "sla", "rl", "sr0", "sr1", "srx", "sra", "rr", "call"]
binaryInstructionNames  = ["load", "and", "or", "xor", "add", "addcy", "sub", "subcy", "test", "testcy", "compare", "comparecy", "input", "output"]

psmDef :: LanguageDef st
psmDef = emptyDef
  { Token.caseSensitive   = False
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
parens     = Token.parens psm
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

-- Parses a value.
value :: CharParser u Value
value = (lexeme $ Value <$> integer) <?> "value"
  where integer = hexadecimal <|> decimal <|> binary

-- Parses a register name.
register :: CharParser u Register
register = (lexeme $ try $ oneOf "sS" *> hexadecimal) <?> "register"

-- Parses a pointer.
pointer :: CharParser u Register
pointer = (lexeme $ try $ parens register) <?> "pointer"

-- Parses an operand.
operand :: CharParser u Operand
operand = valueOperand <|> registerOperand <|> identifierOperand <?> "operand"
  where valueOperand      = ValueOperand      <$> value
        identifierOperand = IdentifierOperand <$> identifier
        registerOperand   = RegisterOperand   <$> (register <|> pointer)
