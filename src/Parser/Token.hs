-- This module defines the primitive token parsers.
module Parser.Token
  ( colon
  , comma
  , identifier
  , reserved
  , whiteSpace

  , value
  , register
  , pointer
  , condition
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

directiveNames :: [String]
directiveNames = ["constant", "string", "table", "address", "include"]

nullaryInstructionNames :: [String]
nullaryInstructionNames = ["return"]

unaryInstructionNames :: [String]
unaryInstructionNames = ["sl0", "sl1", "slx", "sla", "rl", "sr0", "sr1", "srx", "sra", "rr", "call", "jump"]

binaryInstructionNames :: [String]
binaryInstructionNames = ["load", "and", "or", "xor", "add", "addcy", "sub", "subcy", "test", "testcy", "compare", "comparecy", "input", "output", "jump"]

conditionNames :: [String]
conditionNames = ["z", "nz", "c", "nc"]

psmDef :: LanguageDef st
psmDef = emptyDef
  { Token.caseSensitive   = False
  , Token.commentStart    = ""
  , Token.commentEnd      = ""
  , Token.commentLine     = ";"
  , Token.nestedComments  = False
  , Token.reservedNames   = directiveNames ++ nullaryInstructionNames ++ unaryInstructionNames ++ binaryInstructionNames ++ conditionNames
  , Token.reservedOpNames = ["~"]
  }

psm :: TokenParser st
psm = Token.makeTokenParser psmDef

colon :: CharParser u String
colon = Token.colon psm

comma :: CharParser u String
comma = Token.comma psm

identifier :: CharParser u String
identifier = Token.identifier psm

lexeme :: CharParser u a -> CharParser u a
lexeme = Token.lexeme psm

parens :: CharParser u a -> CharParser u a
parens = Token.parens psm

reserved :: String -> CharParser u ()
reserved = Token.reserved psm

whiteSpace :: CharParser u ()
whiteSpace = Token.whiteSpace psm

binDigit :: CharParser u Char
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

-- Parses a condition.
condition :: CharParser u Condition
condition = (lexeme $ readCondition <$> choice ps) <?> "condition"
  where ps = map (\name -> name <$ reserved name) conditionNames

-- Parses an operand.
operand :: CharParser u Operand
operand = valueOperand <|> registerOperand <|> conditionOperand <|> identifierOperand <?> "operand"
  where valueOperand      = ValueOperand      <$> value
        identifierOperand = IdentifierOperand <$> identifier
        registerOperand   = RegisterOperand   <$> (register <|> pointer)
        conditionOperand  = ConditionOperand  <$> condition
