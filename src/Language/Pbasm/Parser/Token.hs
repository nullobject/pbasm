-- This module defines the primitive token parsers.
module Language.Pbasm.Parser.Token
  ( colon,
    comma,
    identifier,
    parens,
    reserved,
    reservedOp,
    whiteSpace,
    hexadecimal,
    decimal,
    binary,
    value,
    register,
    pointer,
    condition,
    operand,
    nullaryInstructionNames,
    unaryInstructionNames,
    binaryInstructionNames,
  )
where

import Data.Char (digitToInt)
import Data.List (nub)
import Data.List.Split (splitOn)
import Language.Pbasm.Core
import Text.ParserCombinators.Parsec hiding (label)
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Token (TokenParser)
import qualified Text.ParserCombinators.Parsec.Token as Token

directiveNames :: [String]
directiveNames = ["constant", "string", "table", "address", "include"]

nullaryInstructionNames :: [String]
nullaryInstructionNames = ["enable interrupt", "disable interrupt", "returni disable", "returni enable", "return"]

unaryInstructionNames :: [String]
unaryInstructionNames = ["regbank", "sl0", "sl1", "slx", "sla", "rl", "sr0", "sr1", "srx", "sra", "rr", "jump", "call"]

binaryInstructionNames :: [String]
binaryInstructionNames = ["load", "star", "and", "or", "xor", "add", "addcy", "sub", "subcy", "test", "testcy", "compare", "comparecy", "input", "output", "outputk", "store", "fetch", "jump", "jump@", "call", "call@", "load&return"]

conditionNames :: [String]
conditionNames = ["z", "nz", "c", "nc"]

psmDef :: LanguageDef st
psmDef =
  emptyDef
    { Token.caseSensitive = False,
      Token.commentStart = "",
      Token.commentEnd = "",
      Token.commentLine = ";",
      Token.identLetter = alphaNum <|> char '_',
      Token.nestedComments = False,
      Token.reservedNames = nub $ concatMap (splitOn " ") names,
      Token.reservedOpNames = ["~"]
    }
  where
    names = directiveNames ++ nullaryInstructionNames ++ unaryInstructionNames ++ binaryInstructionNames ++ conditionNames

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

reservedOp :: String -> CharParser u ()
reservedOp = Token.reservedOp psm

whiteSpace :: CharParser u ()
whiteSpace = Token.whiteSpace psm

binDigit :: CharParser u Char
binDigit = oneOf "01"

-- | Parses a base-n number using the given digit parser.
number :: Int -> CharParser u Char -> CharParser u Int
number base baseDigit = do
  digits <- many1 baseDigit
  let n = foldl (\x d -> base * x + digitToInt d) 0 digits
  seq n (return n)

-- | Parses a hexadecimal number.
hexadecimal :: (Enum a) => CharParser u a
hexadecimal = try $ toEnum <$> number 16 hexDigit <* notFollowedBy alphaNum

-- | Parses a decimal number.
decimal :: (Enum a) => CharParser u a
decimal = try $ toEnum <$> number 10 digit <* char '\'' <* oneOf "dD"

-- | Parses a binary number.
binary :: (Enum a) => CharParser u a
binary = try $ toEnum <$> number 2 binDigit <* char '\'' <* oneOf "bB"

-- | Parses an ASCII character.
character :: (Enum a) => CharParser u a
character = try $ toEnum . fromEnum <$> between (char '"') (char '"' <?> "end of character") charLetter
  where
    charLetter = satisfy (\c -> c /= '\'' && c /= '\\' && c > '\026')

-- | Parses a value.
value :: CharParser u Value
value = lexeme (Value <$> integer) <?> "value"
  where
    integer = character <|> decimal <|> binary <|> hexadecimal

-- | Parses a register name.
register :: CharParser u Register
register = lexeme (try $ oneOf "sS" *> hexadecimal) <?> "register"

-- | Parses a pointer.
pointer :: CharParser u Register
pointer = lexeme (try $ parens register) <?> "pointer"

-- | Parses a condition.
condition :: CharParser u Condition
condition = lexeme (readCondition <$> choice ps) <?> "condition"
  where
    ps = map (\name -> name <$ reserved name) conditionNames

-- | Parses an operand.
operand :: CharParser u Operand
operand = valueOperand <|> registerOperand <|> conditionOperand <|> identifierOperand <?> "operand"
  where
    valueOperand = ValueOperand <$> value
    registerOperand = RegisterOperand <$> (register <|> pointer)
    conditionOperand = ConditionOperand <$> condition
    identifierOperand = invertedIdentifierOperand <|> upperIdentifierOperand <|> lowerIdentifierOperand <|> normalIdentifierOperand

    normalIdentifierOperand = flip IdentifierOperand Nothing <$> identifier
    invertedIdentifierOperand = flip IdentifierOperand (Just InvertModifier) <$> try (reservedOp "~" *> identifier)
    lowerIdentifierOperand = flip IdentifierOperand (Just LowerModifier) <$> try (identifier <* string "'lower")
    upperIdentifierOperand = flip IdentifierOperand (Just UpperModifier) <$> try (identifier <* string "'upper")
