-- This module defines the parser which is used to parse the assembly source
-- code into an AST.
module Language.Pbasm.Parser
  ( Parser,
    statement,
    statements,
    parsePsmFile,
  )
where

import Control.Exception (throw)
import Language.Pbasm.Core
import Language.Pbasm.Parser.State
import Language.Pbasm.Parser.Token
import Text.ParserCombinators.Parsec hiding (Parser, State, label)

type Parser a = CharParser State a

-- Parses a label and updates the label map.
label :: Parser Identifier
label = do
  l <- identifier <* colon
  updateState . addLabel $ l
  return l

-- Parses a constant directive and updates the constant map.
constantDirective :: Parser Statement
constantDirective = do
  d <- reserved "constant" *> (ConstantDirective <$> identifier <*> (comma *> value))
  updateState . addConstant $ d
  return d

-- Parses a directive.
directive :: Parser Statement
directive = constantDirective

-- The nullary instructions are special-cased because they include two-word
-- instructions. Ideally we could generate these automatically.
nullaryInstructions :: [Parser Statement]
nullaryInstructions =
  [ returnInstruction,
    disableInterruptInstruction,
    enableInterruptInstruction,
    returniDisableInstruction,
    returniEnableInstruction
  ]
  where
    returnInstruction = NullaryInstruction "return" <$ reserved "return"
    disableInterruptInstruction = NullaryInstruction "disable interrupt" <$ reserved "disable" <* reserved "interrupt"
    enableInterruptInstruction = NullaryInstruction "enable interrupt" <$ reserved "enable" <* reserved "interrupt"
    returniDisableInstruction = NullaryInstruction "returni disable" <$ reserved "returni" <* reserved "disable"
    returniEnableInstruction = NullaryInstruction "returni enable" <$ reserved "returni" <* reserved "enable"

-- Parses an instruction of arity 1.
unaryInstruction :: String -> CharParser u Statement
unaryInstruction name = try $ reserved name *> (UnaryInstruction name <$> operand)

-- Parses an instruction of arity 2.
binaryInstruction :: String -> CharParser u Statement
binaryInstruction name = try $ reserved name *> (BinaryInstruction name <$> operand <*> (comma *> operand))

-- Parses an instruction and increments the program address.
instruction :: Parser Statement
instruction = do
  i <- choice $ binaryInstructions ++ unaryInstructions ++ nullaryInstructions
  updateState incrementAddress
  return i
  where
    unaryInstructions = map unaryInstruction unaryInstructionNames
    binaryInstructions = map binaryInstruction binaryInstructionNames

-- Parses a statement.
statement :: Parser Statement
statement = optional label *> (directive <|> instruction)

-- Parses multiple statements.
statements :: Parser ParserResult
statements = whiteSpace *> ((,,) <$> many statement <*> constantMap <*> labelMap) <* eof
  where
    constantMap = stateConstantMap <$> getState
    labelMap = stateLabelMap <$> getState

-- Parses a PSM file.
parsePsmFile :: FilePath -> IO ParserResult
parsePsmFile filePath = do
  result <- runParser statements parserState filePath <$> readFile filePath
  case result of
    Right x -> return x
    Left e -> throw $ ParserException $ show e
