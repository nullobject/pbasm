-- This module defines the parser which is used to parse the assembly source
-- code into an AST.
module Parser
  ( directive
  , instruction
  , statements
  , parsePsmFile
  ) where

import Core
import Parser.State
import Parser.Token

import Control.Applicative hiding (many, optional, (<|>))
import Text.ParserCombinators.Parsec hiding (State, label)

type Result = ([Statement], ConstantMap, LabelMap)

-- Parses a label and updates the label map.
label :: CharParser State Identifier
label = do
  l <- identifier <* colon
  updateState . addLabel $ l
  return l

-- Parses a constant directive and updates the constant map.
constantDirective :: CharParser State Statement
constantDirective = do
  d <- reserved "constant" *> (ConstantDirective <$> identifier <*> (comma *> value))
  updateState . addConstant $ d
  return d

-- Parses an instruction of arity 0.
nullaryInstruction :: String -> CharParser u Statement
nullaryInstruction name = try $ NullaryInstruction name <$ reserved name

-- Parses an instruction of arity 1.
unaryInstruction :: String -> CharParser u Statement
unaryInstruction name = try $ reserved name *> (UnaryInstruction name <$> operand)

-- Parses an instruction of arity 2.
binaryInstruction :: String -> CharParser u Statement
binaryInstruction name = try $ reserved name *> (BinaryInstruction name <$> operand <*> (comma *> operand))

-- Parses a directive.
directive :: CharParser State Statement
directive = constantDirective

-- Parses an instruction and increments the program address.
instruction :: CharParser State Statement
instruction = do
    i <- choice $ binaryInstructions ++ unaryInstructions ++ nullaryInstructions
    updateState incrementAddress
    return i
  where nullaryInstructions = map nullaryInstruction nullaryInstructionNames
        unaryInstructions   = map unaryInstruction   unaryInstructionNames
        binaryInstructions  = map binaryInstruction  binaryInstructionNames

-- Parses a statement.
statement :: CharParser State Statement
statement = optional label *> (constantDirective <|> instruction)

-- Parses multiple statements.
statements :: CharParser State Result
statements = whiteSpace *> ((,,) <$> many statement <*> constantMap <*> labelMap) <* eof
  where constantMap = stateConstantMap <$> getState
        labelMap    = stateLabelMap    <$> getState

-- Parses a PSM file.
parsePsmFile :: FilePath -> IO (Either ParseError Result)
parsePsmFile filePath = runParser statements parserState filePath <$> readFile filePath
