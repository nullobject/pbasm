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
import Parser.State
import Parser.Token

import Control.Applicative hiding (many, optional, (<|>))
import Text.ParserCombinators.Parsec hiding (label)

type ParserResult = ([Statement], ConstantMap, LabelMap)

-- Parses a label and updates the label map.
label :: CharParser ParserState Identifier
label = do
  l <- identifier <* colon
  updateState . addLabel $ l
  return l

-- Parses a constant directive and updates the constant map.
constantDirective :: CharParser ParserState Statement
constantDirective = do
  d <- reserved "constant" *> (ConstantDirective <$> identifier <*> (comma *> dataValue))
  updateState . addConstant $ d
  return d

-- Parses an instruction and increments the program address.
instruction :: CharParser ParserState Statement
instruction = do
    i <- choice $ nullaryInstructions ++ unaryInstructions ++ binaryInstructions
    updateState incrementAddress
    return i
  where
    nullaryInstructions = map nullaryInstruction nullaryInstructionNames
    unaryInstructions   = map unaryInstruction unaryInstructionNames
    binaryInstructions  = map binaryInstruction binaryInstructionNames

-- Parses a statement.
statement :: CharParser ParserState Statement
statement = optional label *> (constantDirective <|> instruction)

-- Parses multiple statements.
statements :: CharParser ParserState ParserResult
statements = whiteSpace *> ((,,) <$> many statement <*> constantMap <*> labelMap) <* eof
  where
    constantMap = parserStateConstantMap <$> getState
    labelMap    = parserStateLabelMap    <$> getState

-- Parses a PSM file.
parsePsmFile :: FilePath -> IO (Either ParseError ParserResult)
parsePsmFile filePath = runParser statements parserState filePath <$> readFile filePath
