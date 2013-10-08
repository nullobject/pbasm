-- This module defines the parser state.
module Parser.State where

import Core

import qualified Data.Map as Map

data ParserState = ParserState {
    parserStateAddress     :: AddressValue
  , parserStateLabelMap    :: LabelMap
  , parserStateConstantMap :: ConstantMap
  } deriving (Show)

parserState = ParserState {
    parserStateAddress     = 0
  , parserStateConstantMap = Map.empty
  , parserStateLabelMap    = Map.empty
  }

addConstant :: Statement -> ParserState -> ParserState
addConstant (ConstantDirective constant dataValue) state = state { parserStateConstantMap = constantMap' }
  where
    constantMap  = parserStateConstantMap state
    constantMap' = Map.insert constant dataValue constantMap

addLabel :: Identifier -> ParserState -> ParserState
addLabel identifier state = state { parserStateLabelMap = labelMap' }
  where
    address   = parserStateAddress state
    labelMap  = parserStateLabelMap state
    labelMap' = Map.insert identifier address labelMap

incrementAddress :: ParserState -> ParserState
incrementAddress state = state { parserStateAddress = address' }
  where
    address  = parserStateAddress state
    address' = address + 1
