-- This module defines the parser state.
module Parser.State
  ( State (..)
  , parserState
  , addConstant
  , addLabel
  , incrementAddress
  ) where

import Core

import Data.Map (empty, insert)

data State = State
  { stateAddress     :: Value
  , stateLabelMap    :: LabelMap
  , stateConstantMap :: ConstantMap
  } deriving (Eq, Show)

parserState :: State
parserState = State
  { stateAddress     = 0
  , stateConstantMap = empty
  , stateLabelMap    = empty
  }

addConstant :: Statement -> State -> State
addConstant (ConstantDirective constant value) state = state {stateConstantMap = constantMap'}
  where constantMap  = stateConstantMap state
        constantMap' = insert constant value constantMap
addConstant _ state = state

addLabel :: Identifier -> State -> State
addLabel identifier state = state {stateLabelMap = labelMap'}
  where address   = stateAddress state
        labelMap  = stateLabelMap state
        labelMap' = insert identifier address labelMap

incrementAddress :: State -> State
incrementAddress state = state {stateAddress = address'}
  where address  = stateAddress state
        address' = address + 1
