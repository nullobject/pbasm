module Language.Pbasm.Template.State
  ( State (..),
    templateState,
  )
where

import Language.Pbasm.Core

data State = State
  { -- | Template name
    stateName :: String,
    -- | ROM instructions
    stateOpcodes :: [Opcode],
    -- | True when the template is rendering
    stateRendering :: Bool,
    -- | Current timestamp
    stateTimestamp :: String
  }
  deriving (Eq, Show)

-- Returns the default template state.
templateState :: State
templateState =
  State
    { stateName = "",
      stateOpcodes = [],
      stateRendering = False,
      stateTimestamp = ""
    }
