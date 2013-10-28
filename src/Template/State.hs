module Template.State
  ( State (..)
  , templateState
  ) where

import Core

data State = State
  { stateName      :: String   -- ^ Template name
  , stateOpcodes   :: [Opcode] -- ^ ROM instructions
  , stateRendering :: Bool     -- ^ True when the template is rendering
  , stateTimestamp :: String   -- ^ Current timestamp
  } deriving (Eq, Show)

-- Returns the default template state.
templateState :: State
templateState = State
  { stateName      = ""
  , stateOpcodes   = []
  , stateRendering = False
  , stateTimestamp = ""
  }
