{-# LANGUAGE DeriveDataTypeable #-}

-- This module defines the command-line interface.
module CLI
  (
    Pbasm (..)
  , pbasm
  ) where

import System.Console.CmdArgs

data Pbasm = Pbasm
  { pbasmInputFilePath    :: FilePath       -- ^ Input file path
  , pbasmTemplateFilePath :: Maybe FilePath -- ^ Optional template file path
  } deriving (Show, Data, Typeable)

pbasm :: Pbasm
pbasm = Pbasm
  { pbasmInputFilePath    = def &= args &= typFile
  , pbasmTemplateFilePath = def &= explicit &= name "template" &= typFile &= opt "ROM_form.vhd" &= help "Run the templater with an optional file"
  }
  &= summary "pbasm 0.1.0"
  &= details ["For more information visit: https://github.com/nullobject/pbasm"]
