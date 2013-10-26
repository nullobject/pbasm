{-# LANGUAGE DeriveDataTypeable #-}

-- This module defines the command-line interface.
module CLI
  (
    Pbasm (..)
  , pbasm
  ) where

import System.Console.CmdArgs

data Pbasm = Pbasm
  { file     :: FilePath -- ^ Input file path
  , template :: FilePath -- ^ Template file path
  } deriving (Show, Data, Typeable)

pbasm :: Pbasm
pbasm = Pbasm
  {
    template = def &= typFile &= opt "ROM_form.vhd" &= help "Run the templater with an optional file"
  , file     = def &= args &= typFile
  }
  &= summary "pbasm 0.1.0"
  &= details ["For more information visit: https://github.com/nullobject/pbasm"]
