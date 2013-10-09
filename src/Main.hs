module Main where

import Assembler
import Parser

import System.Environment
import Text.Printf (printf)

main :: IO ()
main = do
  input <- getArgs
  result <- parsePsmFile $ head input
  case result of
    Left e -> print e
    Right (statements, constantMap, labelMap) -> do
      let opcodes = runAssembler statements constantMap labelMap
      mapM_ (printf "%05X\n") opcodes
