module Main where

import Assembler
import Parser

import System.Environment

main :: IO ()
main = do
  input <- getArgs
  result <- parsePsmFile $ head input
  case result of
    Left e -> print e
    Right result -> do
      let opcodes = runAssembler result
      print opcodes
