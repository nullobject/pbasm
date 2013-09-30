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
    Right xs -> do
      let opcodes = runAssembler xs
      print opcodes
