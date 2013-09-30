module Main where

import Assembler
import Parser

import System.Environment
import Text.ParserCombinators.Parsec

main :: IO ()
main = do
  input <- getArgs
  result <- parseFromFile instructions (head input)
  case result of
    Left e -> print e
    Right instructions -> do
      let opcodes = runAssembler instructions
      print opcodes
