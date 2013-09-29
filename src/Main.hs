module Main where

import Assembler
import Parser

import Text.ParserCombinators.Parsec

main :: IO ()
main = do
  input <- getContents
  case runParser instructions () "" input of
    Left e -> print e
    Right instructions -> do
      let opcodes = runAssembler instructions
      print opcodes
