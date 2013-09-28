module Main where

import Assembler
import Parser

import Text.ParserCombinators.Parsec

main :: IO ()
main = do
  parseTest instructions "LOAD s0, FF\n\
                         \AND s0, 01"
