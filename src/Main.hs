module Main where

import Text.ParserCombinators.Parsec

import Parser

main :: IO ()
main = do
  parseTest instructions "LOAD s0, FF\n\
                         \AND s0, 01"
