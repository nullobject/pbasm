module Main where

import Text.ParserCombinators.Parsec

import Parser

main :: IO ()
main = do
  parseTest instruction "LOAD s0, s1"
  parseTest instruction "LOAD s2, 0f"
  parseTest instruction "ADD s3, s4"
  parseTest instruction "ADD s5, f0"
  parseTest instruction "CALL ff0"
  parseTest instruction "JUMP fff"
  parseTest instruction "RETURN"

  putStrLn "lol:\n\
  \  LOAD s0, FF\n\
  \  AND s0, 01"
