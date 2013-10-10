module Main where

import Assembler
import Parser

import System.Environment
import System.FilePath
import Text.Printf

main :: IO ()
main = do
  args <- getArgs
  let inputFileName = head args
  result <- parsePsmFile inputFileName
  case result of
    Left e -> print e
    Right (statements, constantMap, labelMap) -> do
      let opcodes = runAssembler statements constantMap labelMap
      let outputFileName = takeBaseName inputFileName <.> "hex"
      let result = unlines $ map (printf "%05X") opcodes
      writeFile outputFileName result
