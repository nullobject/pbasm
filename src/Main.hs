module Main where

import Assembler
import Parser

import System.Environment
import System.FilePath
import System.IO
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
      out <- openFile outputFileName WriteMode
      mapM_ (hPrintf out "%05X\n") opcodes
      hClose out
