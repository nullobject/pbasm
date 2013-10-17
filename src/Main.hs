module Main where

import Assembler
import CLI
import Parser

import System.Console.CmdArgs
import System.FilePath
import Text.Printf

main :: IO ()
main = do
  x <- cmdArgs pbasm
  print x
  assembleFile $ file x

assembleFile :: FilePath -> IO ()
assembleFile "" = putStrLn "no input file"
assembleFile x = do
  putStrLn $ "Assembling " ++ show x
  result <- parsePsmFile x
  case result of
    Left e -> print e
    Right (statements, constantMap, labelMap) -> do
      let opcodes = runAssembler statements constantMap labelMap
      let outputFileName = takeBaseName x <.> "hex"
      let result = unlines $ map (printf "%05X") opcodes
      writeFile outputFileName result
