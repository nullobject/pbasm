module Main where

import Assembler
import Core
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
assembleFile psmFilePath = do
  putStrLn $ "Assembling " ++ show psmFilePath
  let hexFilePath = takeBaseName psmFilePath <.> "hex"
  parsePsmFile psmFilePath >>= runAssembler >>= writeHexFile hexFilePath

-- Writes the opcodes to the given file path.
writeHexFile :: FilePath -> [Opcode] -> IO ()
writeHexFile hexFilePath opcodes = do
  let s = unlines $ map (printf "%05X") opcodes
  writeFile hexFilePath s
