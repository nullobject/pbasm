module Main where

import Assembler
import Core
import CLI
import Parser
import Template

import System.Console.CmdArgs hiding (name)
import System.FilePath
import Text.Printf

main :: IO ()
main = do
  x <- cmdArgs pbasm
  _ <- assembleFile $ file x
  return ()

-- Assembles the file at the given file path.
assembleFile :: FilePath -> IO [Opcode]
assembleFile "" = do {putStrLn "no input file"; return []}
assembleFile psmFilePath = do
  let hexFilePath    = takeBaseName psmFilePath <.> "hex"
  let romFilePath    = takeDirectory psmFilePath </> "ROM_form.vhd"
  let entityFilePath = takeBaseName hexFilePath <.> "vhd"

  parsePsmFile psmFilePath
    >>= runAssembler
    >>= writeHexFile hexFilePath
    >>= renderTemplate romFilePath entityFilePath (takeBaseName hexFilePath)

-- Renders the template to the given file path.
renderTemplate :: FilePath -> FilePath -> String -> [Opcode] -> IO [Opcode]
renderTemplate inputFilePath outputFilePath name opcodes = do
  input <- readFile inputFilePath
  output <- runTemplate input $ templateState {stateName = name, stateROM = opcodes}
  writeFile outputFilePath output
  return opcodes

-- Writes the opcodes to the given file path.
writeHexFile :: FilePath -> [Opcode] -> IO [Opcode]
writeHexFile outputFilePath opcodes = do
  let output = unlines $ map (printf "%05X") opcodes
  writeFile outputFilePath output
  return opcodes
