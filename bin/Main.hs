module Main where

import CLI
  ( Pbasm (..),
    pbasm,
  )
import Language.Pbasm.Assembler (runAssembler)
import Language.Pbasm.Core (Opcode, showHex)
import Language.Pbasm.Parser (parsePsmFile)
import Language.Pbasm.Template (State (..), runTemplate, templateState)
import System.Console.CmdArgs (cmdArgs)
import System.FilePath (takeBaseName, (<.>))

main :: IO ()
main = do
  args <- cmdArgs pbasm

  let inputFilePath = pbasmInputFilePath args
  let templateFilePath = pbasmTemplateFilePath args
  let name = takeBaseName inputFilePath
  let hexFilePath = takeBaseName inputFilePath <.> "hex"
  let entityFilePath = name <.> "vhd"

  assembleFile inputFilePath hexFilePath >>= renderTemplate templateFilePath entityFilePath name >> return ()

-- | Assembles the file at the given file path.
assembleFile :: FilePath -> FilePath -> IO [Opcode]
assembleFile "" _ = do putStrLn "no input file"; return []
assembleFile inputFilePath hexFilePath = do
  parsePsmFile inputFilePath >>= runAssembler >>= writeHexFile hexFilePath

-- | Writes the opcodes to the given file path.
writeHexFile :: FilePath -> [Opcode] -> IO [Opcode]
writeHexFile hexFilePath opcodes = do
  let output = unlines $ map (showHex 5) opcodes
  writeFile hexFilePath output
  return opcodes

-- | Renders the template to the given file path.
renderTemplate :: Maybe FilePath -> FilePath -> String -> [Opcode] -> IO [Opcode]
renderTemplate Nothing _ _ opcodes = return opcodes
renderTemplate (Just templateFilePath) entityFilePath name opcodes = do
  input <- readFile templateFilePath
  output <- runTemplate input $ templateState {stateName = name, stateOpcodes = opcodes}
  writeFile entityFilePath output
  return opcodes
