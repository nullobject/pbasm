-- This module defines the template which converts a template into a PicoBlaze
-- ROM file.
module Language.Pbasm.Template
  ( State (..),
    templateState,
    runTemplate,
  )
where

import Control.Exception (throw)
import Data.Bits
import Data.List.Split (chunksOf)
import Language.Pbasm.Core
import Language.Pbasm.Parser.Token (hexadecimal)
import Language.Pbasm.Template.State
import Text.ParserCombinators.Parsec hiding (Parser, State, label)

type Parser a = CharParser State a

bankSize :: Int
bankSize = 16

-- Asserts the rendering flag.
enableRendering :: State -> State
enableRendering state = state {stateRendering = True}

-- Parses a template string.
templateString :: Parser String
templateString = many1 $ noneOf "{}"

-- Parses a tag.
tag :: Parser String
tag = try $ braces $ evalTag
  where
    braces = between (string "{") (string "}")

-- Parses and evaluates the given tag using the parser state.
evalTag :: Parser String
evalTag = beginTemplateTag <|> nameTag <|> timestampTag <|> romTag <|> romPTag <|> unknownTag <?> "tag"
  where
    unknownTag = do skipMany $ noneOf "{}"; return ""

-- Parses a 'begin template' tag.
beginTemplateTag :: Parser String
beginTemplateTag = try $ string "begin template" >> updateState enableRendering >> return ""

-- Parses a 'name' tag.
nameTag :: Parser String
nameTag = try $ string "name" >> stateName <$> getState

-- Parses a 'timestamp' tag.
timestampTag :: Parser String
timestampTag = try $ string "timestamp" >> stateTimestamp <$> getState

-- Parses a 'INIT_kk' tag.
romTag :: Parser String
romTag = do
  n <- try $ string "INIT_" *> (fromInteger <$> hexadecimal)
  rom <- stateOpcodes <$> getState
  return $ showBank $ romBank rom n
  where
    showBank = foldr (\opcode -> (++ showHex 4 opcode)) ""

-- Parses a 'INITP_kk' tag.
romPTag :: Parser String
romPTag = do
  n <- try $ string "INITP_" *> (fromInteger <$> hexadecimal)
  rom <- stateOpcodes <$> getState
  return $ showBank $ romBankP rom n
  where
    showBank = foldr (\opcode -> (++ showHex 4 opcode)) ""

-- Calculates the 'INIT_kk' ROM bank at the given index from the opcodes.
romBank :: [Opcode] -> Int -> [Opcode]
romBank rom index = banks !! index
  where
    banks = chunksOf' bankSize rom ++ repeat (replicate bankSize 0)

-- Calculates the 'INITP_kk' ROM bank at the given index from the opcodes.
romBankP :: [Opcode] -> Int -> [Opcode]
romBankP rom index = banks !! index
  where
    banks = chunksOf' bankSize rom' ++ repeat (replicate bankSize 0)
    rom' = map pack $ chunksOf' 8 rom
    pack opcodes = fst $ foldl (\(word, n) opcode -> (word .|. f opcode n, n + 2)) (0, 0) opcodes
    f opcode n = (opcode `shiftR` 16 .&. 3) `shiftL` n

-- Splits a list into length-n chunks. The last chunk will be padded with zeros
-- if its length is less than n.
chunksOf' :: (Num a) => Int -> [a] -> [[a]]
chunksOf' n xs = map pad (chunksOf n xs)
  where
    pad ys = ys ++ replicate (n - length ys) 0

-- Replaces the tags in the input string with their values.
template :: Parser [String]
template = many $ ifRendering (tag <|> templateString)

-- Applies the given parser only if the template is rendering.
ifRendering :: Parser String -> Parser String
ifRendering p = do
  result <- p
  rendering <- stateRendering <$> getState
  return $ if rendering then result else ""

-- Processes the template with the given input string and state.
runTemplate :: String -> State -> IO String
runTemplate input state =
  let result = runParser template state "" input
   in case result of
        Right xs -> return $ concat xs
        Left e -> throw $ TemplateException $ show e
