-- This module defines the template which converts a template into a PicoBlaze
-- ROM file.
module Template
  ( State (..)
  , templateState
  , runTemplate
  ) where

import Core
import Parser.Token (hexadecimal)

import Control.Applicative hiding (many, optional, (<|>))
import Control.Exception (throw)
import Data.Bits
import Data.List.Split (chunksOf)
import Text.ParserCombinators.Parsec hiding (State, label)
import Text.Printf

data State = State
    -- A flag representing whether the template is rendering.
  { stateRendering :: Bool

    -- The template name.
  , stateName :: String

    -- The ROM instructions.
  , stateROM :: [Opcode]

    -- The current timestamp.
  , stateTimestamp :: String
  } deriving (Eq, Show)

bankSize :: Int
bankSize = 16

-- Returns the default template state.
templateState :: State
templateState = State
  { stateRendering = False
  , stateName      = ""
  , stateROM       = []
  , stateTimestamp = ""
  }

showHex :: Opcode -> String
showHex opcode = printf "%04X" $ opcode .&. 0xFFFF

-- Asserts the rendering flag.
enableRendering :: State -> State
enableRendering state = state {stateRendering = True}

-- Parses a template string.
templateString :: CharParser State String
templateString = many1 $ noneOf "{}"

-- Parses a tag.
tag :: CharParser State String
tag = try $ braces $ evalTag
  where braces = between (string "{") (string "}")

-- Parses and evaluates the given tag using the parser state.
evalTag :: CharParser State String
evalTag = beginTemplateTag <|> nameTag <|> timestampTag <|> romTag <|> romPTag <|> unknownTag <?> "tag"
  where unknownTag = do {skipMany $ noneOf "{}"; return ""}

-- Parses a 'begin template' tag.
beginTemplateTag :: CharParser State String
beginTemplateTag = try $ string "begin template" >> updateState enableRendering >> return ""

-- Parses a 'name' tag.
nameTag :: CharParser State String
nameTag = try $ string "name" >> stateName <$> getState

-- Parses a 'timestamp' tag.
timestampTag :: CharParser State String
timestampTag = try $ string "timestamp" >> stateTimestamp <$> getState

-- Parses a 'INIT_kk' tag.
romTag :: CharParser State String
romTag = do
  n <- try $ string "INIT_" *> (fromInteger <$> hexadecimal)
  rom <- stateROM <$> getState
  return $ showBank $ romBank rom n
  where showBank = foldr (\opcode -> (++ showHex opcode)) ""

-- Parses a 'INITP_kk' tag.
romPTag :: CharParser State String
romPTag = do
  n <- try $ string "INITP_" *> (fromInteger <$> hexadecimal)
  rom <- stateROM <$> getState
  return $ showBank $ romBankP rom n
  where showBank = foldr (\opcode -> (++ showHex opcode)) ""

-- Calculates the 'INIT_kk' ROM bank at the given index from the opcodes.
romBank :: [Opcode] -> Int -> [Opcode]
romBank rom index = banks !! index
  where banks = (chunksOf' bankSize rom) ++ (repeat $ replicate bankSize 0)

-- Calculates the 'INITP_kk' ROM bank at the given index from the opcodes.
romBankP :: [Opcode] -> Int -> [Opcode]
romBankP rom index = banks !! index
  where banks        = (chunksOf' bankSize rom') ++ (repeat $ replicate bankSize 0)
        rom'         = map pack $ chunksOf' 8 rom
        pack opcodes = fst $ foldl (\(word, n) opcode -> (word .|. f opcode n, n + 2)) (0, 0) opcodes
        f opcode n   = ((opcode `shiftR` 16) .&. 3) `shiftL` n

-- Splits a list into length-n chunks. The last chunk will be padded with zeros
-- if its length is less than n.
chunksOf' :: Num a => Int -> [a] -> [[a]]
chunksOf' n xs = map pad (chunksOf n xs)
  where pad ys = ys ++ (replicate (n - length ys) 0)

-- Replaces the tags in the input string with their values.
template :: CharParser State [String]
template = many $ ifRendering (tag <|> templateString)

-- Applies the given parser only if the template is rendering.
ifRendering :: CharParser State String -> CharParser State String
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