{-# LANGUAGE DeriveDataTypeable #-}

module Language.Pbasm.Core where

import Control.Exception (Exception)
import Data.Bits
import Data.Map (Map)
import Data.Typeable (Typeable)
import Data.Word (Word32)
import Text.Printf

type Identifier = String

type Opcode = Word32

-- An 8-bit data value.
newtype Value = Value Word32 deriving (Eq, Show)

instance Num Value where
  (Value x) + (Value y) = Value (x + y)
  (Value x) - (Value y) = Value (x - y)
  (Value x) * (Value y) = Value (x * y)
  abs x                 = x
  signum 0              = 0
  signum _              = 1
  fromInteger i         = Value (fromInteger i)

-- The PicoBlaze has 16 general purpose 8-bit registers.
data Register =
    Register0 | Register1 | Register2 | Register3
  | Register4 | Register5 | Register6 | Register7
  | Register8 | Register9 | RegisterA | RegisterB
  | RegisterC | RegisterD | RegisterE | RegisterF
  deriving (Enum, Eq, Show)

data Condition =
    ZeroCondition
  | NotZeroCondition
  | CarryCondition
  | NotCarryCondition
  deriving (Enum, Eq, Show)

readCondition :: String -> Condition
readCondition s
  | s == "z"  = ZeroCondition
  | s == "nz" = NotZeroCondition
  | s == "c"  = CarryCondition
  | s == "nc" = NotCarryCondition
  | otherwise = error $ "Unknown condition: " ++ s

data Modifier
  = InvertModifier -- ^ Invert the constant value.
  | LowerModifier  -- ^ The lower 8-bits of the address value.
  | UpperModifier  -- ^ The upper 4-bits of the address value.
  deriving (Eq, Show)

data Operand =
    ValueOperand Value
  | IdentifierOperand Identifier (Maybe Modifier)
  | RegisterOperand Register
  | ConditionOperand Condition
  deriving (Eq, Show)

data Statement =
    -- Assigns a name to a constant value.
    ConstantDirective Identifier Value

    -- Defines a list of characters for use with OUTPUTK and LOAD&RETURN
    -- instructions.
  | StringDirective Identifier String

    -- Defines a list of values for use with OUTPUTK and LOAD&RETURN
    -- instructions.
  | TableDirective Identifier [Value]

    -- Forces the assembler to assemble all subsequent instructions starting at
    -- the address defined.
  | AddressDirective Value

    -- Inserts the contents of another file into the program immediately
    -- following the directive.
  | IncludeDirective FilePath

    -- An instruction of arity 0.
  | NullaryInstruction String

    -- An instruction of arity 1.
  | UnaryInstruction String Operand

    -- An instruction of arity 2.
  | BinaryInstruction String Operand Operand
  deriving (Eq, Show)

-- A map from constants to data values.
type ConstantMap = Map Identifier Value

-- A map from labels to address values.
type LabelMap = Map Identifier Value

type ParserResult = ([Statement], ConstantMap, LabelMap)

data PbasmException =
    ParserException String
  | AssemblerException String
  | TemplateException String
  deriving (Eq, Show, Typeable)

instance Exception PbasmException

isParserError :: PbasmException -> Bool
isParserError (ParserException _) = True
isParserError _ = False

isAssemblerError :: PbasmException -> Bool
isAssemblerError (AssemblerException _) = True
isAssemblerError _ = False

isTemplateError :: PbasmException -> Bool
isTemplateError (TemplateException _) = True
isTemplateError _ = False

-- Prints the first n hexadecimal digits of the given opcode.
showHex :: Int -> Opcode -> String
showHex n opcode = printf format value
  where format = "%0" ++ show n ++ "X"
        value  = opcode .&. ((1 `shiftL` (n * 4)) - 1)
