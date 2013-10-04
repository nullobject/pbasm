module Core where

import Data.Map
import Data.Word

type Label = String
type Name  = String

-- A 12-bit program address.
newtype Address = Address Word16 deriving (Eq, Show)

-- An 8-bit constant value.
newtype Constant = Constant Word8 deriving (Eq, Show)

-- The PicoBlaze has 16 general purpose 8-bit registers.
data Register =
    Register0 | Register1 | Register2 | Register3
  | Register4 | Register5 | Register6 | Register7
  | Register8 | Register9 | RegisterA | RegisterB
  | RegisterC | RegisterD | RegisterE | RegisterF
  deriving (Enum, Eq, Show)

data Operand =
    AddressOperand  Address
  | ConstantOperand Constant
  | LabelOperand    Label
  | RegisterOperand Register
  deriving (Eq, Show)

data Statement =
    -- Assigns a name to a constant value. For example:
    ConstantDirective Name Constant

    -- Defines a list of characters for use with OUTPUTK and LOAD&RETURN
    -- instructions.
  | StringDirective Name String

    -- Defines a list of values for use with OUTPUTK and LOAD&RETURN
    -- instructions.
  | TableDirective Name [Constant]

    -- Forces the assembler to assemble all subsequent instructions starting at
    -- the address defined.
  | AddressDirective Address

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

-- A map from lables to addresses.
type LabelMap = Map Label Address

data ParserState = ParserState {
    -- The current program address.
    parserStateAddress :: Address

    -- A map from labels to addresses.
  , parserStateLabelMap :: LabelMap
  } deriving (Show)
