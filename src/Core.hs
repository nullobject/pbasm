module Core where

import Data.Map
import Data.Word

type Identifier = String

-- A 12-bit address value.
newtype AddressValue = AddressValue Word16 deriving (Eq, Show)

instance Num AddressValue where
  (AddressValue x) + (AddressValue y) = AddressValue (x + y)
  (AddressValue x) - (AddressValue y) = AddressValue (x - y)
  (AddressValue x) * (AddressValue y) = AddressValue (x * y)
  abs x                               = x
  signum 0                            = 0
  signum _                            = 1
  fromInteger i                       = AddressValue (fromInteger i)

-- An 8-bit constant value.
newtype DataValue = DataValue Word8 deriving (Eq, Show)

instance Num DataValue where
  (DataValue x) + (DataValue y) = DataValue (x + y)
  (DataValue x) - (DataValue y) = DataValue (x - y)
  (DataValue x) * (DataValue y) = DataValue (x * y)
  abs x                         = x
  signum 0                      = 0
  signum _                      = 1
  fromInteger i                 = DataValue (fromInteger i)

-- The PicoBlaze has 16 general purpose 8-bit registers.
data Register =
    Register0 | Register1 | Register2 | Register3
  | Register4 | Register5 | Register6 | Register7
  | Register8 | Register9 | RegisterA | RegisterB
  | RegisterC | RegisterD | RegisterE | RegisterF
  deriving (Enum, Eq, Show)

data Operand =
    AddressOperand  AddressValue
  | DataOperand     DataValue
  | LabelOperand    Identifier
  | RegisterOperand Register
  deriving (Eq, Show)

data Statement =
    -- Assigns a name to a constant value.
    ConstantDirective Identifier DataValue

    -- Defines a list of characters for use with OUTPUTK and LOAD&RETURN
    -- instructions.
  | StringDirective Identifier String

    -- Defines a list of values for use with OUTPUTK and LOAD&RETURN
    -- instructions.
  | TableDirective Identifier [DataValue]

    -- Forces the assembler to assemble all subsequent instructions starting at
    -- the address defined.
  | AddressDirective AddressValue

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
type ConstantMap = Map Identifier DataValue

-- A map from labels to address values.
type LabelMap = Map Identifier AddressValue
