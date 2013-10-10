module Core where

import Data.Map (Map)
import Data.Word (Word32)

type Identifier = String

-- An 8-bit data value.
newtype Value = Value Word32 deriving (Eq, Show)

instance Num Value where
  (Value x) + (Value y) = Value (x + y)
  (Value x) - (Value y) = Value (x - y)
  (Value x) * (Value y) = Value (x * y)
  abs x                         = x
  signum 0                      = 0
  signum _                      = 1
  fromInteger i                 = Value (fromInteger i)

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

data Operand =
    ValueOperand      Value
  | IdentifierOperand Identifier
  | RegisterOperand   Register
  | ConditionOperand  Condition
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
