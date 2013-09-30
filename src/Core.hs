module Core where

import Data.Word

type Name = String

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
  | RegisterOperand Register
  deriving (Eq, Show)

data Directives =
    -- Assigns a name to a constant value. For example:
    ConstantDirective Name Constant

    -- Defines a list of characters for use with OUTPUTK and LOAD&RETURN
    -- instructions.
  | StringDirective Name String

    -- Defines a list of values for use with OUTPUTK and LOAD&RETURN
    -- instructions.
  | TableDirective Name [Constant]

    -- Inserts the contents of another file into the program immediately
    -- following the directive.
  | IncludeDirective FilePath

data Instruction =
    -- Loads a value into the first operand from the second operand.
    -- LOAD sX, sY
    -- LOAD sX, kk
    LoadInstruction Operand Operand

    -- Performs a bit-wise logical AND operation on the first and second operands.
    -- AND sX, sY
    -- AND sX, kk
  | AndInstruction Operand Operand

    -- Performs a bit-wise logical OR operation on the first and second operands.
    -- OR sX, sY
    -- OR sX, kk
  | OrInstruction Operand Operand

    -- Performs a bit-wise logical XOR operation on the first and second operands.
    -- XOR sX, sY
    -- XOR sX, kk
  | XorInstruction Operand Operand

    -- Performs a shift left operation on the operand.
    -- SL0 sX
  | ShiftLeft0Instruction Operand

    -- Performs a shift left operation on the operand.
    -- SL1 sX
  | ShiftLeft1Instruction Operand

    -- Jumps to an address.
    -- JUMP aaa
  | JumpInstruction Operand

    -- Calls a subroutine at an address.
    -- CALL aaa
  | CallInstruction Operand

    -- Completes a subroutine.
    -- RETURN
  | ReturnInstruction
  deriving (Eq, Show)
