module Core where

import Data.Word

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

data Instruction =
    -- LOAD sX, sY
    -- LOAD sX, kk
    -- Loads a value into the first operand from the second operand.
    LoadInstruction Operand Operand

    -- AND sX, sY
    -- AND sX, kk
    -- Performs a bit-wise logical AND operation on the first and second operands.
  | AndInstruction Operand Operand

    -- OR sX, sY
    -- OR sX, kk
    -- Performs a bit-wise logical OR operation on the first and second operands.
  | OrInstruction Operand Operand

    -- XOR sX, sY
    -- XOR sX, kk
    -- Performs a bit-wise logical XOR operation on the first and second operands.
  | XorInstruction Operand Operand

    -- JUMP aaa
    -- Jumps to an address.
  | JumpInstruction Operand

    -- CALL aaa
    -- Calls a subroutine at an address.
  | CallInstruction Operand

    -- RETURN
    -- Completes a subroutine.
  | ReturnInstruction
  deriving (Eq, Show)
