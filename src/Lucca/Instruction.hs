module Lucca.Instruction where

import Lucca.MachineTypes
import Data.Word

-- An instruction
data Instruction = Nop
                 -- Stack manipulation
                 | Load Register -- Load given register to head
                 | Store Register -- Store the head in given register
                 | Push DataType -- Push a literal
                 | Move Register Register
                 | Pop  -- Pop value
                 | Dup  -- Duplicate the head for convenience
                 | Swap -- Swaps the top two elements of the stack
                 -- Call semantics
                 | Ret  -- Return the top of the stack to last stack frame
                 | Blt Int  -- Branch if less than (no stack frame)
                 | Call Int Int -- Call (adds stack frame)
                 | SysInt String -- System call
                 -- Integer arithmetic
                 | Addi -- Add int
                 | Muli -- Mul int
                 | Subi -- Sub int
                 | Divi -- Div int
                 -- Floating arithmetic
                 | Addf -- Add float
                 | Subf -- Sub float
                 | Divf -- Div float
                 | Mulf -- Mul float
                 -- Misc arithmetic
                 | Lshi -- Left shift
                 | Rshi -- Right shift
                 | Cmp  -- Compare
                 deriving Show
