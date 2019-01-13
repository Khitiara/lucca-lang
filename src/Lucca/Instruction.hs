module Lucca.Instruction where

import Lucca.MachineTypes
import Data.Word

data Instruction = Load Register -- Load given register to head
                 | Store Register -- Store the head in given register
                 | Push DataType -- Push a literal
                 | Pop  -- Pop value
                 | Ret  -- Return the top of the stack to last stack frame
                 | Bne Int  -- Branch if not equal (no stack frame)
                 | Call Int -- Call (adds stack frame)
                 | SysInt -- System call
                 | Addi -- Add int
                 | Muli -- Mul int
                 | Subi -- Sub int
                 | Divi -- Div int
                 | Addf -- Add float
                 | Subf -- Sub float
                 | Divf -- Div float
                 | Mulf -- Mul float
                 | Lshi -- Left shift
                 | Rshi -- Right shift
                 | Cmp  -- Compare
                 | Dup  -- Duplicate the head for convenience
                 | Swap
