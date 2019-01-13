module Lucca.Interpret where

import Lucca.Machine
import Lucca.Calling
import Lucca.Arithmetic
import Control.Lens
import Lucca.Utils
import Data.Bits
import Data.Functor

handleInstruction :: (Monad m) => Instruction -> MachineT m ()
handleInstruction (Load reg) = load reg
handleInstruction (Store reg) = store reg
handleInstruction (Push d) = push d
handleInstruction Pop = void pop
handleInstruction Dup = do
    a <- pop
    push a
    push a
handleInstruction Swap = do
    a <- pop
    b <- pop
    push a
    push b
    
handleInstruction Addi = arithmeticInstruction addi
handleInstruction Subi = arithmeticInstruction subi
handleInstruction Muli = arithmeticInstruction muli
handleInstruction Divi = do
    a <- pop
    b <- pop
    (q, r) <- liftMaybe ArithmeticError $ divi a b
    push r
    push q
handleInstruction Addf = arithmeticInstruction addf
handleInstruction Subf = arithmeticInstruction subf
handleInstruction Mulf = arithmeticInstruction mulf
handleInstruction Divf = arithmeticInstruction divf

handleInstruction Lshi = do
    a <- pop
    o <- liftMaybe ArithmeticError $ case a of
        N a' -> Just $ N $ shiftL a' 1
        _ -> Nothing
    push o
handleInstruction Rshi = do
    a <- pop
    o <- liftMaybe ArithmeticError $ case a of
        N a' -> Just $ N $ shiftR a' 1
        _ -> Nothing
    push o

handleInstruction Cmp = do
    a <- pop
    b <- pop
    c <- liftMaybe ArithmeticError $ doCompare a b
    machine . cmpReg ?= c
    
handleInstruction Ret = doReturn
handleInstruction (Blt a) = blt a
handleInstruction (Call a) = doCall a
