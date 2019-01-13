{-# LANGUAGE TemplateHaskell, RankNTypes, FlexibleContexts #-}

module Lucca.Machine 
( module Lucca.Machine
, throwError
, catchError
, module Lucca.Instruction
, module Lucca.MachineTypes) where

import Lucca.MachineTypes
import Lucca.Instruction
import Control.Monad.Except
import Control.Monad.State
import Control.Lens
import Lucca.Utils
import Data.Vector
import Lucca.Arithmetic
import Data.Bits

newtype Program = Program { instr :: Vector Instruction }
makePrisms ''Program
data RunningState = RunningState { _machine :: MachineState, _program :: Program }
makeLenses ''RunningState

data MachineError = ArithmeticError | NoInstruction | OutOfBounds | EmptyRegister | IllegalAccess | OtherError String
type MachineT m = StateT RunningState (ExceptT MachineError m)
type Machine = MachineT Identity

fetch :: (Monad m) => MachineT m Instruction
fetch = use (machine . pcReg) >>= (\pc -> liftMaybe' NoInstruction $ use $ program . _Program . idx pc)

incPc :: (Monad m) => MachineT m ()
incPc = (machine . pcReg) += 1

push :: (Monad m) => DataType -> MachineT m ()
push d = (machine . stack) %= ((Data d):)

pop :: (Monad m) => MachineT m DataType
pop = use (machine . stack) >>= processStack
    where processStack ((Data d):xs) = machine . stack .= xs >> return d
          processStack _ = throwError OutOfBounds
          
load :: (Monad m) => Register -> MachineT m ()
load r = liftMaybe' EmptyRegister (use $ machine . register r) >>= push

store :: (Monad m) => Register -> MachineT m ()
store (GEN i) = pop >>= ((machine . genReg i) ?=)
store _ = throwError IllegalAccess

arithmeticInstruction :: (Monad m) => (DataType -> DataType -> Maybe DataType) -> MachineT m ()
arithmeticInstruction f = do
    a <- pop
    b <- pop
    out <- liftMaybe ArithmeticError $ f a b
    push out
doCompare :: DataType -> DataType -> Maybe Ordering
doCompare (N a) (N b) = Just $ compare a b
doCompare (N a) (F b) = Just $ compare (fromIntegral a) b
doCompare (F a) (N b) = Just $ compare a (fromIntegral b)
doCompare (F a) (F b) = Just $ compare a b
doCompare (S a) (S b) = Just $ compare a b
doCompare (O a) (O b) = Just $ compare a b


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
    
