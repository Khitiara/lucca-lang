{-# LANGUAGE TemplateHaskell, RankNTypes, FlexibleContexts, ExistentialQuantification, GeneralizedNewtypeDeriving #-}

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

data Program = Program { _instr :: Vector Instruction }
makeLenses ''Program
data RunningState = RunningState { _machine :: MachineState, _program :: Program }
makeLenses ''RunningState

data MachineError = ArithmeticError | NoInstruction Int | OutOfStack | EmptyRegister 
                  | IllegalAccess | OtherError String | ReturnFromMain | UnknownSI String deriving Show
newtype MachineT m a = MachineT { runMachineT :: StateT RunningState (ExceptT MachineError m) a }
    deriving (Functor, Applicative, Monad, MonadState RunningState, MonadError MachineError)
   
instance MonadTrans MachineT where
    lift = MachineT . lift . lift
   
type Machine = MachineT Identity

fetch :: (Monad m) => MachineT m Instruction
fetch = use (machine . pcReg) >>= (\pc -> liftMaybe' (NoInstruction pc) $ use $ program . instr . idx pc)

incPc :: (Monad m) => MachineT m ()
incPc = (machine . pcReg) += 1

push' :: (Monad m) => StackEntry -> MachineT m ()
push' e = (machine . stack) %= (e:)

push :: (Monad m) => DataType -> MachineT m ()
push = push' . Data

pop :: (Monad m) => MachineT m DataType
pop = use (machine . stack) >>= processStack
    where processStack ((Data d):xs) = machine . stack .= xs >> return d
          processStack _ = throwError OutOfStack
          
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
