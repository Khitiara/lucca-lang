module Lucca(module Lucca.Instruction, runMachine, MonadInteract, MachineError(..), DataType(..), Register(..)) where

import Lucca.Machine
import Lucca.Interpret
import Lucca.IO.Class
import Lucca.Instruction

import Control.Monad.Except
import Control.Monad.State

import qualified Data.Vector as V

setupMachine :: V.Vector Instruction -> Int -> RunningState
setupMachine instrs mainAddr = 
    let prog = Program $ V.fromList [Call $ mainAddr + 2, Ret] <> instrs
        stack = []
        genRegs = V.replicate 16 Nothing
        spr = SR 0 Nothing Nothing
    in RunningState (MachineState stack genRegs spr) prog

runMachine :: (Monad m, MonadInteract m) => V.Vector Instruction -> Int -> ExceptT MachineError m ()
runMachine instr ma = let state = setupMachine instr ma
                      in  void $ runStateT (runMachineT runProgram) state
