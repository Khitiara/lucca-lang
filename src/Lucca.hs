module Lucca(module Lucca.Instruction, module Lucca, MonadInteract, MachineError(..), DataType(..), Register(..)) where

import Lucca.Machine
import Lucca.Interpret
import Lucca.IO.Class
import Lucca.Instruction
import Lucca.Parsing

import Control.Monad.Except
import Control.Monad.State
import Text.Parsec (ParseError)
import Text.Parsec.String (parseFromFile)

import qualified Data.Vector as V

setupMachine :: V.Vector Instruction -> Int -> RunningState
setupMachine instrs mainAddr = 
    let prog = Program $ V.fromList [Call mainAddr 0, Ret] <> instrs
        stack = []
        genRegs = V.replicate 4 Nothing
        spr = SR 0 Nothing Nothing
    in RunningState (MachineState stack genRegs spr) prog

runMachine :: (Monad m, MonadInteract m) => V.Vector Instruction -> Int -> ExceptT MachineError m ()
runMachine instr ma = let state = setupMachine instr ma
                      in  void $ runStateT (runMachineT runProgram) state

data Err = P ParseError | M MachineError deriving Show

runFile path = liftIO (parseFromFile parseProgram path) >>= runFile'
    where runFile' :: Either ParseError (V.Vector Instruction) -> ExceptT Err IO ()
          runFile' e = do instrs <- withExceptT P $ liftEither e
                          withExceptT M $ runMachine instrs 0
