{-# LANGUAGE TemplateHaskell, RankNTypes, FlexibleContexts #-}

module Lucca.Calling where

import Lucca.Machine
import Control.Lens
import Data.Ord

popToFrame :: (Monad m) => MachineT m Int
popToFrame = use (machine . stack) >>= processStack
    where processStack :: (Monad m) => [StackEntry] -> MachineT m Int
          processStack ((Data _):xs) = machine . stack .= xs >> popToFrame
          processStack ((Frame r):xs) = machine . stack .= xs >> return r
          processStack [] = throwError NoInstruction

doReturn :: (Monad m) => MachineT m ()
doReturn = do
    rVal <- pop
    machine . retReg ?= rVal
    addr <- popToFrame
    machine . pcReg .= addr - 1

blt :: (Monad m) => Int -> MachineT m ()
blt addr = do
    cval <- use $ machine . cmpReg
    when (cval == LT) $ machine . pcReg .= addr - 1

doCall :: (Monad m) => Int -> MachineT m ()
doCall addr = do
    inst <- use $ machine . pcReg
    push' $ Frame $ inst + 1
    machine . pcReg .= addr - 1
