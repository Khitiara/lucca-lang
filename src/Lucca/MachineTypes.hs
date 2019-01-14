{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module Lucca.MachineTypes where

import Data.Word
import Data.Vector as V
import Data.Ord

import Lucca.Utils

import Control.Lens
import Control.Monad

data DataType = N Int | F Double | S String | O Ordering deriving Show
makePrisms ''DataType

data StackEntry = Data DataType | Frame { _retAdd :: Int }

data SpecialRegisters = SR { _pc :: Int, _cmp :: Maybe Ordering, _ret :: Maybe DataType }
makeLenses ''SpecialRegisters

data MachineState = MachineState { _stack :: [StackEntry], _genRegs :: Vector (Maybe DataType)
                                 , _spRegs :: SpecialRegisters }
makeLenses ''MachineState

data Register = GEN Int | PC | CMP | RET

pcReg :: Lens' MachineState Int -- Program counter register
pcReg = spRegs . pc
cmpReg :: Lens' MachineState (Maybe Ordering) -- Comparison result register
cmpReg = spRegs . cmp
retReg :: Lens' MachineState (Maybe DataType) -- Return register
retReg = spRegs . ret
genReg :: Int -> Lens' MachineState (Maybe DataType) -- General register
genReg i = genRegs . idx' i

-- Special purpose registers should never be written manually
register :: Register -> Getter MachineState (Maybe DataType)
register (GEN i) = genReg i
register PC = pcReg . to N . to Just
register CMP = cmpReg . to (fmap O)
