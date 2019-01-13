module Lucca.Arithmetic where

import Lucca.MachineTypes
import Control.Lens

addi :: DataType -> DataType -> Maybe DataType
addi (N a) (N b) = Just $ N $ a + b
addi _ _ = Nothing

subi :: DataType -> DataType -> Maybe DataType
subi (N a) (N b) = Just $ N $ a - b
subi _ _ = Nothing

muli :: DataType -> DataType -> Maybe DataType
muli (N a) (N b) = Just $ N $ a * b
muli _ _ = Nothing

divi :: DataType -> DataType -> Maybe (DataType, DataType)
divi (N a) (N b) = Just $ both %~ N $ divMod a b
divi _ _ = Nothing

addf :: DataType -> DataType -> Maybe DataType
addf (F a) (F b) = Just $ F $ a + b
addf _ _ = Nothing

subf :: DataType -> DataType -> Maybe DataType
subf (F a) (F b) = Just $ F $ a - b
subf _ _ = Nothing

mulf :: DataType -> DataType -> Maybe DataType
mulf (F a) (F b) = Just $ F $ a * b
mulf _ _ = Nothing

divf :: DataType -> DataType -> Maybe DataType
divf (F a) (F b) = Just $ F $ a / b
divf _ _ = Nothing
