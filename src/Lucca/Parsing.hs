module Lucca.Parsing where

import Lucca.Parsing.Instruction
import Control.Monad
import Text.Parsec
import Text.Parsec.String
import Data.Vector as V
import Lucca.Instruction

parseProgram :: Parser (Vector Instruction)
parseProgram = V.fromList <$> program'
