module Main where

import Lucca
import qualified Data.Vector as V
import Control.Monad.Except

main :: IO ()
main = void $ runExceptT (runMachine testProgram 0)

testProgram = V.fromList [Push $ N 5, Push $ N 3, Addi, Store $ GEN 0, SysInt "tostr", Load RET, Store $ GEN 0, SysInt "print", Push $ N 0, Ret]
