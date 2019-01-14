module Main where

import Lucca
import qualified Data.Vector as V
import Control.Monad.Except

main :: IO ()
main = putStrLn =<< show <$> runExceptT (runMachine testProgram2 0)

testProgram = V.fromList [Push $ N 5, Push $ N 3, Addi, Store $ GEN 0, SysInt "tostr", Move RET $ GEN 0
                         , Push $ S "\n", Store $ GEN 1, SysInt "strcat", Move RET $ GEN 0, SysInt "print"
                         , Push $ N 0, Ret]

testProgram2 = V.fromList [Push $ S "Hello ", Store $ GEN 0, Push $ S "World!\n", Store $ GEN 1, SysInt "strcat"
                          , Move RET $ GEN 0, SysInt "print", Push $ N 0, Ret]
