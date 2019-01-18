module Main where

import Lucca
import qualified Data.Vector as V
import Control.Monad.Except
import Control.Arrow (left)

main :: IO ()
main = putStrLn =<< show <$> runExceptT (runFile "data/test1.lca")

testProgram = V.fromList [Push $ N 5, Push $ N 3, Addi, SysInt "tostr", Push $ S "\n", Load RET, SysInt "strcat"
                         , Load RET, SysInt "print", Push $ S "World!\n", Push $ S "Hello ", SysInt "strcat"
                         , Load RET, SysInt "print", Push $ N 0, Ret]

testProgram2 = V.fromList [Push $ S "Hello ", Store $ GEN 0, Push $ S "World!\n", Store $ GEN 1, SysInt "strcat"
                          , Move RET $ GEN 0, SysInt "print", Push $ N 0, Ret]
