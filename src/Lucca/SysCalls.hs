{-# LANGUAGE TemplateHaskell, RankNTypes, FlexibleContexts #-}

module Lucca.SysCalls(syscalls) where
import Control.Lens
import Lucca.Machine
import Control.Monad.Writer
import Lucca.Utils
import qualified Data.Map as Map
import Lucca.IO.Class

printStr :: (Monad m, MonadInteract m) => MachineT m ()
printStr = load (GEN 0) >> (printL =<< liftMaybe' (OtherError "Not a string!") ((^?_S) <$> pop))

strcat :: (Monad m) => MachineT m ()
strcat = do
    load $ GEN 1
    load $ GEN 0
    a <- liftMaybe' (OtherError "Not a string!") $ (^?_S) <$> pop
    b <- liftMaybe' (OtherError "Not a string!") $ (^?_S) <$> pop
    (machine . retReg) ?= S (a ++ b)
    

tostr :: (Monad m) => MachineT m ()
tostr =  ((machine . retReg) ?=) =<< S <$> process <$> liftMaybe' (OtherError "Not a string!") (use $ machine . genReg 0)
    where process (S str) = str
          process (N n) = show n
          process (O o) = show o
          process (F f) = show f

getsLine :: (Monad m, MonadInteract m) => MachineT m ()
getsLine = ((machine . retReg) ?=) =<< S <$> gets

atoi :: (Monad m) => MachineT m ()
atoi = do
    load $ GEN 0
    a <- liftMaybe' (OtherError "Not a string!") $ (^?_S) <$> pop
    (machine . retReg) ?= N (read a)
    
syscalls :: (Monad m, MonadInteract m) => Map.Map String (MachineT m ())
syscalls = Map.fromList [("print", printStr), ("strcat", strcat), ("tostr", tostr), ("gets", getsLine), ("atoi", atoi)]
