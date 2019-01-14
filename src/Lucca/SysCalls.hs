{-# LANGUAGE TemplateHaskell, RankNTypes, FlexibleContexts #-}

module Lucca.SysCalls(syscalls) where
import Control.Lens
import Lucca.Machine
import Control.Monad.Writer
import Lucca.Utils
import qualified Data.Map as Map

printStr :: (MonadWriter String m) => MachineT m ()
printStr = tell =<< liftMaybe' (OtherError "Not a string!") ((^?_S) <$> pop)

strcat :: (Monad m) => MachineT m ()
strcat = do
    a <- liftMaybe' (OtherError "Not a string!") ((^?_S) <$> pop)
    b <- liftMaybe' (OtherError "Not a string!") ((^?_S) <$> pop)
    push $ S $ a ++ b

tostr :: (Monad m) => MachineT m ()
tostr = push =<< S <$> process <$> pop
    where process (S str) = str
          process (N n) = show n
          process (O o) = show o
          process (F f) = show f

syscalls :: (Monad m, MonadWriter String m) => Map.Map String (MachineT m ())
syscalls = Map.fromList [("print", printStr), ("strcat", strcat), ("tostr", tostr)]
