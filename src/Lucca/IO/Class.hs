{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving #-}

module Lucca.IO.Class where

import System.IO
import Control.Monad.Writer
import Control.Monad
import Data.String

class MonadInteract m where
    printL :: String -> m ()
    gets :: m String
    logL :: String -> m ()

instance MonadInteract IO where
    printL = putStrLn
    gets = getLine
    logL s = return ()

instance (MonadTrans m1, MonadInteract m2, Monad m2) => MonadInteract (m1 m2) where
    printL = lift . printL
    gets = lift gets
    logL = lift . logL
