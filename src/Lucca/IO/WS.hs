{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, StandaloneDeriving, MultiParamTypeClasses #-}

module Lucca.IO.WS where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans
import Data.String
import Data.String.ToString
import Lucca.IO.Class
import Control.Monad.Fail

newtype WST w s m a = WST { runWST :: WriterT w (StateT s m) a } 
    deriving (Functor, Applicative, Monad, MonadWriter w, MonadState s)
    
deriving instance (Monoid w, MonadFail m) => MonadFail (WST w s m)

instance (IsString s1, Monoid s1, ToString s2, Monad m, MonadFail m) => MonadInteract (WST s1 [s2] m) where
    printL = tell . fromString
    gets = do
        x:xs <- get
        put xs
        return $ toString x
    logL = tell . fromString
