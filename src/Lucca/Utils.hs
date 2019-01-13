{-# LANGUAGE RankNTypes #-}

module Lucca.Utils where

import Data.Maybe
import Data.Functor
import Control.Lens
import Data.Vector as V
import Control.Monad
import Control.Monad.Except

idx :: Int -> Lens' (Vector a) (Maybe a) -- If set to Nothing, does nothing
idx i f v = f (v V.!? i) <&> (maybe v (\a -> v V.// [(i, a)]))

idx' :: Int -> Lens' (Vector (Maybe a)) (Maybe a) -- Out of bounds yields Nothing on get
idx' i f v = f (join (v V.!? i)) <&> \a -> v V.// [(i, a)]

liftMaybe :: MonadError e m => e -> Maybe a -> m a
liftMaybe _ (Just x) = return x
liftMaybe err Nothing = throwError err

liftMaybe' :: MonadError e m => e -> m (Maybe a) -> m a
liftMaybe' e = (>>= liftMaybe e)
