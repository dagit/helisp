{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fglasgow-exts #-}
module MonadUnique
        ( UniqueT,
          Unique,
          MonadUnique,
          fresh,
          evalUniqueT,
          evalUnique )
    where

import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Error

newtype UniqueT m a = UniqueT (StateT Integer m a)
    deriving (Functor, Monad, MonadTrans, MonadIO, MonadError (m a))

newtype Unique a = Unique (UniqueT Identity a)
    deriving (Functor, Monad, MonadUnique)

class Monad m => MonadUnique m where
    fresh :: m Integer

instance (Monad m) => MonadUnique (UniqueT m) where
    fresh = UniqueT $ do
                n <- get
                put (succ n)
                return n

evalUniqueT :: (Monad m) => UniqueT m a -> m a
evalUniqueT (UniqueT s) = evalStateT s 0
evalUnique :: Unique a -> a
evalUnique (Unique s) = runIdentity (evalUniqueT s)