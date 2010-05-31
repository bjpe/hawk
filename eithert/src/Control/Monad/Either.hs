-- --------------------------------------------------------------------------
{- |
   Module      :  Control.Monad.Either
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  NONE

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Main.hs 57 2009-05-29 11:33:59Z inf6254 $

   Monad transformer for Either, inspired by Control.Monad.ErrorT but without
   the Error restriction of the Left value.
-}
-- --------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Control.Monad.Either
	( EitherT (..)
  , returnLeft
  , mapEitherT
  , module Control.Monad
  , module Control.Monad.Fix
  , module Control.Monad.Trans
	) where

import Control.Monad
import Control.Monad.CatchIO
import Control.Monad.Fix
import Control.Monad.RWS.Class
import Control.Monad.Trans
import Prelude hiding (catch)

-- --------------------------------------------------------------------------
-- instances for Either
-- --------------------------------------------------------------------------
{-
instance Monad (Either e) where
  return = Right
  Right m >>= k = k m
  Left e  >>= _ = Left e

instance Applicative (Either e) where
	pure = Right
	a <*> b = do x <- a; y <- b; return (x y)

instance MonadFix (Either e) where
	mfix f = let
		a = f $ case a of
			Right r -> r
			_ -> error "empty mfix argument"
		in a
-}
-- --------------------------------------------------------------------------
-- Definitions for EitherT
-- --------------------------------------------------------------------------

newtype EitherT a m b = EitherT { runEitherT :: m (Either a b) }

returnLeft :: Monad m => a -> EitherT a m b
returnLeft = EitherT . return . Left

mapEitherT :: (m (Either a b) -> n (Either a' b'))
           -> EitherT a m b
           -> EitherT a' n b'
mapEitherT f = EitherT . f . runEitherT

-- --------------------------------------------------------------------------
-- instances for EitherT
-- --------------------------------------------------------------------------
instance Functor f => Functor (EitherT a f) where
	fmap f = EitherT . fmap (fmap f) . runEitherT

instance Monad m => Monad (EitherT a m) where
  return  = EitherT . return . Right
  m >>= k = EitherT $ do
    a <- runEitherT m
    case a of
    	Left  l -> return (Left l)
    	Right r -> runEitherT (k r)

instance MonadFix m => MonadFix (EitherT a m) where
	mfix f = EitherT $ mfix $ \a -> runEitherT $ f $ case a of
        	Right r -> r
        	_       -> error "empty mfix argument"

-- --------------------------------------------------------------------------
-- instances for mtl transformers
-- --------------------------------------------------------------------------
instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right

instance MonadIO m => MonadIO (EitherT e m) where
  liftIO = lift . liftIO

instance MonadRWS r w s m => MonadRWS r w s (EitherT e m)

instance MonadReader r m => MonadReader r (EitherT e m) where
  ask     = lift ask
  local f = EitherT . local f . runEitherT

instance MonadState s m => MonadState s (EitherT e m) where
  get = lift get
  put = lift . put

instance MonadWriter w m => MonadWriter w (EitherT e m) where
  tell     = lift . tell
  listen m = EitherT $ do
    (a, w) <- listen $ runEitherT m
    case a of
      Left  l -> return $ Left l
      Right r -> return $ Right (r, w)
  pass m   = EitherT $ pass $ do
    a <- runEitherT m
    case a of
      Left  l      -> return (Left  l, id)
      Right (r, f) -> return (Right r, f)

-- --------------------------------------------------------------------------
-- instance for MonadCatchIO
-- --------------------------------------------------------------------------
instance MonadCatchIO m => MonadCatchIO (EitherT e m) where
  m `catch` f = mapEitherT (`catch` (runEitherT . f)) m
  block       = mapEitherT block
  unblock     = mapEitherT unblock

