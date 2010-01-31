-- --------------------------------------------------------------------------
{- |
   Module      :  $Header$
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Main.hs 57 2009-05-29 11:33:59Z inf6254 $

   Exceptions for the model
-}
-- --------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification #-}
module Hawk.Model.Exception where

import Data.Typeable
import Control.Exception 
  ( Exception(..)
  , SomeException
  )

-- | Exception type for all 'Exception's which may occur in the model
data SomeModelException = forall a . (Exception a) => SomeModelException a
    deriving Typeable

instance Show SomeModelException where 
  show (SomeModelException e) = show e

instance Exception SomeModelException

-- | Convert an 'Exception' to 'SomeModelException'
modelToException :: Exception e => e -> SomeException
modelToException = toException . SomeModelException

-- | Try to extract a 'SomeModelException'
modelFromException :: Exception e => SomeException -> (Maybe e)
modelFromException x = do
  SomeModelException e <- fromException x
  cast e

data RecordNotFound = RecordNotFound String
    deriving (Typeable, Show)

instance Exception RecordNotFound where
  toException = modelToException
  fromException = modelFromException

data SqlException = SqlException String
    deriving (Typeable, Show)

instance Exception SqlException where
  toException = modelToException
  fromException = modelFromException

data UnmarshalException = UnmarshalException String
    deriving (Typeable, Show)

instance Exception UnmarshalException where
  toException = modelToException
  fromException = modelFromException

