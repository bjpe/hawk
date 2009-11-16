-- --------------------------------------------------------------------------
{- |
   Module      :  Hawk.Model.Persistence.MonadDB
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  NONE

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Main.hs 57 2009-05-29 11:33:59Z inf6254 $

-}
-- --------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances #-}
module Hawk.Model.Updater
  ( Params
  , UpdaterT
  , runUpdaterT
  , updateByParams
  , updateAndValidate
  , subParam
  , concatNameWith
  , Updateable (..)
  ) where

import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.Map as M (Map, lookup, member)
import Data.Maybe (listToMaybe)
import Data.Time.Calendar
import Hawk.Model.Validator
import Hawk.Model.MonadDB

type Params = M.Map AttributeName String

newtype UpdaterT m a = UpdaterT { unwrap :: ReaderT Params (ValidatorT m) a }
        deriving (Functor, Monad, MonadIO, MonadWriter ValidationErrors, MonadReader Params)

instance MonadTrans UpdaterT where
  lift = UpdaterT . lift . lift


runUpdaterT :: Monad m => UpdaterT m a -> Params -> ValidatorT m a
runUpdaterT = runReaderT . unwrap


updateByParams :: (MonadDB m, Updateable u) => u -> String -> Params -> m (u, ValidationErrors)
updateByParams updateable name = runValidatorT . runUpdaterT (updater updateable name)


updateAndValidate :: (MonadDB m, Updateable u, Validatable u) => u -> String -> Params -> m (u, ValidationErrors)
updateAndValidate updateable name params = runValidatorT $ do
  u' <- runUpdaterT (updater updateable name) params
  validator u'
  return u'


subParam :: String -> String -> String
subParam = concatNameWith "."


concatNameWith :: String -> String -> String -> String
concatNameWith _   xs [] = xs
concatNameWith _   [] ys = ys
concatNameWith sep xs ys = xs ++ sep ++ ys

    
class Updateable u where
  updater :: MonadDB m => u -> String -> UpdaterT m u
  updater u _ = return u


instance Updateable Bool where
  updater value name = primitiveUpdate value name maybeRead $ 
                      "The attribute '" ++ name ++"' must be a Bool"


instance Updateable String where
  updater value name = primitiveUpdate value name Just $ 
                      "The attribute '" ++ name ++"' must be a string"


instance Updateable Int where
  updater value name = primitiveUpdate value name maybeRead $ 
                      "The attribute '" ++ name ++ "' must be an int"


instance Updateable Integer where
  updater value name = primitiveUpdate value name maybeRead $ 
                      "The attribute '" ++ name ++ "' must be an integer"

instance Updateable Double where
  updater value name = primitiveUpdate value name maybeRead $ 
                      "The attribute '" ++ name ++ "' must be a double"                


instance Updateable Day where
  updater value name = do
    year  <- updater dy $ subParam name "year"
    month <- updater dd $ subParam name "month"
    day   <- updater dm $ subParam name "day"
    return $ fromGregorian year month day
    where (dy, dm, dd) = toGregorian value


instance Updateable u => Updateable (Maybe u) where
  updater value name = do
    params <- ask
    if name `M.member` params
      then do
           (value', errs) <- listen $ updater undefined name
           if null errs
              then return $ Just value'
              else return value
      else return value


primitiveUpdate :: Monad m => a -> String -> (String -> Maybe a) -> String -> UpdaterT m a
primitiveUpdate value name parser err = do
  params <- ask
  case M.lookup name params of 
    Nothing    -> return value
    Just param -> case parser param of
      Nothing  -> addError name err >> return value
      Just a   -> return a

maybeRead :: Read a => String -> Maybe a
maybeRead = listToMaybe . map fst . filter (null . snd) . reads
