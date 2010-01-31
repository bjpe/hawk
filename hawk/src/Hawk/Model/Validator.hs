{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}
-- --------------------------------------------------------------------------
{- |
   Module      :  $Header$
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Main.hs 57 2009-05-29 11:33:59Z inf6254 $

-}
-- --------------------------------------------------------------------------
module Hawk.Model.Validator 
  ( ValidationError
  , AttributeName
  , ValidationErrors
  , ValidatorT (ValidatorT)
  , Validatable (..)
  , execValidatorT
  , runValidatorT
  , addError
  , silent
  , check
  , validate
  , (>&>)
  , (>?>)
  , validateNull
  , validateNotNull
  , validateNothing
  , validateJust
  , validateNotEmpty
  , validateLength
  , validateFuture
  , validatePast
  , validateUniqueness
  ) where

import Control.Monad.Writer
import Data.Time
import Data.Maybe (isJust, isNothing, fromJust)
import Data.Convertible
import Hawk.Model.Criteria
import Hawk.Model.Types
import Hawk.Model.WithPrimaryKey
import Hawk.Model.CriteriaSelect
import Hawk.Model.Persistent
import Hawk.Model.MonadDB

type ValidationError  = String
type AttributeName    = String
type ValidationErrors = [(AttributeName, ValidationError)]

newtype ValidatorT m a = ValidatorT { unwrap :: WriterT ValidationErrors m a }
        deriving (Functor, Monad, MonadIO, MonadTrans, MonadWriter ValidationErrors)


execValidatorT :: Monad m => ValidatorT m a -> m ValidationErrors
execValidatorT = execWriterT . unwrap


runValidatorT :: Monad m => ValidatorT m a -> m (a, ValidationErrors)
runValidatorT = runWriterT . unwrap


addError :: MonadWriter ValidationErrors m => AttributeName -> ValidationError -> m ()
addError src err = tell [(src, err)]


silent :: MonadWriter ValidationErrors m => m a -> m a
silent = censor (const [])


check :: Monad m => Bool -> ValidationError -> AttributeName -> ValidatorT m Bool
check b err src = unless b (addError src err) >> return b


validate :: Monad m => (a -> Bool) -> ValidationError -> AttributeName -> a -> ValidatorT m Bool
validate p err src value = check (p value) err src


(>&>) :: Monad m =>
     (AttributeName -> a -> ValidatorT m Bool) ->
     (AttributeName -> a -> ValidatorT m Bool) ->
      AttributeName -> a -> ValidatorT m Bool
(f >&> g) name value = do
  res  <- f name value
  res2 <- g name value
  return $ res && res2


(>?>) :: Monad m =>
     (AttributeName -> a -> ValidatorT m Bool) ->
     (AttributeName -> a -> ValidatorT m Bool) ->
      AttributeName -> a -> ValidatorT m Bool
(f >?> g) name value = do
  res <- f name value
  if res
    then g name value
    else return False


validateNull :: (Monad m) => AttributeName -> [a] -> ValidatorT m Bool
validateNull = validate null "must be null"


validateNotNull :: (Monad m) => AttributeName -> [a] -> ValidatorT m Bool
validateNotNull = validate (not . null) "must not be null"


validateNothing :: Monad m => AttributeName -> Maybe a -> ValidatorT m Bool
validateNothing = validate isNothing "must be nothing"


validateJust :: Monad m => AttributeName -> Maybe a -> ValidatorT m Bool
validateJust = validate isJust "must be just"


validateNotEmpty :: Monad m => AttributeName -> Maybe [a] -> ValidatorT m Bool
validateNotEmpty name value = do
  res <- validateJust name value
  if res 
    then validateNotNull name $ fromJust value
    else return False


validateLength :: Monad m => Int -> Int -> AttributeName -> [a] -> ValidatorT m Bool
validateLength minLen maxLen name value = check (minLen <= len && len <= maxLen) err name
  where
    len = length value
    err = "length must be between " ++ show minLen ++ " and " ++ show maxLen


validateFuture :: MonadIO m => AttributeName -> UTCTime -> ValidatorT m Bool
validateFuture time src = do
  now <- liftIO getCurrentTime
  validate (> now) "must be in the future" time src


validatePast :: MonadIO m => AttributeName -> UTCTime -> ValidatorT m Bool
validatePast time src = do
  now <- liftIO getCurrentTime
  validate (< now) "must be in the past" time src


validateUniqueness :: (WithPrimaryKey p, MonadDB m, Convertible a SqlValue)
                   => [(ColumnName, SqlValue)]
                   -> (p -> a)
                   -> AttributeName
                   -> p
                   -> ValidatorT m Bool
validateUniqueness scope getter src p = do
  ids <- lift $ querySelect query
  check (null ids || ids == [[pkValue]]) "must be unique" src
  where
    query                = setProjection [colP pkCol]
                         $ modifyCriteria (setRestriction rest)
                         $ setTables [tableName p]
                           newSelect
    rest                 = allExpr 
                         $ map toRestriction 
                         $ (src, toSql $ getter p) : scope
    toRestriction (c, v) = eqExpr (col c) (sqlVal v)
    pkCol                = pkColumn p
    pkValue              = toSql $ primaryKey p


class Validatable v where
  validator :: MonadDB m => v -> ValidatorT m ()
  validator _ = return ()

