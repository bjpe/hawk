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

{-# LANGUAGE TypeFamilies, FlexibleContexts, MultiParamTypeClasses #-}

module Hawk.Model.WithForeignKey
    ( ForeignKeyRelationship (..)
    , getMaybeReferenced
    , getOneReferenced
    , setReferenced
    , removeReferenced
    , getReferencings
    , getMaybeReferencing
    , getOneReferencing
    , addReferencing
    , removeReferencing
    , replaceReferencing
    ) where

import Hawk.Model.Criteria
import Hawk.Model.Exception
import Hawk.Model.MonadDB
import Hawk.Model.Persistent
import Hawk.Model.WithPrimaryKey
import Hawk.Model.Types
import Hawk.Model.Util

import Control.Monad (liftM)

import Control.Exception (throw)



-- | Typeclass for ForeignKey relationships
{-
class (Persistent self, WithPrimaryKey other) =>
      WithForeignKey self relationshipName other |
      relationshipName -> self other where
-}
class (Persistent (Referencing r), WithPrimaryKey (Referenced r)) =>
      ForeignKeyRelationship r where
  type Referencing r :: *
  type Referenced  r :: *

  relationshipName :: r -> String

  -- | Get the 'ColumnName' containing the 'ForeignKey'
  fkColumn      :: r -> ColumnName

  -- | Get the value of the 'ForeignKey'
  foreignKey    :: r -> Referencing r -> ForeignKey

  -- | Set the value of the 'ForeignKey'
  setForeignKey :: r -> Referencing r -> ForeignKey -> Referencing r

  
-- --------------------------------------------------------------------------
-- Functions derived from WithForeignKey
-- --------------------------------------------------------------------------
getMaybeReferenced :: (MonadDB m, ForeignKeyRelationship r)
                   => r
                   -> Referencing r
                   -> m (Maybe (Referenced r))
getMaybeReferenced rel referencing
  = case foreignKey rel referencing of
      Nothing  -> return Nothing
      Just key -> findMaybeByPK key


getOneReferenced :: (MonadDB m, ForeignKeyRelationship r)
              => r
              -> Referencing r
              -> m (Referenced r)
getOneReferenced rel referencing
  = case foreignKey rel referencing of
      Nothing  -> throw $ RecordNotFound msg
      Just key -> findOneByPK key
  where msg =  "Persistent '" ++ persistentType referencing
            ++ "' has no foreign key for relationship '"
            ++ relationshipName rel ++ "'"


setReferenced :: (MonadDB m, ForeignKeyRelationship r)
              => r 
              -> Referencing r
              -> Maybe (Referenced r)
              -> m (Referencing r)
setReferenced rel referencing
  = return . setForeignKey rel referencing . fmap primaryKey


removeReferenced :: (MonadDB m, ForeignKeyRelationship r)
                 => r 
                 -> Referencing r
                 -> Referenced r
                 -> m (Referencing r)
removeReferenced rel referencing referenced
  = if Just (primaryKey referenced) == foreignKey rel referencing
       then setReferenced rel referencing Nothing
       else return referencing


getReferencings :: (MonadDB m, ForeignKeyRelationship r)
                => r
                -> Referenced r
                -> m [Referencing r]
getReferencings rel referenced
  = selectPersistents $ restrictionCriteria restr
  where restr = eqExpr (col $ fkColumn rel) (val $ primaryKey referenced)


getMaybeReferencing :: (MonadDB m, ForeignKeyRelationship r)
                    => r
                    -> Referenced r
                    -> m (Maybe (Referencing r))
getMaybeReferencing rel = liftM safeHead . getReferencings rel


getOneReferencing :: (MonadDB m, ForeignKeyRelationship r)
                  => r
                  -> Referenced r
                  -> m (Referencing r)
getOneReferencing rel parent
  = liftM (unsafeHeadMsg msg) $ getReferencings rel parent
  where msg =  "Persistent '" ++ persistentType parent
            ++ "' with primary key '" ++ show (primaryKey parent)
            ++ "' has no child in the foreign key relationship '"
            ++ show (relationshipName rel) ++ "'"


addReferencing :: (MonadDB m, ForeignKeyRelationship r)
               => r
               -> Referenced r
               -> Referencing r
               -> m (Referencing r)
addReferencing rel referenced referencing
  = setReferenced rel referencing (Just referenced)


removeReferencing :: (MonadDB m, ForeignKeyRelationship r)
                  => r
                  -> Referenced r
                  -> Referencing r
                  -> m (Referencing r)
removeReferencing = flip . removeReferenced


replaceReferencing :: (MonadDB m, ForeignKeyRelationship r)
                   => r
                   -> Referenced r
                   -> Maybe (Referencing r)
                   -> m (Maybe (Referencing r), Maybe (Referencing r))
replaceReferencing rel referenced newReferencing = do
  oldReferencing  <- getMaybeReferencing rel referenced
  oldReferencing' <- case oldReferencing of
                       Nothing -> return Nothing
                       Just o  -> liftM Just $ setReferenced rel o Nothing
  newReferencing' <- case newReferencing of
                       Nothing -> return Nothing
                       Just n  -> liftM Just $ setReferenced rel n (Just referenced)
  return (oldReferencing', newReferencing')
