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
module Hawk.Model.WithPrimaryKey
  ( WithPrimaryKey (..)
  , findByPKs
  , findOneByPK
  , findMaybeByPK
  , insertByPK
  , insertInTransaction
  , updateByPK
  , updateInTransaction
  , deleteByPK
  , deleteInTransaction
  ) where

import Hawk.Model.Criteria
import Hawk.Model.MonadDB
import Hawk.Model.Persistent
import Hawk.Model.Types
import Hawk.Model.Util

import Control.Monad (liftM)

-- | Typeclass for persistent structures with a 'PrimaryKey'
class Persistent p => WithPrimaryKey p where

  -- | Get the column containing the 'PrimaryKey'
  pkColumn       :: p -> ColumnName

  -- | Get the 'PrimaryKey'
  primaryKey     :: p -> PrimaryKey

  -- | Set the 'PrimaryKey'
  setPrimaryKey  :: PrimaryKey -> p -> p


-- --------------------------------------------------------------------------
-- Methods for WithPrimaryKey
-- --------------------------------------------------------------------------

-- | Get the values of a 'WithPrimaryKey' without the 'PrimaryKey'
nonPKValues :: (WithPrimaryKey p) => p -> [(ColumnName, SqlValue)]
nonPKValues p = filter ((/= pkColumn p) . fst) $ toSqlAL p

-- | Create a 'Criteria' for the 'PrimaryKey's in the list
idCriteria :: (WithPrimaryKey p) => p -> [PrimaryKey] -> Criteria
idCriteria p ks = setRestriction (inExpr (col $ pkColumn p) ks) newCriteria

-- TODO documentation
findByPKs :: (MonadDB m, WithPrimaryKey p) => [PrimaryKey] -> m [p]
findByPKs ks = result
  where result = selectPersistents $ idCriteria (typeOfM2 result) ks

-- TODO documentation
findOneByPK :: (MonadDB m, WithPrimaryKey p) => PrimaryKey -> m p
findOneByPK k = result
  where result  = selectOnePersistent $ idCriteria (typeOfM result) [k]

-- TODO documentation
findMaybeByPK :: (MonadDB m, WithPrimaryKey p) => PrimaryKey -> m (Maybe p)
findMaybeByPK k = result
  where result  = selectMaybePersistent $ idCriteria (typeOfM2 result) [k]

-- |Insert a 'WithPrimaryKey' into the database
--  The result contains the original 'WithPrimaryKey' with its 'PrimaryKey'
--  updated
insertByPK :: (MonadDB m, WithPrimaryKey p) => p -> m p
insertByPK p = do
  _ <- executeManipulation $ setValues (nonPKValues p) $ newInsert $ tableName p
  newId <- sqlSelect "SELECT last_insert_rowid()" []
  return $ setPrimaryKey (fromSql $ head $ head newId) p

insertInTransaction :: (MonadDB m, WithPrimaryKey p) => p -> m p
insertInTransaction = inTransaction . insertByPK

updateByPK :: (MonadDB m, WithPrimaryKey p) => p -> m Bool
updateByPK p = liftM (==1)
             $ executeManipulation
             $ setManipulationCriteria (idCriteria p [primaryKey p])
             $ setSet (nonPKValues p)
             $ newUpdate
             $ tableName p

updateInTransaction :: (MonadDB m, WithPrimaryKey p) => p -> m p
updateInTransaction p = inTransaction $ updateByPK p >> return p

deleteByPK :: (MonadDB m, WithPrimaryKey p) => p -> m Bool
deleteByPK p = liftM (==1)
             $ deletePersistentsByCriteria p 
             $ idCriteria p [primaryKey p]

deleteInTransaction :: (MonadDB m, WithPrimaryKey p) => p -> m Bool
deleteInTransaction = inTransaction . deleteByPK
