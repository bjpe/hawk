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

module Hawk.Model.Persistent
    ( -- Persistent and methods
      Persistent (..)
    , countPersistents
    , selectPersistents
    , selectOnePersistent
    , selectMaybePersistent
    , deletePersistentsByCriteria
    , deleteByCriteriaInTransaction
    , insertPersistent
    ) where

import Hawk.Model.Criteria
import Hawk.Model.CriteriaSelect
import Hawk.Model.Exception
import Hawk.Model.MonadDB
import Hawk.Model.Types
import Hawk.Model.Util

import Control.Exception (throw, onException)

import Control.Monad (liftM)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Char (toLower)

-- --------------------------------------------------------------------------
-- Persistent
-- --------------------------------------------------------------------------

-- | Typeclass for structures which are persistent in a database
--   Minimal implementation: 'persistentType', 'fromSqlList', 'toSqlAL'
class Persistent p where

  -- | The type of the persistent. The result must not use the value passed
  --   in such that
  --   persistentType undefined
  --   will give an appropriate result
  persistentType :: p -> String

  -- | Marshal a 'Persistent' as a list of 'SqlValue's
  toSqlList   :: p -> [SqlValue]
  toSqlList   = map snd . toSqlAL

  -- | Unmarshal a 'Persistent' from a list of 'SqlValue's
  fromSqlList :: [SqlValue] -> p
  fromSqlList list = result
    where
      result   = fromSqlAL $ zip columns list
      columns  = tableColumns (typeOf result)

  -- | Get the name of the associated database table, defaults to
  --  (++"s") map toLower $ persistentType p
  tableName :: p -> TableName
  tableName = (++"s") . map toLower . persistentType

  -- | Get the columns of the table
  tableColumns :: p -> [ColumnName]
  tableColumns = map fst . toSqlAL

  -- | Get the table columns and the values as an associated list
  toSqlAL :: p -> [(ColumnName, SqlValue)]
  toSqlAL p = zip (tableColumns p) (toSqlList p)

  -- | Unmarshal a persistent from an associated list containing the table
  --   columns and the 'SqlValue's.
  fromSqlAL :: [(ColumnName, SqlValue)] -> p
  fromSqlAL = fromSqlList . map snd


-- --------------------------------------------------------------------------
-- Methods for Persistents
-- --------------------------------------------------------------------------

-- | Unmarshal a list of 'SqlValue's into a 'Persistent'.
--   Any 'Exception' is caught and rethrown as an 'UnmarshalException'.
unmarshal :: (MonadIO m, Persistent p) => [SqlValue] -> m p
unmarshal list = result
  where result = liftIO $ onException (return $ fromSqlList list)
                        $ throw (UnmarshalException msg)
        msg    = "Could not convert " ++ show list
              ++ " to Persistent " ++ persistentType (typeOfM result)

-- | Get the qualified table columns (table.column) of a 'Persistent'
qualifiedTableColumns :: Persistent p => p -> [String]
qualifiedTableColumns p = map prependTable $ tableColumns p
  where prependTable c  = tableName p ++ '.' : c

-- | Extend a 'Criteria' for a 'Persistent' to a 'Query'
toQuery :: Persistent p => p -> Criteria -> Query
toQuery p c = setProjection (map colP $ qualifiedTableColumns p)
            $ setCriteria c
            $ setTables [tableName p] 
              newSelect

-- | Count the number of 'Persistent's satisfying the given 'Criteria'
countPersistents :: (MonadDB m, Persistent p) => p -> Criteria -> m Integer
countPersistents p c = liftM (fromSql . head . head)
                     $ querySelect
                     $ setProjection [rowCountP]
                     $ setCriteria c
                     $ setTables [tableName p]
                       newSelect

-- | Select a list of 'Persistent's by a 'Criteria'
selectPersistents :: (MonadDB m, Persistent p) => Criteria -> m [p]
selectPersistents c = result
  where result = querySelect (toQuery (typeOfM2 result) c) >>=
                 mapM unmarshal

-- | Select maybe one 'Persistent' by a 'Criteria'
selectMaybePersistent :: (MonadDB m, Persistent p) => Criteria -> m (Maybe p)
selectMaybePersistent = liftM safeHead . selectPersistents

-- | Select one 'Persistent' by a 'Criteria'. If no 'Persistent' satisfying
--   the 'Criteria' is found a 'RecordNotFound' exception is thrown.
--   If more then one record is found the first record will be returned.
selectOnePersistent :: (MonadDB m, Persistent p) => Criteria -> m p
selectOnePersistent c = liftM (unsafeHeadC c)
                      $ selectPersistents c

-- | Remove 'Persistent's by a 'Criteria'
deletePersistentsByCriteria :: (MonadDB m, Persistent p) => p -> Criteria -> m Integer
deletePersistentsByCriteria p c = executeManipulation 
                                $ setManipulationCriteria c 
                                $ newDelete 
                                $ tableName p


deleteByCriteriaInTransaction :: (MonadDB m, Persistent p) => p -> Criteria -> m Integer
deleteByCriteriaInTransaction p = inTransaction . deletePersistentsByCriteria p

-- |Insert a 'Persistent' into the database.
insertPersistent :: (MonadDB m, Persistent p) => p -> m Integer
insertPersistent p = executeManipulation
                   $ setValues (toSqlAL p)
                   $ newInsert 
                   $ tableName p
