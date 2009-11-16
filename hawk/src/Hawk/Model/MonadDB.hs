-- --------------------------------------------------------------------------
{- |
   Module      :  Hawk.Model.Persistence.MonadDB
   Copyright   :  Copyright (C) 2009 Bj�rn Peem�ller, Stefan Roggensack
   License     :  NONE

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Main.hs 57 2009-05-29 11:33:59Z inf6254 $

-}
-- --------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Hawk.Model.MonadDB
  ( MonadDB (..)
  , commit
  , rollback
  , inTransaction
  , sqlSelect
  , sqlExecute
  , sqlExecuteMany
  ) where

import Hawk.Model.Exception (SqlException (..))

import Prelude hiding (catch)

import Control.Monad.CatchIO
  ( MonadCatchIO
  , catch
  , onException
  )

import Control.Exception
  ( SomeException
  , throw
  )

import Control.Monad.Trans
  ( MonadIO
  , liftIO
  )

import qualified Database.HDBC as HDBC
  ( SqlValue
  , quickQuery'
  , execute
  , commit
  , ConnWrapper
  , rollback
  , Statement (executeMany)
  , IConnection (prepare)
  , catchSql
  )

import qualified System.Log.Logger as Logger
import System.Log.Logger.TH (deriveLoggers)
$(deriveLoggers "Logger" [Logger.DEBUG])

-- | Typeclass for operations on a database
class MonadCatchIO m => MonadDB m where
  getConnection :: m HDBC.ConnWrapper

-- | Handle a 'HDBC.SqlError' by catching it and throwing a 'SqlException'
handleSqlError :: IO a -> IO a
handleSqlError action = HDBC.catchSql action (throw . SqlException . show)

-- | Lift an 'IO'-Action using a 'HDBC.ConnWrapper' to a 'MonadDB' action.
--   'HDBC.SqlError's are catched and rethrown as 'SqlException's
liftDB :: MonadDB m => (HDBC.ConnWrapper -> IO a) -> m a
liftDB dbAction = getConnection >>= liftIO . handleSqlError . dbAction

-- | Commit the changes to the database
commit :: MonadDB m => m ()
commit = liftDB HDBC.commit

-- | Rollback the changes
rollback :: MonadDB m => m ()
rollback = liftDB HDBC.rollback

-- | Perform a non-lazy sql select
sqlSelect :: MonadDB m => String -> [HDBC.SqlValue] -> m [[HDBC.SqlValue]]
sqlSelect sql values = liftDB $ \conn -> do
  debugM $ "sqlSelect: sql=" ++ sql ++ ", values=" ++ show values
  res <- HDBC.quickQuery' conn sql values
  debugM $ "Returning: " ++ show res
  return res

-- | Execute a sql statement
sqlExecute :: MonadDB m => String -> [HDBC.SqlValue] -> m Integer
sqlExecute sql values  = liftDB $ \conn -> do
  debugM $ "sqlExecute: sql=" ++ sql ++ ", values=" ++ show values
  stat <- HDBC.prepare conn sql
  res <- HDBC.execute stat values
  -- TODO HDBC.run does not close connection correct every time, need to investigate
  debugM $ "Returning: " ++ show res
  return res

-- | Execute a sql statement many times using prepared statements
sqlExecuteMany :: MonadDB m => String -> [[HDBC.SqlValue]] -> m ()
sqlExecuteMany sql listOfValues = liftDB $ \conn -> do
  debugM $ "sqlExecuteMany: sql=" ++ sql ++ ", values=" ++ show listOfValues
  statement <- HDBC.prepare conn sql
  res <- HDBC.executeMany statement listOfValues
  debugM $ "Returning: " ++ show res
  return res

-- Execute a 'MonadDB' action in a transaction
inTransaction :: MonadDB m => m a -> m a
inTransaction action = onException committedAction $ rollback `catch` rollbackHandler
  where
    rollbackHandler :: MonadDB m => SomeException -> m ()
    rollbackHandler _ = return ()
    committedAction = do
                      res <- action
                      commit
                      return res
