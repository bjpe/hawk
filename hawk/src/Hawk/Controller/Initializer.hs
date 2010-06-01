-- --------------------------------------------------------------------------
{- |
   Module      :  $Header$
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Main.hs 57 2009-05-29 11:33:59Z inf6254 $

   The initializer for loading the environment, connecting to the database
   and such stuff.
-}
-- --------------------------------------------------------------------------
{-# LANGUAGE Rank2Types #-}
module Hawk.Controller.Initializer
  ( AppEnvironment (..)
  , loadEnvironment
  , updateLogger
  , getApplication
--  , AppConfiguration
  ) where

import Hawk.Controller.Types
  ( BasicConfiguration
  , Options
  , AppConfiguration (..)
  )

import System.Log.Logger
  ( Priority
  , updateGlobalLogger
  , setLevel
--  , rootLoggerName
  )
  
import Control.Monad.Trans

import Database.HDBC (ConnWrapper)
import Hack (Application)

import Hawk.Controller.Server (requestHandler)

-- | 'AppEnvironment' you have to define to run your application properly
data AppEnvironment = AppEnvironment
  { connectToDB :: IO ConnWrapper       -- ^ DB connection to your DB, e.g. $ConnWrapper \`liftM\` connectSqlite3 \".\/db\/database.db\"$
                                        -- atm only Sqlite3 is supported
  , logLevels   :: [(String, Priority)] -- ^
  , envOptions  :: Options              -- ^
  }

loadEnvironment :: AppEnvironment -> IO (ConnWrapper, Options)
loadEnvironment appEnv = do
  updateLogger (logLevels appEnv)
  db <- connectToDB appEnv
  return (db, envOptions appEnv)

updateLogger :: [(String, Priority)] -> IO ()
updateLogger = mapM_ (\(name, level) -> updateGlobalLogger name (setLevel level))

-- | Call this function to encapsulate your application into Hawk-Framework
getApplication :: (forall a m. (AppConfiguration a, MonadIO m) => m a)  -- ^ 'AppConfiguration' Type you defined in your application, not known to Hawk
               -> AppEnvironment     -- ^ 'AppEnvironment' look above
               -> BasicConfiguration -- ^ Hawk configuration Type
               -> Application
getApplication app env conf x = do
  (conn, envOpts) <- loadEnvironment env
  requestHandler app conn conf envOpts x
