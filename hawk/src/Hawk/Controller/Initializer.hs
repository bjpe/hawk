-- --------------------------------------------------------------------------
{- |
   Module      :  Hawk.Core.Initializer
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  NONE

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Main.hs 57 2009-05-29 11:33:59Z inf6254 $

   The initializer for loading the environment, connecting to the database
   and such stuff.
-}
-- --------------------------------------------------------------------------
module Hawk.Controller.Initializer
  ( AppEnvironment (..)
  , loadEnvironment
  , updateLogger
  , getApplication
  ) where

import Hawk.Controller.Types
  ( AppConfiguration
  , Options
  )

import System.Log.Logger
  ( Priority
  , updateGlobalLogger
  , setLevel
--  , rootLoggerName
  )

import Database.HDBC (ConnWrapper)
import Hack (Application)

import Hawk.Controller.Server (requestHandler)

data AppEnvironment = AppEnvironment
  { connectToDB :: IO ConnWrapper
  , logLevels   :: [(String, Priority)]
  , envOptions  :: Options
  }

loadEnvironment :: AppEnvironment -> IO (ConnWrapper, Options)
loadEnvironment appEnv = do
  updateLogger (logLevels appEnv)
  db <- connectToDB appEnv
  return (db, envOptions appEnv)

updateLogger :: [(String, Priority)] -> IO ()
updateLogger = mapM_ (\(name, level) -> updateGlobalLogger name (setLevel level))

getApplication :: AppEnvironment -> AppConfiguration -> Application
getApplication env conf x = do
  (conn, envOpts) <- loadEnvironment env
  requestHandler conn conf envOpts x
