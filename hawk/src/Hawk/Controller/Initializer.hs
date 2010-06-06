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
module Hawk.Controller.Initializer
  ( updateLogger
  , getApplication
  , defaultAppEnv
  ) where

import Hawk.Controller.Types
  ( AppEnvironment (..)
  , AppConfiguration (..)
  )

import System.Log.Logger
  ( Priority
  , updateGlobalLogger
  , setLevel
  )
  
import Hack (Application)

import Hawk.Controller.Server (requestHandler)
import Hawk.Controller.Session.NoSession (noSession)
import Hawk.Controller.Auth.EmptyAuth (emptyAuth)

import Database.HDBC.Sqlite3
import Database.HDBC (ConnWrapper(..))

defaultAppEnv :: IO AppEnvironment
defaultAppEnv = do
  dbConn <- (putStrLn "Config.Config: Initially connected to Database") >> connectSqlite3 "./db/database.db"
  return AppEnvironment
    { dbConnection = ConnWrapper dbConn
    , sessionStore = noSession
    , sessionOpts  = []
    , authType     = emptyAuth
    , authOpts     = []
    , routing      = undefined
    , templatePath = "./App/template"
    , publicDir    = "./public"
    , error401file = "401.html"
    , error404file = "404.html"
    , error500file = "500.html"
    , confOptions  = []
    , envOptions   = []
    , logLevels    = []
    , appData      = getInstance
    }

updateLogger :: [(String, Priority)] -> IO ()
updateLogger = mapM_ (\(name, level) -> updateGlobalLogger name (setLevel level))

-- | Call this function to encapsulate your application into Hawk-Framework
getApplication :: AppEnvironment     -- ^ 'AppEnvironment' type
               -> Application
getApplication env x = updLogger 
                    >> requestHandler env x
                    where updLogger = updateLogger $ logLevels env
