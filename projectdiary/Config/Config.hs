module Config.Config where

import Config.Routes as Routes

import System.Log.Logger
import Hawk.Controller.Initializer (AppEnvironment (..))
import Hawk.Controller.Types
import Hawk.Controller.Routes (simpleRouting)
import Hawk.Controller.Session.DatabaseSession


import Database.HDBC.Sqlite3
import qualified Database.HDBC as HDBC (ConnWrapper(..))
import Control.Monad (liftM)
import Data.ByteString.Lazy.UTF8 (fromString)
import Hawk.Controller.Auth.EmptyAuth (emptyAuth)

development :: AppEnvironment
development = AppEnvironment
  { connectToDB = liftM HDBC.ConnWrapper $ connectSqlite3 "./db/database.db"
  , logLevels   = [(rootLoggerName, DEBUG), ("Hawk.Model", DEBUG)]
  , envOptions  = [] -- [("hide_hol", "True")] -- TODO need to document
  }

configuration :: BasicConfiguration
configuration = BasicConfiguration
  { -- Session
    sessionStore    = databaseStore
  , sessionOpts     = [("secret" , "12345678901234567890123456789012")]

    -- dispatcher
  , routing         = simpleRouting Routes.controllers

    -- paths
  , templatePath    = "./App/template"
  , publicDir       = "./public"

    -- other
  , confOptions     = []
  , error404file    = "404.html"
  , error500file    = "500.html"
  , authType        = emptyAuth
  , authOpts        = []
  }

instance AppConfiguration () where
    getInstance = return ()
