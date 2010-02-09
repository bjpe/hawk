module Config.Config where

import qualified Config.Routes as Routes (routing)

import Hawk.Controller.Initializer (AppEnvironment (..))
import Hawk.Controller.Routes (simpleRouting)
import Hawk.Controller.Session.NoSession (noSession)
import Hawk.Controller.Auth.EmptyAuth (emptyAuth)
import Hawk.Controller.Types (BasicConfiguration (..), AppConfiguration (..))

import Control.Monad (liftM)
import Database.HDBC.Sqlite3
import Database.HDBC (ConnWrapper(..))
import System.Log.Logger

development :: AppEnvironment
development = AppEnvironment
  { connectToDB = ConnWrapper `liftM` connectSqlite3 "./db/database.db"
  , logLevels   = [(rootLoggerName, DEBUG)]
  , envOptions  = []
  }

configuration :: BasicConfiguration
configuration = BasicConfiguration
  { sessionStore = noSession
  , sessionOpts  = []
  , authType     = emptyAuth
  , authOpts     = []
  , routing      = simpleRouting Routes.routing
  , templatePath = "./App/template"
  , publicDir    = "./public"
  , confOptions  = []
  , error401file = "401.html"
  , error404file = "404.html"
  , error500file = "500.html"
  }

instance AppConfiguration () where getInstance = return ()
