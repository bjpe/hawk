{-# LANGUAGE TemplateHaskell #-}
module Config.Config (configuration) 
where

import qualified Config.Routes as Routes (routing)

import Hawk.Controller.Routes (simpleRouting )
import Hawk.Controller.Session.CookieSession (cookieStore)
import Hawk.Controller.Auth.DbAuth (dbAuth)
--import Hawk.Controller.Auth.HttpAuth (httpAuth)
import Hawk.Controller.Types (AppEnvironment (..), AppConfiguration (..))

import Database.HDBC.Sqlite3
import Database.HDBC (ConnWrapper(..))
import System.Log.Logger

configuration :: IO AppEnvironment
configuration = do
  dbConn <- (putStrLn "Config.Config: Initially connected to Database") >> connectSqlite3 "./db/database.db"
  return AppEnvironment
    { dbConnection = ConnWrapper dbConn
    , sessionStore = cookieStore
    , sessionOpts  = [("secret", "12345678901234567890123456789012")]
  {-  , authType     = httpAuth
    , authOpts     = ["./db/httpbasic.csv"] -}
    , authType     = dbAuth
    , authOpts     = ["user", "username", "password", "", "username", "password"]
    , routing      = simpleRouting Routes.routing
    , templatePath = "./App/template"
    , publicDir    = "./public"
    , error401file = "401.html"
    , error404file = "404.html"
    , error500file = "500.html"
    , confOptions  = []
    , envOptions   = []
    , logLevels    = [(rootLoggerName, DEBUG)]
    , appData      = getInstance
    }

