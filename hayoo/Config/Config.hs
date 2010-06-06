{-# LANGUAGE TemplateHaskell #-}
module Config.Config (configuration) 
where

import qualified Config.Routes as Routes (routing)

import Hawk.Controller.Initializer (defaultAppEnv)

import Hawk.Controller.Routes (simpleRouting )
import Hawk.Controller.Session.CookieSession (cookieStore)
import Hawk.Controller.Auth.DbAuth (dbAuth)
--import Hawk.Controller.Auth.HttpAuth (httpAuth)
import Hawk.Controller.Types (AppEnvironment (..))

import System.Log.Logger

configuration :: IO AppEnvironment
configuration = do
  def <- defaultAppEnv
  return $ def
    { sessionStore = cookieStore
    , sessionOpts  = [("secret", "12345678901234567890123456789012")]
  {-  , authType     = httpAuth
    , authOpts     = ["./db/httpbasic.csv"] -}
    , authType     = dbAuth
    , routing      = simpleRouting Routes.routing
    , logLevels    = [(rootLoggerName, DEBUG)]
    }

