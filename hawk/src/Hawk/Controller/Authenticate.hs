{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module Hawk.Controller.Authenticate
where

--import Hawk.Controller.Types 
import Hawk.Controller.StateAccess ( getSessionValue, setSessionValue )
import Hawk.Controller.Auth.ResultType
import Hawk.Controller.Types -- ( AuthType (..) )
import Hawk.Model.MonadDB ( MonadDB )

import Control.Monad.Reader

auth :: (MonadDB m, MonadIO m, HasState m) => String -> String -> m AuthResult
auth u p = do 
  s <- asks configuration
  let f = authType s
  (authenticate f) u p


getSessionAuth :: (HasState m) => m (Maybe String)
getSessionAuth = getSessionValue "user_auth"

setSessionAuth :: (HasState m) => String -> m ()
setSessionAuth u = setSessionValue "user_auth" u

-- class Auth a where
--   auth :: a -> EnvController AuthResult

--  getAuthConfig :: EnvController a -- AuthConfig
--  getAuthConfig = asks (authConfigHandler . appConfiguration)

--  getAuthConfig = asks databaseConnection


