{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
-- session handling is required for authentification
module Hawk.Controller.Authenticate
where

--import Hawk.Controller.Types 
import Hawk.Controller.StateAccess ( getSessionValue, setSessionValue, deleteSessionKey, setFlash )
import Hawk.Controller.CustomResponses ( redirectToAction )
import Hawk.Controller.Auth.ResultType
import Hawk.Controller.Types -- ( AuthType (..) )
import Hawk.Model.MonadDB ( MonadDB )

import Control.Monad.Reader

-- monaddb only needed for db auth support
auth :: (MonadDB m, MonadIO m, HasState m) => String -> String -> m AuthResult
auth u p = do 
  s <- asks configuration
  let f = authType s
  authenticate f u p

tryLogin :: (MonadDB m, MonadIO m, HasState m) => String -> String -> m AuthResult
tryLogin = auth

getSessionAuth :: (HasState m) => m (Maybe String)
getSessionAuth = getSessionValue "user_auth"

setSessionAuth :: (HasState m) => String -> m ()
setSessionAuth = setSessionValue "user_auth"

delSessionAuth :: (HasState m) => m ()
delSessionAuth = deleteSessionKey "user_auth"

logout :: (HasState m) => m ()
logout = delSessionAuth

isAuthedAs :: (HasState m) => m (Maybe String)
isAuthedAs = getSessionAuth

isAuthed :: (HasState m) => m Bool
isAuthed = do
  a <- getSessionAuth
  case a of
    Nothing -> return False
    Just _  -> return True

-- argumente sind hier die aktion auf die redirected werden soll
authF :: String -> String -> StateController a -> StateController a
authF = authF' ""

authF' :: String -> String -> String -> StateController a -> StateController a
authF' e c a contr = do
  b <- isAuthed
  if b
    then contr
    else
      case e of 
        "" -> redirectToAction c a
        _ -> do
          setFlash "error" e --"You are not logged in."
          redirectToAction c a

-- if auth is implemented as controller store result in session
{-
getAuthResult :: (HasState m) => m (Maybe String)
getAuthResult = getSessionValue "auth_result" -}

