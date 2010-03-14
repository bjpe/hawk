{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
-- session handling is required for authentification
module Hawk.Controller.Authenticate
where

--import Hawk.Controller.Types 
import Hawk.Controller.StateAccess ( getSessionValue, setSessionValue, deleteSessionKey, setFlash )
import Hawk.Controller.CustomResponses ( redirectToAction )
import Hawk.Controller.Types -- ( AuthType (..) )

import Control.Monad.Reader

-- monaddb only needed for db auth support
auth :: AuthType --(MonadDB m, MonadIO m, HasState m) => m AuthResult
auth = do 
  s <- asks configuration
  authType s

tryLogin :: AuthType
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

