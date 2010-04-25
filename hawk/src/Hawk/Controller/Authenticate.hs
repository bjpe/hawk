--{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
-- session handling is required for authentification
module Hawk.Controller.Authenticate
where

--import Hawk.Controller.Types 
import Hawk.Controller.StateAccess ( getSessionValue, setSessionValue, deleteSessionKey, setFlash )
import Hawk.Controller.CustomResponses ( redirectToAction )
import Hawk.Controller.Types -- ( AuthType (..) )

import Control.Monad.Reader

import Data.Maybe (isJust)

-- monaddb only needed for db auth support
auth :: AuthType --(MonadDB m, MonadIO m, HasState m) => m AuthResult
auth = asks configuration >>= authType

tryLogin :: AuthType
tryLogin = auth

getSessionAuth :: (HasState m) => m (Maybe String)
getSessionAuth = getSessionValue authKey

setSessionAuth :: (HasState m) => String -> m ()
setSessionAuth = setSessionValue authKey

delSessionAuth :: (HasState m) => m ()
delSessionAuth = deleteSessionKey authKey

logout :: (HasState m) => m ()
logout = delSessionAuth

isAuthedAs :: (HasState m) => m (Maybe String)
isAuthedAs = getSessionAuth

isAuthed :: (HasState m) => m Bool
isAuthed = isJust `liftM` getSessionAuth

authKey :: String
authKey = "user_auth"

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

