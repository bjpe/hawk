module Hawk.Controller.Auth.HttpAuth where

import Hawk.Controller.Types
import Hawk.Controller.Auth.ResultType ( AuthResult (..) )
import Hawk.Controller.Authenticate

import Hawk.Controller.Request
import Hawk.Controller.Util.Text

import Hawk.Model.MonadDB

import Control.Monad.Trans
import Control.Monad.Reader

import HAppS.Crypto.Base64 ( decode )

httpAuth :: AuthType
httpAuth = AuthType
  { -- authenticate :: String -> String -> m AuthResult
    authenticate = httpAuthenticate
  }
  
-- only supports basic authentication atm
-- this http authentication is only usable for a single userclass
-- maybe extend this by multiple domain specific configurations ?
httpAuthenticate :: (MonadDB m, MonadIO m, HasState m) => String -> String -> m AuthResult
httpAuthenticate user pass = do
  sess <- isAuthed
  case sess of
    False -> do
      if notEmpty user pass
        then httpAuth' user pass
        else do
          a <- getRequestHeader "Authorization"
          case a of
            Nothing -> return AuthFailureIdNotFound
            Just v  ->
              let s = snd (splitWhere (== ' ') v) 
                  (u,p) = splitWhere (== ':') (decode s)
              in httpAuth' u p
    True -> return AuthSuccess

httpAuth' :: (MonadDB m, MonadIO m, HasState m) => String -> String -> m AuthResult
httpAuth' u p = do
  path <- getOpts
  fc <- liftIO (readFile path)
  let t = splitAll (== ',') fc
      l = map (splitWhere (== ':')) t
  case (lookup u l) of
    Nothing -> return AuthFailureIdNotFound
    Just v -> if v == p
      then setSessionAuth u >> return AuthSuccess
      else return AuthFailureInvalidCredential

notEmpty :: String -> String -> Bool
notEmpty "" "" = False
notEmpty _  _  = True

getOpts :: (MonadIO m, HasState m) => m String
getOpts = do
  conf <- asks configuration
  let o = authOpts conf 
  case o of
    (p:_) -> return p
    _ -> return "wrong-authOpts" -- this will occur in a db request error

