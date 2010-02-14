module Hawk.Controller.Auth.HttpAuth where

import Hawk.Controller.Types
import Hawk.Controller.Auth.ResultType ( AuthResult (..) )
import Hawk.Controller.Authenticate

import Hawk.Controller.Request
import Hawk.Controller.Util.Text

import Hawk.Model.MonadDB()

import Control.Monad.Trans
import Control.Monad.Reader

import HAppS.Crypto.Base64 ( decode )

-- only supports csv based basic authentication
-- this http authentication is only usable for a single userclass
-- maybe extend this by multiple domain specific configurations ?
-- (MonadDB m, MonadIO m, HasState m) => String -> String -> m AuthResult
httpAuth :: AuthType
httpAuth = do
  sess <- isAuthed
  case sess of
    False -> do
      a <- getRequestHeader "Authorization"
      case a of
        Nothing -> return AuthFailureIdNotFound
        Just v  ->
          let s = snd (splitWhere (== ' ') v) 
              (u,p) = splitWhere (== ':') (decode s)
          in httpAuth' u p
    True -> return AuthSuccess
  where 
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

getOpts :: (MonadIO m, HasState m) => m String
getOpts = do
  conf <- asks configuration
  let o = authOpts conf 
  case o of
    (p:_) -> return p
    _ -> return "wrong-authOpts" -- this will occur in a db request error

