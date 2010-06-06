module Hawk.Controller.Auth.Types where

data AuthResult = AuthSuccess
                | AuthFailureUnknown String -- can contain a sql exception string
                | AuthFailureIdNotFound
                | AuthFailureAmbiguousId
                | AuthFailureInvalidCredential

{-class AuthMethod a where
  auth :: AuthType

class AuthDB a where
  getById :: String  -- input from a browser interface ever will be a string
          -> (MonadDB m, MonadIO m, HasState m) => m (Either AuthResult a) -- i.e. password or openid response data
-}
