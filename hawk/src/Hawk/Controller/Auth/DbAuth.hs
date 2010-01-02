module Hawk.Controller.Auth.DbAuth where

import Hawk.Controller.Types
import Hawk.Controller.Auth.ResultType ( AuthResult (..) )
import Hawk.Controller.Authenticate

import Hawk.Model.MonadDB
import Hawk.Model.Criteria
import Hawk.Model.CriteriaSelect

import Database.HDBC.SqlValue
import Control.Monad.Trans (MonadIO)
import Control.Monad.Reader

-- authOpts have to be: [tableName, identityColumnName, credentialColumnName, cryptoFunctionName]

dbAuth :: AuthType
dbAuth = AuthType
  { -- authenticate :: (String, String) -> m AuthResult
    authenticate = dbAuthenticate
  }

dbAuthenticate :: (MonadDB m, MonadIO m, HasState m) => String -> String -> m AuthResult
dbAuthenticate u p = do
    sess <- getSessionAuth
    case sess of
      Nothing -> do
        (t, idCol, credCol, crypto) <- getOpts
        --select idcol and credcol from table where idcol == id
        rows <- querySelect
             $ setProjection [colP idCol, colP credCol]
             $ setTables [t]
             $ setCriteria (restrictionCriteria ((val u) .==. (col idCol)))
             $ newSelect
--        let rows = [[SqlString "admin"]]
        case rows of
          (x:[])   -> case lookupAuthDbRes (encrypt crypto p) x of -- compare for correct password
                        Nothing -> return AuthFailureInvalidCredential
                        Just v  -> setSessionAuth u >> return AuthSuccess -- TODO setup session values for that
          (x:xs:_) -> return AuthFailureAmbiguousId       -- id is not unique
          _        -> return AuthFailureIdNotFound        -- no identity found
      Just v  -> return AuthSuccess -- update session timeout, -> session timeout will be automatically updated, by session handler

getOpts :: (MonadIO m, HasState m) => m (String, String, String, String)
getOpts = do
  c <- asks configuration
  let o = authOpts c 
  case o of
    (t:i:c:cr:_) -> return (t, i, c, cr)
    _ -> return ("wrong-authOpts", "", "", "") -- this will occur in a db request error

lookupAuthDbRes :: String -> [SqlValue] -> Maybe String
lookupAuthDbRes s [] = Nothing
lookupAuthDbRes s (x:xs)
  | s == (fromSql x) = Just s
  | otherwise        = lookupAuthDbRes s xs

encrypt :: String -> String -> String
encrypt "MD5"      s = s -- hash s to md5
encrypt "PASSWORD" s = s -- do the password encryption algorithm for the db
encrypt _          s = s

