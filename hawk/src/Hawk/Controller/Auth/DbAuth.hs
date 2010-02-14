module Hawk.Controller.Auth.DbAuth where

import Hawk.Controller.Types
import Hawk.Controller.Auth.ResultType ( AuthResult (..) )
import Hawk.Controller.Authenticate
import Hawk.Controller.Request ( getParam )

import Hawk.Model.Criteria
import Hawk.Model.CriteriaSelect

import Database.HDBC.SqlValue
import Control.Monad.Trans ()
import Control.Monad.Reader

-- authOpts are: [tableName, identityColumnName, credentialColumnName, cryptoFunctionName, username-formfield, password-formfield]

--(MonadDB m, MonadIO m, HasState m) => String -> String -> m AuthResult
dbAuth :: AuthType
dbAuth = do
    sess <- isAuthed
    case sess of
      False -> do
        (t, idCol, credCol, crypto, user, pass) <- getOpts
        u <- getParam user
        p <- getParam pass
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
                        Just _  -> setSessionAuth u >> return AuthSuccess
          (_:_:_) -> return AuthFailureAmbiguousId       -- id is not unique
          _        -> return AuthFailureIdNotFound        -- no identity found
      True -> return AuthSuccess

getOpts :: (MonadIO m, HasState m) => m (String, String, String, String, String, String)
getOpts = do
  conf <- asks configuration
  let o = authOpts conf 
  case o of
    (t:i:c:cr:uf:pf:_) -> return (t, i, c, cr, uf, pf)
    _ -> return ("wrong-authOpts", "", "", "", "", "") -- this will occur in a db request error

lookupAuthDbRes :: String -> [SqlValue] -> Maybe String
lookupAuthDbRes _ [] = Nothing
lookupAuthDbRes s (x:xs)
  | s == (fromSql x) = Just s
  | otherwise        = lookupAuthDbRes s xs

encrypt :: String -> String -> String
encrypt "MD5"      s = s -- hash s to md5
encrypt "PASSWORD" s = s -- do the password encryption algorithm for the db
encrypt _          s = s

