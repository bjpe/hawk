-- --------------------------------------------------------------------------
{- |
   Module      :  Hawk.Controller.Session.DatabaseSession
   Copyright   :  Copyright (C) 2010 Björn Peemöller
   License     :  NONE
   
   Maintainer  :  inf6254@fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Main.hs 57 2009-05-29 11:33:59Z inf6254 $
  
   Saving a session in the database.
-}
-- --------------------------------------------------------------------------
module Hawk.Controller.Session.DatabaseSession (databaseStore) where

import Data.Maybe (fromMaybe)
import Data.Time (UTCTime)
import Hawk.Controller.Types
import Hawk.Controller.Session
import Hawk.Controller.Cookies
import Hawk.Controller.Util.Read (maybeRead)
import Hawk.Model

databaseStore :: SessionStore
databaseStore = SessionStore
  { readSession   = readDatabaseSession
  , saveSession   = saveDatabaseSession
  }

data RawSession = RawSession
  { rs_id     :: Integer
  , rs_expiry :: Maybe UTCTime
  , rs_data   :: String
  }

instance Persistent RawSession where
  persistentType _             = "Session"
  fromSqlList (l0:l1:l2:[])    = RawSession (fromSql l0) (fromSql l1) (fromSql l2)
  fromSqlList _                = error "wrong list length"
  toSqlAL raw                  = [ ("_id"   , toSql $ rs_id     raw)
                                 , ("expiry", toSql $ rs_expiry raw)
                                 , ("data"  , toSql $ rs_data   raw)
                                 ]                                 
  tableName                    = const "session"

instance WithPrimaryKey RawSession where
  primaryKey = rs_id
  pkColumn _ = "_id"
  setPrimaryKey pk raw = raw { rs_id = pk }

toRaw :: Session -> RawSession
toRaw s = RawSession (read $ sessionId s) (expiry s) (show $ dataStore s)

fromRaw :: RawSession -> Session
fromRaw raw = Session (show $ rs_id raw) (rs_expiry raw) (read $ rs_data raw)

newDatabaseSession :: HasState m => m Session
newDatabaseSession = do
  rs <- insertByPK $ toRaw $ emptySession "0"
  commit
  return $ fromRaw rs

readDatabaseSession :: HasState m => SessionOpts -> m Session
readDatabaseSession opts = do
  sId <- getCookieValue $ sessionCookieName opts
  return (sId >>= maybeRead) >>= maybe (return Nothing) findMaybeByPK >>= maybe newDatabaseSession (return . fromRaw)

saveDatabaseSession :: HasState m => SessionOpts -> Session -> m ()
saveDatabaseSession opts sess = do
  updateByPK (toRaw sess) >> commit
  setCookie $ (newCookie (sessionCookieName opts) (sessionId sess)) { cookiePath = Just "/" }

sessionCookieName :: SessionOpts -> String
sessionCookieName = fromMaybe defaultName . lookup "sessionId"
