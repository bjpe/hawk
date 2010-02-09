module App.Model.GuestbookEntry (GuestbookEntry (..)) where

import Hawk.Model
import Control.Monad.Trans (liftIO)
import Data.Time (UTCTime, getCurrentTime)

data GuestbookEntry = GuestbookEntry
  { _id         :: PrimaryKey
  , name        :: String
  , message     :: String
  , createdAt   :: UTCTime
  } deriving (Eq, Read, Show)

instance Persistent GuestbookEntry where
  persistentType _ = "GuestbookEntry"
  fromSqlList (l0:l1:l2:l3:[])
    = GuestbookEntry (fromSql l0) (fromSql l1) (fromSql l2) (fromSql l3)
  fromSqlList _ = error "wrong list length"
  toSqlAL x = [ ("_id"       , toSql $ _id       x)
              , ("name"      , toSql $ name      x)
              , ("message"   , toSql $ message   x)
              , ("createdAt" , toSql $ createdAt x)
              ]
  tableName = const "guestbook"

instance WithPrimaryKey GuestbookEntry where
  primaryKey = _id
  pkColumn = head . tableColumns
  setPrimaryKey pk ge = ge {_id = pk}

instance Model GuestbookEntry where
  new = do
    t <- liftIO getCurrentTime
    return $ GuestbookEntry 0 "" "" t
  insert ge = do
    now <- liftIO getCurrentTime
    insertInTransaction $ ge { createdAt = now }

instance Validatable GuestbookEntry where
  validator ge = do
    validateNotNull "name"          $ name    ge
    validateLength  5 400 "message" $ message ge
    return ()

instance Updateable GuestbookEntry where
  updater ge s = do
    n <- updater (name    ge) $ subParam s "name"
    m <- updater (message ge) $ subParam s "message"
    return $ ge { name = n, message = m }
