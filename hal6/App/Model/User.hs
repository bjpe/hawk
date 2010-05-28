module App.Model.User (User(..)) where

import Hawk.Model

import Control.Monad.Reader (liftIO)
import Data.Time (UTCTime, getCurrentTime)

data User = User 
  { _id       :: PrimaryKey
  , username  :: String
  , password  :: String
  , student   :: Bool
  , tutorial  :: Bool
  , workshop  :: Bool
  , party     :: Bool
  , created   :: UTCTime
  }

instance Persistent User where
  persistentType _ = "User"
  fromSqlList (l0:l1:l2:l3:l4:l5:l6:l7:[])
    = User (fromSql l0) (fromSql l1) (fromSql l2) (fromSql l3)
           (fromSql l4) (fromSql l5) (fromSql l6) (fromSql l7)
  fromSqlList _ = error "wrong list length"
  toSqlAL x = [ ("_id"           , toSql $ _id            x)
              , ("username"      , toSql $ username       x) 
              , ("password"      , toSql $ password       x)
              , ("student"       , toSql $ student        x)
              , ("tutorial"      , toSql $ tutorial       x) 
              , ("workshop"      , toSql $ workshop       x)
              , ("party"         , toSql $ party          x)
              , ("created"       , toSql $ created        x)
              ]
  tableName = const "user"

instance WithPrimaryKey User where
  primaryKey = _id
  pkColumn = head . tableColumns
  setPrimaryKey pk c = c {_id = pk}

instance Model User where
  new = (liftIO getCurrentTime) 
    >>= (\t -> return $ User 0 "" "" False False False False t)
  insert u = (liftIO getCurrentTime) 
    >>= (\t -> insertInTransaction $ u { created = t })
  
instance Updateable User where
  updater u s = do
    n <- updater (username  u) $ subParam s "username"
    p <- updater (password  u) $ subParam s "password"
    b <- updater (student   u) $ subParam s "student"
    t <- updater (tutorial  u) $ subParam s "tutorial"
    w <- updater (workshop  u) $ subParam s "workshop"
    g <- updater (party     u) $ subParam s "party"
    return $ u 
      { username = n
      , password = p
      , student = b
      , tutorial = t
      , workshop = w
      , party = g }

instance Validatable User where
  validator u = do
    _ <- validateNotNull    "username"       $ username u
    _ <- validateNotNull    "password"       $ password u
    _ <- validateUniqueness [("username", toSql $ username u)] username "username" u
    return ()

