module App.Model.User where

import Hawk.Model

-- User table
-- - username : String
-- - password : String
-- - e-mail : String
-- - _id : int
data User = User
  { _uid      :: PrimaryKey
  , username :: String
  , password :: String
  , email    :: Maybe String
  } deriving (Eq, Read, Show)

instance Persistent User where
  persistentType _ = "User"
  fromSqlList (l0:l1:l2:l3:[])
    = User (fromSql l0) (fromSql l1) (fromSql l2) (fromSql l3)
  fromSqlList _ = error "wrong list length"
  toSqlAL x = [ ("_uid"     , toSql $ _uid     x)
              , ("username" , toSql $ username x) 
              , ("password" , toSql $ password x)
              , ("email"    , toSql $ email    x)
              ]
  tableName = const "user"

-- primary key functionality
instance WithPrimaryKey User where
  primaryKey = _uid
  pkColumn = head . tableColumns
  setPrimaryKey pk c = c {_uid = pk}

instance Model User where
  new = return $ User 0 "" "" Nothing
  
instance Updateable User where
  updater u s = do
    user <- updater (username u) $ subParam s "username"
    pass <- updater (password u) $ subParam s "password"
    mail <- updater (email    u) $ subParam s "email"
    return $ u { username = user, password = pass, email = mail }

instance Validatable User where
  validator u = do
    validateNotNull    "username" $ username u
    validateUniqueness [("username", toSql $ username u)] username "username" u
    validateNotNull    "password" $ password u
    return ()


