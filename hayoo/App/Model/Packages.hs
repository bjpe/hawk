module App.Model.Packages where

import Hawk.Model

data Packages = Packages
  { _pid  :: PrimaryKey
  , name  :: String
  } deriving (Eq, Read, Show)

instance Persistent Packages where
  persistentType _ = "Packages"
  fromSqlList (l0:l1:[])
    = Packages (fromSql l0) (fromSql l1)
  fromSqlList _ = error "wrong list length"
  toSqlAL x = [ ("_pid" , toSql $ _uid x)
              , ("name" , toSql $ name x) 
              ]
  tableName = const "packages"

-- primary key functionality
instance WithPrimaryKey Packages where
  primaryKey = _pid
  pkColumn = head . tableColumns
  setPrimaryKey pk c = c {_pid = pk}

instance Model Packages where
  new = return $ Packages 0 ""
  
instance Updateable Packages where
  updater p s = do
    pa <- updater (name p) $ subParam s "name"
    return $ p { name = pa }

instance Validatable Packages where
  validator p = do
    validateNotNull    "name" $ name p
    validateUniqueness [("name", toSql $ name p)] name "name" p
    return ()

