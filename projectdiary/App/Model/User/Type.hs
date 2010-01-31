module App.Model.User.Type 
    ( User (..)
    ) where

import Hawk.Model

data User = User
    { _id             :: PrimaryKey
    , name            :: String
    , hashedPassword  :: String
    , admin           :: Bool
    } deriving (Eq, Read, Show)
  
instance Persistent User where
  persistentType _ = "User"

  fromSqlList l 
    = User (f 0 l) (f 1 l) (f 2 l) (int2Bool $ f 3 l)
      where f i = fromSql . (!!i)
      
  toSqlList x 
    = map ($ x) [ toSql . _id
               , toSql . name
               , toSql . hashedPassword
               , toSql . bool2Int . admin
               ]
               
  tableName = const "users"
    
  tableColumns  = const ["_id", "name", "hashed_password", "admin"]

bool2Int :: Bool -> Int
bool2Int False = 0
bool2Int True  = 1

int2Bool :: Int -> Bool
int2Bool 0 = False
int2Bool _ = True

instance WithPrimaryKey User where
  primaryKey = _id
  pkColumn = head . tableColumns
  setPrimaryKey pk p = p {_id = pk}

instance Model User where
  new = return $ User 0 "" "" False

instance Updateable User where
  updater u paramName = do
    name'  <- updater (name  u) $ subParam paramName "name"
    admin' <- updater (admin u) $ subParam paramName "admin"
    return $ u { name = name', admin = admin' }

instance Validatable User where
  validator u = do
    validateNotNull "name" $ name u
    validateUniqueness [] name "name" u
    validateNotNull "password" $ hashedPassword u
    return ()
