module App.Model.Worker.Type 
    ( Worker (..)
    ) where

import Hawk.Model

data Worker = Worker
    { _id         :: PrimaryKey
    , userId      :: Maybe PrimaryKey
    , projectId   :: Maybe PrimaryKey
    } deriving (Eq, Read, Show)
  
instance Persistent Worker where
  persistentType _ = "Worker"

  fromSqlList l 
    = Worker (f 0 l) (f 1 l) (f 2 l)
      where f i = fromSql . (!!i)
      
  toSqlList x 
    = map ($ x) [ toSql . _id
               , toSql . userId
               , toSql . projectId
               ]
               
  tableName 
    = const "workers"
    
  tableColumns = const ["_id", "user_id", "project_id"]

instance WithPrimaryKey Worker where
  primaryKey = _id
  pkColumn = head . tableColumns
  setPrimaryKey pk p = p {_id = pk}

instance Model Worker where
  new = return $ Worker 0 Nothing Nothing

instance Validatable Worker where
  validator w = do
    validateUniqueness [("project_id", toSql $ projectId w)] userId "user_id" w
    return ()
