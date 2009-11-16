module App.Model.Area.Type
    ( Area (..)
    ) where

import Hawk.Model
import Data.Time.Clock (UTCTime)

data Area = Area
    { _id         :: PrimaryKey
    , projectId   :: Maybe PrimaryKey
    , description :: String
    , comment     :: Maybe String
    , createdAt   :: Maybe UTCTime
    } deriving (Eq, Read, Show)

instance Persistent Area where
  persistentType _ = "Area"

  fromSqlList (l0:l1:l2:l3:l4:[])
    = Area (fromSql l0) (fromSql l1) (fromSql l2) (fromSql l3) (fromSql l4)
  fromSqlList _ = error "wrong list length"

  toSqlAL x = [ ("_id"        , toSql $ _id         x)
              , ("project_id" , toSql $ projectId  x) 
              , ("description", toSql $ description x)
              , ("comment"    , toSql $ comment     x)
              , ("created_at" , toSql $ createdAt   x)
              ]

  tableName = const "areas"

instance WithPrimaryKey Area where
  primaryKey         = _id
  pkColumn           = head . tableColumns
  setPrimaryKey pk p = p {_id = pk}

instance Model Area where
  new = return $ Area 0 Nothing "" Nothing Nothing

instance Updateable Area where
  updater a name = do
    desc'    <- updater (description a) $ subParam name "description"
    comment' <- updater (comment     a) $ subParam name "comment"
    return $ a { description = desc', comment = comment' }

instance Validatable Area where
  validator a = do
    validateNotNull "description" $ description a
    validateUniqueness [("project_id", toSql $ projectId a)] description "description" a
    return ()
