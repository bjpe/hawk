module App.Model.Project.Type
  ( Project (..)
  ) where

import Hawk.Model
import Control.Monad (liftM)
import Control.Monad.Trans (liftIO)
import Data.Time.Calendar (Day)
import Data.Time.Clock
 ( getCurrentTime
 , UTCTime (..)
 )

data Project = Project
    { _id         :: PrimaryKey
    , prefix      :: String
    , description :: String
    , startAt     :: Day
    , endAt       :: Day
    } deriving (Eq, Read, Show)


instance Persistent Project where
  persistentType _ = "Project"
  
  fromSqlList (l0:l1:l2:l3:l4:[])
    = Project (fromSql l0) (fromSql l1) (fromSql l2) (fromSql l3) (fromSql l4)
  fromSqlList _ = error "this type could not convertet to Project"

  toSqlAL x = [ ("_id"        , toSql $ _id         x)
              , ("prefix"     , toSql $ prefix      x)
              , ("description", toSql $ description x)
              , ("start_at"   , toSql $ startAt     x)
              , ("end_at"     , toSql $ endAt       x)
              ]

  tableName _ = "projects"

instance WithPrimaryKey Project where
  primaryKey         = _id
  pkColumn           = head . tableColumns
  setPrimaryKey pk p = p {_id = pk}

instance Model Project where
  new = do
    today <- liftM utctDay $ liftIO getCurrentTime
    return $ Project 0 "" "" today today

instance Updateable Project where
  updater p name = do
    prefix'  <- updater (prefix      p) $ subParam name "prefix"
    desc'    <- updater (description p) $ subParam name "description"
    startAt' <- updater (startAt     p) $ subParam name "startAt"
    endAt'   <- updater (endAt       p) $ subParam name "endAt"
    return $ p { prefix = prefix', description = desc', startAt = startAt', endAt = endAt' }

instance Validatable Project where
  validator p = do
    validateNotNull "prefix" (prefix p)
    validateNotNull "description" (description p)
    validateUniqueness [] prefix "prefix" p
    check (startAt p <= endAt p) "the start date must be before the end date" ""
    return ()
