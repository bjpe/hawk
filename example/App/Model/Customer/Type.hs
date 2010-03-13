module App.Model.Customer.Type (Customer (..)) where

import Hawk.Model
import Control.Monad.Trans (liftIO)
import Data.Time (Day, UTCTime(..), getCurrentTime)

-- the customer type
data Customer = Customer
  { _id         :: PrimaryKey
  , categoryId  :: ForeignKey
  , firstName   :: String
  , initials    :: Maybe String
  , lastName    :: String
  , dateOfBirth :: Day
  , updatedAt   :: Maybe UTCTime
  } deriving (Eq, Read, Show)
    
-- persistence information
instance Persistent Customer where
  persistentType _ = "Customer"
  fromSqlList (l0:l1:l2:l3:l4:l5:l6:[])
    = Customer (fromSql l0) (fromSql l1) (fromSql l2)
      (fromSql l3) (fromSql l4) (fromSql l5) (fromSql l6)
  fromSqlList _ = error "wrong list length"
  toSqlAL x = [ ("_id"          , toSql $ _id         x)
              , ("category_id"  , toSql $ categoryId  x) 
              , ("firstname"    , toSql $ firstName   x)
              , ("initials"     , toSql $ initials    x)
              , ("lastname"     , toSql $ lastName    x)
              , ("date_of_birth", toSql $ dateOfBirth x)
              , ("updated_at"   , toSql $ updatedAt   x)
              ]
  tableName = const "customers"

-- primary key functionality
instance WithPrimaryKey Customer where
  primaryKey = _id
  pkColumn = head . tableColumns
  setPrimaryKey pk c = c {_id = pk}

-- model behaviour
instance Model Customer where
  new = do
    t <- liftIO getCurrentTime
    return $ Customer 0 Nothing "" Nothing "" (utctDay t) Nothing
  insert c = do
    now <- liftIO getCurrentTime
    insertInTransaction $ c { updatedAt = Just now }
  update c = do
    now <- liftIO getCurrentTime
    updateInTransaction $ c { updatedAt = Just now }

-- validation
instance Validatable Customer where
  validator c = do
    validateNotNull "firstname"   $ firstName   c
    validateNotNull "lastname"    $ lastName    c
    validatePast    "dateOfBirth" $ UTCTime (dateOfBirth c) 0
    return ()

-- update
instance Updateable Customer where
  updater c name = do
    fn  <- updater (firstName   c) $ subParam name "firstname"
    ini <- updater (initials    c) $ subParam name "initials"
    ln  <- updater (lastName    c) $ subParam name "lastname"
    dob <- updater (dateOfBirth c) $ subParam name "dateofbirth"
    return $ c { firstName = fn, initials = ini
               , lastName = ln , dateOfBirth = dob }
