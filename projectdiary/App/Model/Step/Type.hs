-- --------------------------------------------------------------------------
{- |
   Module      :  $Header$
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  non-portable (MultiParamTypeClasses, TODO use at least Criteria ....)
   Version     :  $Id: Step.hs 237 2009-04-24 10:11:08Z inf6254 $

   The Type for a Step
-}
-- ----------------------------------------------------------------------------
module App.Model.Step.Type
    ( Step (..)
    ) where

import Hawk.Model
import Data.Time.Clock (UTCTime, getCurrentTime)
import Control.Monad.Trans (liftIO)

data Step = Step
    { _id         :: PrimaryKey
    , packageId   :: Maybe PrimaryKey
    , userId      :: Maybe PrimaryKey
    , description :: String
    , comment     :: Maybe String
    , duration    :: Double
    , completion  :: Int
    , createdAt   :: Maybe UTCTime
    } deriving (Eq, Read, Show)
 
instance Persistent Step where
  persistentType _ = "Step"

  fromSqlList l
    = Step (f 0 l) (f 1 l) (f 2 l) (f 3 l) (f 4 l) (f 5 l) (f 6 l) (f 7 l)
      where f i = fromSql . (!!i)
      
  toSqlList x
    = map ($ x) [ toSql . _id
               , toSql . packageId
               , toSql . userId
               , toSql . description
               , toSql . comment
               , toSql . duration
               , toSql . completion
               ]
                           
  tableName 
    =  const "steps"
    
  tableColumns 
    = const [ "_id", "package_id", "user_id", "description", "comment"
             , "duration", "completiondegree", "created_at" ]

instance WithPrimaryKey Step where
  primaryKey = _id
  pkColumn = head . tableColumns
  setPrimaryKey pk p = p {_id = pk}

instance Model Step where
    new = return $ Step 0 Nothing  Nothing "" Nothing 0.0 0 Nothing
    insert p = do
        now <- liftIO getCurrentTime
        insertInTransaction $ p { createdAt = Just now }

instance Updateable Step where
  updater s name = do
    desc'       <- updater (description s) $ subParam name "description"
    comment'    <- updater (comment     s) $ subParam name "comment"
    duration'   <- updater (duration    s) $ subParam name "duration"
    completion' <- updater (completion  s) $ subParam name "completion"
    return $ s { description = desc', comment = comment', duration = duration',
            completion = completion' }

instance Validatable Step where
  validator s = do
    validateNotNull "description" $ description s
    validateUniqueness [("package_id", toSql $ packageId s)] description "description" s
    validate (> 0.0) "must be greater than zero" "duration" $ duration s
    check (c >= 0 && c <= 100) "must be between 0 and 100" "completion"
    return ()
    where c = completion s
