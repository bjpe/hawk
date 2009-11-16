-- --------------------------------------------------------------------------
{- |
   Module      :  App.Model.Base.Package.Type
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  non-portable (MultiParamTypeClasses, TODO use at least Criteria ....)
   Version     :  $Id: Package.hs 274 2009-04-28 12:14:00Z inf6509 $

   The Type for a Package
-}
-- ----------------------------------------------------------------------------
module App.Model.Package.Type
    ( Package (..)
    ) where

import Hawk.Model
import Data.Time.Clock (UTCTime, getCurrentTime)
import Control.Monad.Trans (liftIO)

data Package = Package
    { _id         :: PrimaryKey
    , areaId      :: Maybe PrimaryKey
    , description :: String
    , comment     :: Maybe String
    , workMin     :: Double
    , workMax     :: Double
    , createdAt   :: Maybe UTCTime
    } deriving (Eq, Show)
  
instance Persistent Package where
  persistentType _ = "Package"

  fromSqlList l
    = Package (f 0 l) (f 1 l) (f 2 l) (f 3 l) (f 4 l) (f 5 l) (f 6 l)
      where f i = fromSql . (!!i)
      
  toSqlList x
    = map ($ x) [ toSql . _id
               , toSql . areaId
               , toSql . description
               , toSql . comment
               , toSql . workMin
               , toSql . workMax
               , toSql . createdAt
               ]
  
  tableName = const "packages"
    
  tableColumns = const [ "_id", "area_id", "description", "comment"
            , "work_min", "work_max", "created_at"]

instance WithPrimaryKey Package where
  primaryKey = _id
  pkColumn = head . tableColumns
  setPrimaryKey pk p = p {_id = pk}

instance Model Package where
  new = return $ Package 0 Nothing "" Nothing 0.0 0.0 Nothing
  insert p = do
    now <- liftIO getCurrentTime
    insertInTransaction $ p { createdAt = Just now }

instance Updateable Package where
  updater p name = do
    desc'    <- updater (description p) $ subParam name "description"
    comment' <- updater (comment     p) $ subParam name "comment"
    workMin' <- updater (workMin     p) $ subParam name "workMin"
    workMax' <- updater (workMax     p) $ subParam name "workMax"
    return $ p { description = desc', comment = comment', workMin = workMin', workMax = workMax' }

-- Konstante der maximalen Abweichung von minimalem und maximalem Aufwand
minMaxFactor :: Double
minMaxFactor = 2.0

instance Validatable Package where
  validator p = do
    validateNotNull "description" $ description p
    validateUniqueness [("area_id", toSql $ areaId p)] description "description" p
    check (wmin > 0.0) "must be greater than zero" "workMin"
    check (wmax > 0.0) "must be greater than zero" "workMax"
    check (wmax >= wmin) "the maximum work must be greater than the minimal" ""
    check (wmax <= minMaxFactor * wmin) ("the maximum may not be greater than " ++ show minMaxFactor ++ " times the minimum") ""
    return ()
    where
      wmin = workMin p
      wmax = workMax p
