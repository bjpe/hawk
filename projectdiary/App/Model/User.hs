-- --------------------------------------------------------------------------
{- |
   Module      :  $Header$
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  non-portable (TODO use at least Criteria ....)
   Version     :  $Id: Area.hs 339 2009-05-16 13:19:28Z inf6509 $

   The functions for a User
-}
-- ----------------------------------------------------------------------------
module App.Model.User
    ( module App.Model.User.Type
    , module App.Model.User.Functions
    , module App.Model.User
    ) where

import App.Model.User.Type
import App.Model.User.Functions
import qualified App.Model.Project.Type as Project
import qualified App.Model.Worker as Worker

import Hawk.Model

import Control.Monad (liftM)
import Data.Digest.Pure.SHA (sha1, showDigest)
import Data.ByteString.Lazy.UTF8 (fromString)

deleteCascading :: MonadDB m => User -> m Bool
deleteCascading u = do
  getWorkers u >>= mapM delete
  delete u

userLogin :: MonadDB m => String -> String -> Project.Project -> m (Maybe User)
userLogin _name password _project = do
  user <- login _name password
  case user of
    Nothing -> return Nothing
    Just u  -> do
      atProject <- isAtProject u _project
      if atProject then return (Just u) else return Nothing

adminLogin :: MonadDB m => String -> String -> m (Maybe User)
adminLogin _name password = do
  user <- login _name password
  case user of
    Nothing -> return Nothing
    Just u  -> if admin u then return (Just u) else return Nothing

isAtProject :: MonadDB m => User -> Project.Project -> m Bool
isAtProject u p 
  = if admin u
      then return True
      else liftM (>0) $ countPersistents (undefined :: Worker.Worker) $ restrictionCriteria r
  where r = (col "user_id" .==. val (_id u)) .&&.
            (col "project_id" .==. val (Project._id p))
  
login :: MonadDB m => String -> String -> m (Maybe User)
login name' password = selectMaybe $ restrictionCriteria r
  where r = (col "name" .==. val name') .&&.
            (col "hashed_password" .==. val (hashPassword password))

hashPassword :: String -> String
hashPassword = showDigest . sha1 . fromString
