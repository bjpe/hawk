-- --------------------------------------------------------------------------
{- |
   Module      :  $Header$
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Main.hs 57 2009-05-29 11:33:59Z inf6254 $

   Controller for session handling and authentication
-}
-- --------------------------------------------------------------------------
module App.Action.MainController where

import Hawk.Controller
import Hawk.Model
import Hawk.Controller.Util.Monad (liftMaybe)

import App.Model.User    as User
import App.Model.Project as Project

import Data.Maybe (fromMaybe, fromJust, isNothing, isJust)
import Data.Time.Clock (getCurrentTime, addUTCTime, NominalDiffTime)
import Control.Monad.Trans (liftIO)
import Control.Monad (liftM, when)

maxSessionTime :: NominalDiffTime
maxSessionTime = 60 * 30 -- 30 minutes

-- --------------------------------------------------------------------------
-- Login functions
-- --------------------------------------------------------------------------

-- Close the current session and redirect to the login page
redirectToLogin :: String -> Maybe String -> StateController a
redirectToLogin contr msg = do
  clearSession
  setFlash "notice" $ fromMaybe "Please log in" msg
  redirectToAction contr "login"


-- Check if the logged in user has the right to access the seleceted project
projectAuthorize :: Project -> StateController ()
projectAuthorize p = prepareSession "diary" $ do
  cp <- currentProject
  when (Just p /= cp) $ redirectToLogin "diary" Nothing


-- Check if the logged in user has the right to access the diary
userAuthorize :: StateController a -> StateController a
userAuthorize contr = prepareSession "diary" $ do
  u <- currentUser
  p <- currentProject
  if isNothing u || isNothing p
    then redirectToLogin "diary" Nothing
    else do
      permitted <- isAtProject (fromJust u) (fromJust p)
      if permitted then contr
        else redirectToLogin "diary" Nothing


-- Check if the logged in user has admin privileges
adminAuthorize :: StateController a -> StateController a
adminAuthorize contr = prepareSession "admin" $ do
  adminId <- getSessionValue "admin_id"
  if isJust adminId then contr else redirectToLogin "admin" Nothing


-- Return the project logged in to
currentProject :: (MonadDB m, HasState m) => m (Maybe Project)
currentProject = readSessionValue "project_id" >>= liftMaybe findMaybe


justProject :: StateController Project
justProject = fromJust `liftM` currentProject


-- Return the user logged in
currentUser :: StateController (Maybe User)
currentUser = readSessionValue "user_id" >>= liftMaybe findMaybe


-- Check whether the session has not expired
prepareSession :: String -> StateController a -> StateController a
prepareSession target contr = do
  sess <- getSession
  isExpired <- expired sess
  if isExpired then redirectToLogin target (Just "Your session has expired")
    else do
      now <- liftIO getCurrentTime
      setSession $ setExpiry (Just $ addUTCTime maxSessionTime now) sess
      contr

