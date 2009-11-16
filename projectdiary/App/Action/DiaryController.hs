-- --------------------------------------------------------------------------
{- |
   Module      :  App.Controller.DiaryController
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Main.hs 57 2009-05-29 11:33:59Z inf6254 $

   Controller for showing and modifying the project diary.
-}
-- --------------------------------------------------------------------------
module App.Action.DiaryController where

-- App imports
import App.Action.MainController
import qualified App.Model.User as User
import App.Model.Project

-- Hawk imports
import Hawk.Controller
import Hawk.Model

-- other imports
import Data.Maybe (fromJust)
import Control.Monad (liftM)

loginAction :: StateController ()
loginAction = do
  method <- getRequestMethod
  case method of
    POST -> do
      p    <- getParam "project.id" >>= findOne . read
      n    <- getParam "user"
      pass <- getParam "password"
      user <- User.userLogin n pass p
      case user of
        Nothing -> do
          setFlash "error" "Login failed"
          return ()
        Just u  -> do
          setSessionValue "user_id"    $ show $ User._id    u
          setSessionValue "project_id" $ show $ _id         p
          setSessionValue "user_name"         $ User.name   u
          redirectToAction "diary" "index"
    _ -> do
      clearSession
      setFlash "notice" "Please login to your project"
      return ()

logoutAction :: StateController ()
logoutAction = redirectToLogin "diary" Nothing

indexAction :: StateController Project
indexAction = fromJust `liftM` currentProject

editAction :: StateController Project
editAction = do
  project <- fromJust `liftM` currentProject
  method <- getRequestMethod
  case method of
    POST -> do
      (p, errs) <- getParams >>= updateAndValidate project "project"
      if null errs then do
        update p
        setFlash "notice" "The project has been updated successfully"
        redirectToAction "diary" "index"
        else do
          setErrors "project" errs
          return p
    _    -> return project
