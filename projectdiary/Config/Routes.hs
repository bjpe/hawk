-- --------------------------------------------------------------------------
{- |
   Module      :  Config.Routes
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Main.hs 57 2009-05-29 11:33:59Z inf6254 $

   The routes of the application.
-}
-- --------------------------------------------------------------------------
module Config.Routes where

import App.Action.CookieTest        as CookieTest
import App.Action.SessionTest       as SessionTest
--import App.Action.Project           as Project
import App.Action.ProjectController as Project
import App.Action.AdminController   as Admin
import App.View.DiaryView   as Diary
import App.Action.AreaController    as Area
import App.Action.PackageController as Package
import App.Action.StepController    as Step
import App.View.UserView    as User
import Hawk.Controller.Types              (Controller)

import qualified Data.Map as M

controllers :: M.Map String (M.Map String Controller)
controllers = M.singleton "diary"   (M.fromList Diary.views)
    `M.union` M.singleton "area"    (M.fromList Area.controllers)
    `M.union` M.singleton "package" (M.fromList Package.controllers)
    `M.union` M.singleton "step"    (M.fromList Step.controllers)
    `M.union` M.singleton "project" (M.fromList Project.controllers)
    `M.union` M.singleton "cookie"  (M.fromList CookieTest.controllers)
    `M.union` M.singleton "session" (M.fromList SessionTest.controllers)
    `M.union` M.singleton "admin"   (M.fromList Admin.controllers)
    `M.union` M.singleton "user"    (M.fromList User.views)
    `M.union` M.singleton ""        (M.fromList Diary.views)
