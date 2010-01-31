-- --------------------------------------------------------------------------
{- |
   Module      :  $Header$
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  non-portable (MultiParamTypeClasses, TODO use at least Criteria ....)
   Version     :  $Id $

   The Functions for a Project
-}
-- ----------------------------------------------------------------------------
module App.Model.Project.Functions where

import App.Model.Project.Type
import App.Model.Area.Type (Area)
import App.Model.Area.ForeignKeys
import App.Model.Worker.Type (Worker)
import App.Model.Worker.ForeignKeys

import Hawk.Model

getAreas :: MonadDB m => Project -> m [Area]
getAreas = getChildren Area2Project

addArea :: MonadDB m => Project -> Area -> m Area
addArea = addChild Area2Project

removeArea :: MonadDB m => Project -> Area -> m Area
removeArea = removeChild Area2Project


getWorkers :: MonadDB m => Project -> m [Worker]
getWorkers = getChildren Worker2Project

addWorker :: MonadDB m => Project -> Worker -> m Worker
addWorker = addChild Worker2Project

removeWorker :: MonadDB m => Project -> Worker -> m Worker
removeWorker = removeChild Worker2Project

