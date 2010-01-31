-- --------------------------------------------------------------------------
{- |
   Module      :  $Header$
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  non-portable (MultiParamTypeClasses, TODO use at least Criteria ....)
   Version     :  $Id $

   The Functions for a User
-}
-- ----------------------------------------------------------------------------
module App.Model.User.Functions where

import Hawk.Model as P
import App.Model.User.Type
import App.Model.Step.Type (Step)
import App.Model.Step.ForeignKeys
import App.Model.Worker.Type (Worker)
import App.Model.Worker.ForeignKeys

getWorkers :: MonadDB m => User -> m [Worker]
getWorkers = getChildren Worker2User

addWorker :: MonadDB m => User -> Worker -> m Worker
addWorker = addChild Worker2User

removeWorker :: MonadDB m => User -> Worker -> m Worker
removeWorker = removeChild Worker2User

getSteps :: MonadDB m => User -> m [Step]
getSteps = getChildren Step2User

addStep :: MonadDB m => User -> Step -> m Step
addStep = addChild Step2User

removeStep :: MonadDB m => User -> Step -> m Step
removeStep = removeChild Step2User

