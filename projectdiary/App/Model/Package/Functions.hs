-- --------------------------------------------------------------------------
{- |
   Module      :  $Header$
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  non-portable (MultiParamTypeClasses, TODO use at least Criteria ....)
   Version     :  $Id $

   The Functions for a Package
-}
-- ----------------------------------------------------------------------------
module App.Model.Package.Functions where

import App.Model.Package.Type
import App.Model.Package.ForeignKeys
import App.Model.Area.Type (Area)
import App.Model.Step.Type (Step)
import App.Model.Step.ForeignKeys

import Hawk.Model

getArea :: MonadDB m => Package -> m Area
getArea = getParent Package2Area

setArea :: MonadDB m => Package -> (Maybe Area) -> m Package
setArea = setParent Package2Area

getSteps :: MonadDB m => Package -> m [Step]
getSteps = getChildren Step2Package

addStep :: MonadDB m => Package -> Step -> m Step
addStep = addChild Step2Package

removeStep :: MonadDB m => Package -> Step -> m Step
removeStep = removeChild Step2Package

