-- --------------------------------------------------------------------------
{- |
   Module      :  App.Model.Base.Area.Functions
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  non-portable
   Version     :  $Id $

   The Functions for a Area
-}
-- ----------------------------------------------------------------------------
module App.Model.Area.Functions where

import App.Model.Area.Type
import App.Model.Area.ForeignKeys
import App.Model.Project.Type (Project)
import App.Model.Package.Type (Package)
import App.Model.Package.ForeignKeys

import Hawk.Model

getProject :: MonadDB m => Area -> m Project
getProject = getParent Area2Project

setProject :: MonadDB m => Area -> (Maybe Project) -> m Area
setProject = setParent Area2Project

getPackages :: MonadDB m => Area -> m [Package]
getPackages = getChildren Package2Area

addPackage :: MonadDB m => Area -> Package -> m Package
addPackage = addChild Package2Area

removePackage :: MonadDB m => Area -> Package -> m Package
removePackage = removeChild Package2Area

