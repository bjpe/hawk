-- --------------------------------------------------------------------------
{- |
   Module      :  App.Model.Base.Step.Functions
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  non-portable (MultiParamTypeClasses, TODO use at least Criteria ....)
   Version     :  $Id $

   The Functions for a Step
-}
-- ----------------------------------------------------------------------------
module App.Model.Step.Functions where

import Hawk.Model as P

import App.Model.Step.Type
import App.Model.Step.ForeignKeys
import App.Model.Package.Type (Package)
import App.Model.User.Type (User)

getPackage :: MonadDB m => Step -> m Package
getPackage = getParent Step2Package

setPackage :: MonadDB m => Step -> (Maybe Package) -> m Step
setPackage = setParent Step2Package

getUser :: MonadDB m => Step -> m User
getUser = getParent Step2User

setUser :: MonadDB m => Step -> (Maybe User) -> m Step
setUser = setParent Step2User
