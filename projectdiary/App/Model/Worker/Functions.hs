-- --------------------------------------------------------------------------
{- |
   Module      :  App.Model.Base.Worker.Functions
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  non-portable (MultiParamTypeClasses, TODO use at least Criteria ....)
   Version     :  $Id $

   The functions for a Worker
-}
-- ----------------------------------------------------------------------------
module App.Model.Worker.Functions where

import Hawk.Model as P
import App.Model.Worker.Type
import App.Model.Worker.ForeignKeys
import App.Model.User.Type (User)
import App.Model.Project.Type (Project)

getUser :: MonadDB m => Worker -> m User
getUser = getParent Worker2User

setUser :: MonadDB m => Worker -> (Maybe User) -> m Worker
setUser = setParent Worker2User

getMaybeUser :: MonadDB m => Worker -> m (Maybe User)
getMaybeUser = getMaybeParent Worker2User

setProject :: MonadDB m => Worker -> (Maybe Project) -> m Worker
setProject = setParent Worker2Project

getProject :: MonadDB m => Worker -> m Project
getProject = getParent Worker2Project
