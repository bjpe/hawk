-- --------------------------------------------------------------------------
{- |
   Module      :  $Header$
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3
   
   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Main.hs 57 2009-05-29 11:33:59Z inf6254 $
  
   Saving a session in the database.
-}
-- --------------------------------------------------------------------------
module Hawk.Core.Session.DatabaseSession where

import Hawk.Controller.Session

instance HasState m => SessionStore m where
  -- newSession    :: SessionOpts -> m Session
  newSession = undefined
    
  -- readSession   :: SessionOpts -> m Session
  readSession = undefined
    
  -- saveSession   :: SessionOpts -> Session -> m ()
  saveSession = undefined
  
  -- deleteSession :: SessionOpts -> Session -> m ()
  deleteSession = undefined
