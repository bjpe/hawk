-- --------------------------------------------------------------------------
{- |
   Module      :  Hawk.Core.Session.NoSession
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  NONE

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Main.hs 57 2009-05-29 11:33:59Z inf6254 $

   Does not save the session
-}
-- --------------------------------------------------------------------------
module Hawk.Controller.Session.NoSession where

import Hawk.Controller.Types
import Hawk.Controller.Session

noSession :: SessionStore
noSession = SessionStore 
  { -- readSession   :: s -> SessionOpts -> m Session
    readSession = \_ -> return $ emptySession "emptySession"

  , -- saveSession   :: s -> SessionOpts -> Session -> m ()
    saveSession = \_ _ -> return ()
  }

