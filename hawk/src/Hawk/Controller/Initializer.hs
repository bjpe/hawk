-- --------------------------------------------------------------------------
{- |
   Module      :  $Header$
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Main.hs 57 2009-05-29 11:33:59Z inf6254 $

   The initializer for loading the environment, connecting to the database
   and such stuff.
-}
-- --------------------------------------------------------------------------
-- {-# LANGUAGE Rank2Types #-}
module Hawk.Controller.Initializer
  ( updateLogger
  , getApplication
  ) where

import Hawk.Controller.Types
  ( AppEnvironment (..)
  )

import System.Log.Logger
  ( Priority
  , updateGlobalLogger
  , setLevel
  )
  
import Hack (Application)

import Hawk.Controller.Server (requestHandler)

updateLogger :: [(String, Priority)] -> IO ()
updateLogger = mapM_ (\(name, level) -> updateGlobalLogger name (setLevel level))

-- | Call this function to encapsulate your application into Hawk-Framework
getApplication :: AppEnvironment     -- ^ 'AppEnvironment' type
               -> Application
getApplication env x = updLogger 
                    >> requestHandler env x
                    where updLogger = updateLogger $ logLevels env
