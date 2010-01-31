-- --------------------------------------------------------------------------
{- |
   Module      :  $Header$
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Main.hs 57 2009-05-29 11:33:59Z inf6254 $

   Basic operations for sessions.
-}
-- --------------------------------------------------------------------------
module Hawk.Controller.Session
  ( Session
  , SessionOpts
  , defaultName
  , emptySession
  , sessionId
  , setSessionId
  , expiry
  , setExpiry
  , getValue
  , setValue
  , deleteKey
  , expired
  ) where

import Control.Monad (liftM)

import Control.Monad.Trans
  ( MonadIO
  , liftIO
  )

import Data.Time
  ( UTCTime
  , getCurrentTime
  )

import Data.Default

import qualified Data.Map as M
  ( Map
  , empty
  , lookup
  , insert
  , delete
  )

-- | Options for 'Session's
type SessionOpts = [(String, String)]

-- | A Session
data Session = Session
  { sessionId :: String
  , expiry    :: Maybe UTCTime
  , dataStore :: M.Map String String
  } deriving (Read, Show)

instance Default Session where
  def = emptySession defaultName

-- | The default name of a 'Session'
defaultName :: String
defaultName = "holsession"

-- | Create an empty 'Session' from an id
emptySession :: String -> Session
emptySession sId = Session sId Nothing M.empty

-- | Set the id of a 'Session'
setSessionId :: String -> Session -> Session
setSessionId sId s = s { sessionId = sId }

-- | Set the expiry time
setExpiry :: Maybe UTCTime -> Session -> Session
setExpiry time s = s { expiry = time }

-- | Get a value from a 'Session'
getValue :: String -> Session -> Maybe String
getValue key = M.lookup key . dataStore

-- | Set a value in the 'Session'
setValue :: String -> String -> Session -> Session
setValue k v s = s { dataStore = M.insert k v $ dataStore s }

-- | Delete a key from the 'Session'
deleteKey :: String -> Session -> Session
deleteKey k s = s { dataStore = M.delete k $ dataStore s }

-- | Checks whether a 'Session' is expired
expired :: MonadIO m => Session -> m Bool
expired s = case expiry s of
  Nothing   -> return False
  Just time -> liftM (> time) $ liftIO getCurrentTime
