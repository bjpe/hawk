-- --------------------------------------------------------------------------
{- |
   Module      :  Hawk.Core.Types
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  NONE

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Main.hs 57 2009-05-29 11:33:59Z inf6254 $

   
-}
-- --------------------------------------------------------------------------
{-# LANGUAGE Rank2Types, FlexibleContexts, FlexibleInstances,
    GeneralizedNewtypeDeriving, TypeFamilies #-}
module Hawk.Controller.Types
  ( Options
  , AppConfiguration (..)
  , RequestEnv (..)
  , ResponseState (..)
  , HasState
  , module Hack

  , EnvController (..)
  , StateController

  , View (..)

  , SessionStore (..)
  , Route (..)
  , Routing
  , Controller
  , ByteString
  ) where

import Hawk.Controller.Session
  ( Session
  , SessionOpts
  )

import Hawk.Model ( MonadDB (..) )

import Control.Monad.CatchIO ( MonadCatchIO (..) )
import Control.Monad.Either
import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString.Lazy ( ByteString )
import Data.Default
import Data.Map (Map)
import Database.HDBC ( ConnWrapper )
import Hack
import Network.CGI.Cookie ( Cookie )

-- --------------------------------------------------------------------------
-- Options, AppConfiguration, ResponseEnv, ResponseState
-- --------------------------------------------------------------------------
type Options = [(String, String)]

data AppConfiguration = AppConfiguration
  { sessionStore :: SessionStore
  , sessionOpts  :: SessionOpts
  , routing      :: Hack.Env -> Maybe Controller
  , templatePath :: String
  , publicDir    :: String
  , error404file :: String
  , error500file :: String
  , confOptions  :: Options
  }

data RequestEnv = RequestEnv
  { databaseConnection :: ConnWrapper
  , configuration      :: AppConfiguration
  , request            :: Hack.Env
  , environmentOptions :: Options
  }

data ResponseState = ResponseState
  { session            :: Session
  , cookies            :: [Cookie]
  , responseHeaders    :: Map String String
  , flash              :: Map String String
  , errors             :: Map String [(String, String)]
  }

instance Default ResponseState where def = ResponseState def def def def def

class ( MonadReader RequestEnv m, MonadState ResponseState m ) => HasState m where

-- --------------------------------------------------------------------------
-- EnvController
-- --------------------------------------------------------------------------
newtype EnvController a
  = EnvController { runController :: ReaderT RequestEnv IO a }
  deriving (Functor, Monad, MonadIO, MonadCatchIO, MonadReader RequestEnv)

instance MonadDB EnvController where
  getConnection = asks databaseConnection

-- --------------------------------------------------------------------------
-- StateController
-- --------------------------------------------------------------------------
type StateController = EitherT Response (StateT ResponseState EnvController)

instance HasState (StateT ResponseState EnvController) where

instance HasState (EitherT e (StateT ResponseState EnvController)) where
instance MonadDB (EitherT e (StateT ResponseState EnvController)) where
  getConnection = lift $ lift getConnection

-- --------------------------------------------------------------------------
-- Rendering
-- --------------------------------------------------------------------------
class View a where
  type Target a :: *
  render :: a -> Target a -> StateController ByteString

-- --------------------------------------------------------------------------
-- SessionStore
-- --------------------------------------------------------------------------
data SessionStore = SessionStore
  { readSession   :: (MonadIO m, HasState m) => SessionOpts -> m Session
  , saveSession   :: (MonadIO m, HasState m) => SessionOpts -> Session -> m ()
  }

-- --------------------------------------------------------------------------
-- Routing
-- --------------------------------------------------------------------------
data Route = Route
  { routeController :: String
  , routeAction     :: String
  } deriving (Show)

type Routing = (String, Controller)
type Controller = StateController ByteString