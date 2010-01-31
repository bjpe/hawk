-- --------------------------------------------------------------------------
{- |
   Module      :  $Header$
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Main.hs 57 2009-05-29 11:33:59Z inf6254 $

   
-}
-- --------------------------------------------------------------------------
{-# LANGUAGE Rank2Types, FlexibleContexts, FlexibleInstances,
    GeneralizedNewtypeDeriving, TypeFamilies, UndecidableInstances #-}
module Hawk.Controller.Types
  ( Options
  , BasicConfiguration (..)
  , RequestEnv (..)
  , ResponseState (..)
  , HasState
  , module Hack

  , AppConfiguration (..)

  , EnvController (..)
  , StateController

  , View (..)

  , AuthType (..)
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

import Hawk.Controller.Auth.ResultType

import Hawk.Model.MonadDB ( MonadDB (..) )

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
-- Options, BasicConfiguration, ResponseEnv, ResponseState
-- --------------------------------------------------------------------------
type Options = [(String, String)]

data BasicConfiguration = BasicConfiguration
  { sessionStore :: SessionStore
  , sessionOpts  :: SessionOpts
--  , authConfig   :: AuthConfig
  , authType     :: AuthType
  , authOpts     :: [String]
  , routing      :: Hack.Env -> Maybe Controller
  , templatePath :: String
  , publicDir    :: String
  , error401file :: String
  , error404file :: String
  , error500file :: String
  , confOptions  :: Options
  }

data RequestEnv = RequestEnv
  { databaseConnection :: ConnWrapper
  , configuration      :: BasicConfiguration
  , request            :: Hack.Env
  , environmentOptions :: Options
  , appConfiguration   :: (AppConfiguration a) => a -- type defined by application
  }

data ResponseState = ResponseState
  { session            :: Session
  , cookies            :: [Cookie]
  , responseHeaders    :: Map String String
  , flash              :: Map String String
  , errors             :: Map String [(String, String)]
  }

instance Default ResponseState where def = ResponseState def def def def def

class ( MonadReader RequestEnv m, MonadState ResponseState m, MonadCatchIO m ) => HasState m where

instance (MonadReader RequestEnv m, MonadCatchIO m) => MonadDB m where
  getConnection = asks databaseConnection

-- --------------------------------------------------------------------------
-- EnvController
-- --------------------------------------------------------------------------
newtype EnvController a
  = EnvController { runController :: ReaderT RequestEnv IO a }
  deriving (Functor, Monad, MonadIO, MonadCatchIO, MonadReader RequestEnv)

--instance MonadDB EnvController where getConnection = asks databaseConnection

-- --------------------------------------------------------------------------
-- StateController
-- --------------------------------------------------------------------------
type StateController = EitherT Response (StateT ResponseState EnvController)

instance HasState (StateT ResponseState EnvController) where

instance HasState (EitherT e (StateT ResponseState EnvController)) where
--instance MonadDB (EitherT e (StateT ResponseState EnvController)) where
--  getConnection = lift $ lift getConnection

-- --------------------------------------------------------------------------
-- AuthedStateController
-- --------------------------------------------------------------------------
-- type AuthedStateController = undefined

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
-- Authentication
-- --------------------------------------------------------------------------
data AuthType = AuthType
  { authenticate :: (MonadDB m, MonadIO m, HasState m) => String -> String -> m AuthResult }

-- --------------------------------------------------------------------------
-- Routing
-- --------------------------------------------------------------------------
data Route = Route
  { routeController :: String
  , routeAction     :: String
  } deriving (Show)

type Routing = (String, Controller)
type Controller = StateController ByteString

-- --------------------------------------------------------------------------
-- AppConfig
-- --------------------------------------------------------------------------
class AppConfiguration a where
  getInstance :: a

