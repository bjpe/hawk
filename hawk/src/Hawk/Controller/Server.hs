-- --------------------------------------------------------------------------
{- |
   Module      :  $Header$
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Main.hs 57 2009-05-29 11:33:59Z inf6254 $

   The main dispatcher.
-}
-- --------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module Hawk.Controller.Server ( requestHandler ) where

import Control.Exception ( SomeException )
import Control.Monad.CatchIO ( catch )
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Either
import Control.Monad.Trans()
import Data.ByteString.Lazy.UTF8 ( fromString )
import Data.Default
import qualified Data.Map as M
  ( null
  , toList
  )
import Database.HDBC ( ConnWrapper )
import Prelude hiding ( catch )
import qualified System.Log.Logger as Logger
import System.Log.Logger.TH ( deriveLoggers )

-- Hwak imports
import Hawk.Controller.Cookies
import Hawk.Controller.Responses
import Hawk.Controller.StateAccess
  ( setSessionValue
  , getSessionValue
  , deleteSessionKey
  , getSession
  , setSession
  )

import Hawk.Controller.Static
import Hawk.Controller.Types
import Hawk.Controller.Util.Read ( maybeRead )


$(deriveLoggers "Logger" [Logger.DEBUG])

-- --------------------------------------------------------------------------
-- The request handler
-- --------------------------------------------------------------------------
-- | Runs the 'EnvController' from "Types" with the dispatching function 
-- to process all requests "Hack" sends to the application
requestHandler :: (forall a m. (AppConfiguration a, MonadIO m) => m a)  -- ^ 'AppConfiguration' Type you defined in your application, $a$ is not known to Hawk
               -> ConnWrapper        -- ^ Database Connection
               -> BasicConfiguration -- ^ Hawk configuration type for its modules, e.g. Session, Authentication, etc. as well as base directories
               -> Options            -- ^
               -> Application
requestHandler app conn conf opts env = runReaderT (runController dispatch) (RequestEnv conn conf env opts app)

-- --------------------------------------------------------------------------
-- Private request handling functions
-- --------------------------------------------------------------------------
-- | Dispatch a 'Request' and return the 'Response'
dispatch :: EnvController Response
dispatch = liftM addDefaultHeaders
         $ handleExceptions
         $ staticRequest
         $ trace
         $ tryDynamic error404

-- | Catch any 'Exception's and return a server error response
handleExceptions :: EnvController Response -> EnvController Response
handleExceptions contr = contr `catch` handler
  where
    handler :: SomeException -> EnvController Response
    handler e = return $ errorResponse $ fromString $ "Internal server error: " ++ show e

-- | try to run dynamic content
tryDynamic :: EnvController Response -> EnvController Response
tryDynamic contr = do
  rtng <- asks $ routing . configuration
  req <- asks request
  case rtng req of
    Nothing       -> contr
    Just dynContr -> executeController dynContr
--    Just (dynContr, [m a -> m a]) -> 

executeController :: StateController ByteString -> EnvController Response
executeController = (\s -> evalStateT s def)
                  . manageHeaders
                  . withCookies
                  . withSession
                  . sessionFlash
                  . liftM (either id pageResponse)
                  . runEitherT

manageHeaders :: HasState m => m Response -> m Response
manageHeaders contr = do
  hdrs <- M.toList `liftM` gets responseHeaders
  addCustomHeaders hdrs `liftM` contr

-- | Trace the incoming request and the calculated response
trace :: EnvController Response -> EnvController Response
trace contr = do
  asks request >>= debugM . show
  res <- contr
  debugM $ show res
  return res

-- | Provide a session for the invoked controller
withSession :: (MonadIO m, HasState m) => m a -> m a
withSession contr = do
  config <- asks configuration
  let store = sessionStore config
  let opts  = sessionOpts  config
  sess <- readSession store opts
  debugM $ "Incoming session: " ++ show sess
  setSession sess
  res <- contr
  sess' <- getSession
  debugM $ "Outgoing session: " ++ show sess'
  saveSession store opts sess'
  return res

-- | Handle a flash persisted inside a session in case of redirects
sessionFlash :: HasState m => m Response -> m Response
sessionFlash contr = do
  -- get flash out of the session
  flashInSession <- getSessionValue flashKey
  case flashInSession >>= maybeRead of
    Nothing -> return ()
    Just f  -> do
      modify $ \s -> s { flash = f }
      deleteSessionKey flashKey
  -- run the controller
  response <- contr
  -- save flash to session if we have a redirect
  when (isRedirect response) $ do
      f <- gets flash
      unless (M.null f) (setSessionValue flashKey $ show f)
  return response
  where flashKey = "_flash"
