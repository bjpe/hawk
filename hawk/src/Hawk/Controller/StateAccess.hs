module Hawk.Controller.StateAccess where

import Control.Monad (liftM)
import Control.Monad.State
  ( modify
  , gets
  )
import Control.Monad.Trans (MonadIO)
import qualified Data.Map as M
import Data.Time (UTCTime)
import Hawk.Controller.Session
import Hawk.Controller.Types
import Hawk.Controller.Util.Read

setHeader :: HasState m => String -> String -> m ()
setHeader k v = modify $ \s -> s { responseHeaders = M.insert k v $ responseHeaders s }

setFlash :: HasState m => String -> String -> m ()
setFlash key msg = modify $ \s -> s { flash = M.insert key msg $ flash s }

getFlash :: HasState m => String -> m (Maybe String)
getFlash key = gets $ M.lookup key . flash

setErrors :: HasState m => String -> [(String, String)] -> m ()
setErrors key errs = modify $ \s -> s { errors = M.insert key errs $ errors s }

getErrors :: HasState m => String -> m [(String, String)]
getErrors key = gets $ M.findWithDefault [] key . errors

-- --------------------------------------------------------------------------
-- Session
-- --------------------------------------------------------------------------
getSession :: HasState m => m Session
getSession = gets session

setSession :: HasState m => Session -> m ()
setSession sess = modify $ \s -> s { session = sess }

getSessionValue :: HasState m => String -> m (Maybe String)
getSessionValue key = gets $ getValue key . session

readSessionValue :: (HasState m, Read v) => String -> m (Maybe v)
readSessionValue = liftM (>>= maybeRead) . getSessionValue

setSessionValue :: HasState m => String -> String -> m ()
setSessionValue k v = do
  sess <- getSession
  modify $ \s -> s { session = setValue k v sess }

deleteSessionKey :: HasState m => String -> m ()
deleteSessionKey key = do
  sess <- getSession
  modify $ \s -> s { session = deleteKey key sess }

clearSession :: HasState m => m ()
clearSession = do
  sess <- getSession
  modify $ \s -> s { session = emptySession (sessionId sess) }

sessionExpired :: (HasState m, MonadIO m) => m Bool
sessionExpired = getSession >>= expired

getSessionExpiry :: HasState m => m (Maybe UTCTime)
getSessionExpiry = gets $ expiry . session

setSessionExpiry :: HasState m => Maybe UTCTime -> m ()
setSessionExpiry e = do
  sess <- getSession
  modify $ \s -> s { session = setExpiry e sess }
