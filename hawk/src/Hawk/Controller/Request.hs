{-# LANGUAGE FlexibleContexts #-}
module Hawk.Controller.Request
  ( getRequestMethod
  , getScriptName
  , getPathInfo
  , getEscapedPath
  , getQueryString
  , getServerName
  , getServerPort
  , getRequestHeaders
  , getRequestHeader
  , getReferer
  , getUrlScheme
  , getRequestBody
  , lookupParam
  , getParam
  , readParam
  , getParams
  ) where

import Control.Monad (liftM)
import Control.Monad.Reader 
  ( MonadReader
  , asks
  )
import Data.ByteString.Lazy.UTF8 (toString)
import qualified Data.Map as M
import Hawk.Controller.Types
import Hawk.Controller.Util.Read
import Hawk.Controller.Util.Uri
  ( fromParamString
  , unescapeUri
  )

getRequestMethod :: (MonadReader RequestEnv m) => m RequestMethod
getRequestMethod = asks $ requestMethod . request

getScriptName :: (MonadReader RequestEnv m) => m String
getScriptName = asks $ scriptName . request

getPathInfo :: (MonadReader RequestEnv m) => m String
getPathInfo = asks $ pathInfo . request

getEscapedPath :: (MonadReader RequestEnv m) => m String
getEscapedPath = asks $ unescapeUri . pathInfo . request

getQueryString :: (MonadReader RequestEnv m) => m String
getQueryString = asks $ queryString . request

getServerName :: (MonadReader RequestEnv m) => m String
getServerName = asks $ serverName . request

getServerPort :: (MonadReader RequestEnv m) => m Int
getServerPort = asks $ serverPort . request

getRequestHeaders :: (MonadReader RequestEnv m) => m [(String, String)]
getRequestHeaders = asks $ http . request

getRequestHeader :: (MonadReader RequestEnv m) => String -> m (Maybe String)
getRequestHeader key = lookup key `liftM` getRequestHeaders

getReferer :: (MonadReader RequestEnv m) => m (Maybe String)
getReferer = getRequestHeader "Referer"

getUrlScheme :: (MonadReader RequestEnv m) => m Hack_UrlScheme
getUrlScheme = asks $ hackUrlScheme . request

getRequestBody :: (MonadReader RequestEnv m) => m ByteString
getRequestBody = asks $ hackInput . request

lookupParam :: HasState m => String -> m (Maybe String)
lookupParam key = M.lookup key `liftM` getParams

getParam :: HasState m => String -> m String
getParam key = M.findWithDefault "" key `liftM` getParams

readParam :: (HasState m, Read v) => String -> m (Maybe v)
readParam = liftM (>>= maybeRead) . lookupParam

getParams :: HasState m => m (M.Map String String)
getParams = extractParams `liftM` asks request

extractParams :: Env -> M.Map String String
extractParams e = M.fromList getReq `M.union` M.fromList postReq
  where
    getReq  = fromParamString $ queryString e
    postReq = fromParamString $ toString $ hackInput e
