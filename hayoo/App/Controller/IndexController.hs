--{-# LANGUAGE TemplateHaskell #-}
module App.Controller.IndexController where

import App.View.IndexView
import App.HolWrapper
{-import App.HolumbusWrapper.HolumbusWrapper
import App.HolumbusWrapper.QuerySettingsHelper
import qualified App.HolumbusWrapper.Types as T-}

-- import Config.Types

--import Holumbus.Query.Result

import Hawk.Controller
import Hawk.View
--import Hawk.Controller.Util.Text

--import Control.Monad.Reader (asks)
import Control.Monad (liftM)

{-import qualified System.Log.Logger as Logger
import System.Log.Logger.TH ( deriveLoggers )

$(deriveLoggers "Logger" [Logger.DEBUG])-}

routes :: [Routing]
routes = 
  [ ("index",indexAction >>= render (typedView "index" indexXhtml))
  , ("search",searchAction >>= render (typedView "search" searchXhtml))
  , ("config",showConfigAction >>= render (typedView "config" configXhtml))
  , ("help",getUser >>= render (typedView "help" helpXhtml))
  , ("about",getUser >>= render (typedView "about" aboutXhtml))]

indexAction :: StateController String
indexAction = do
  q <- lookupParam "q"
--  debugM $ "Text"
  case q of
    Nothing -> getUser
    Just _v  -> redirectWithParams "index" "search"

searchAction :: StateController (SearchResult, String) -- (T.HayooResult, T.QueryInfo, String)
searchAction = do 
  qi <- mkQueryInfo
  case qi of
    Nothing -> redirectToAction "index" "index"
    Just v -> do
      u <- getUser
      return (query v customParser customRanking, u)
{-  appCfg <- asks appConfiguration
  cfg <- appCfg
  q <- lookupParam "q"
  o <- getParam "o"
  qs <- getQuerySettings
  user <- getUser
  case q of
    Nothing -> redirectToAction "index" "index"
    Just v  -> let qi = createQuery cfg v qs $ toInt o
               in return $ (query qi, qi, user)
-}
showConfigAction :: StateController (String, String)
showConfigAction = do
  q <- getParam "q"
  u <- getUser
  return (q,u)

getUser :: StateController String
getUser = (maybe "" id) `liftM` isAuthedAs

