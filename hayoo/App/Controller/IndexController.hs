{-# LANGUAGE TemplateHaskell #-}
module App.Controller.IndexController where

import App.View.IndexView
import App.HolumbusWrapper.HolumbusWrapper
import App.HolumbusWrapper.QuerySettingsHelper
import qualified App.HolumbusWrapper.Types as T

import Config.Types

import Holumbus.Query.Result

import Hawk.Controller
import Hawk.View
import Hawk.Controller.Util.Text

import Control.Monad.Reader ( asks )

-- import qualified System.Log.Logger as Logger
-- import System.Log.Logger.TH ( deriveLoggers )

-- $(deriveLoggers "Logger" [Logger.DEBUG])

routes :: [Routing]
routes = 
  [ ("index",indexAction >>= render (typedView "index" indexXhtml))
  , ("search",searchAction >>= render (typedView "search" searchXhtml))
  , ("config",showConfigAction >>= render (typedView "config" configXhtml))
  , ("help",return () >>= render (typedView "help" helpXhtml))
  , ("about",return () >>= render (typedView "about" aboutXhtml))]

indexAction :: StateController ()
indexAction = do
  q <- lookupParam "q"
--  debugM $ "Text"
  case q of
    Nothing -> return ()
    Just v  -> redirectWithParams "index" "search"

searchAction :: StateController (Result T.FunctionInfo, String, T.QuerySettings)
searchAction = do 
  appCfg <- asks appConfiguration
  cfg <- appCfg
  q <- lookupParam "q"
  qs <- getQuerySettings
  case q of
    Nothing -> redirectToAction "index" "index"
    Just v  -> return (doThat cfg v qs, v, qs)

showConfigAction :: StateController String
showConfigAction = getParam "q"

-- ### local ############
-- TODO also handle optional query configuration
doThat :: AppConfig -> String -> T.QuerySettings -> Result T.FunctionInfo
doThat cfg q qs = query $ createQuery cfg q qs

createQuery :: AppConfig -> String -> T.QuerySettings -> T.QueryInfo
createQuery cfg q qs = T.QueryInfo
  { T.queryString   = q
  , T.querySettings = qs
  , T.index         = hayooIndexHandler cfg
  , T.documents     = hayooDocsHandler cfg
  }

