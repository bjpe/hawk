{-# LANGUAGE TemplateHaskell #-}
module App.Controller.IndexController where

import App.View.IndexView
import App.HolumbusWrapper.HolumbusWrapper
import qualified App.HolumbusWrapper.Types as T

import Config.Types

import Holumbus.Query.Result

import Hawk.Controller
import Hawk.View

import qualified Data.Map as M
import Control.Monad.Reader


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
  case q of
    Nothing -> return ()
    Just v  -> redirectWithParams "index" "search"

searchAction :: StateController (Result T.FunctionInfo, String)
searchAction = do 
  appCfg <- asks appConfiguration
  cfg <- appCfg
  q <- lookupParam "q"
  case q of
    Nothing -> redirectToAction "index" "index"
    Just v  -> return (doThat cfg v, v)

showConfigAction :: StateController String
showConfigAction = getParam "q"

-- ### local ############
-- TODO also handle optional query configuration
doThat :: AppConfig -> String -> Result T.FunctionInfo
doThat cfg q = query $ createQuery cfg q

createQuery :: AppConfig -> String -> T.QueryInfo
createQuery cfg q = T.QueryInfo
  { T.queryString   = q
  , T.querySettings = getQuerySettings
  , T.index         = hayooIndexHandler cfg
  , T.documents     = hayooDocsHandler cfg
  }

