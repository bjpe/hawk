module App.Controller.AjaxController where

import App.View.AjaxView
import App.HolumbusWrapper.Types
import App.HolumbusWrapper.QuerySettingsHelper
import App.HolumbusWrapper.HolumbusWrapper

import Config.Types

import Hawk.Controller
import Hawk.View

import Holumbus.Query.Result

import Control.Monad.Reader (asks)

routes :: [Routing]
routes = 
  [ ("index", indexAction >>= render (jsonView indexJson))
  , ("search", searchAction >>= render (jsonView searchJson))
  ]

indexAction :: StateController JSON
indexAction = return $ jObject [("status", jString "here i am")]

searchAction :: StateController (HayooResult, QueryInfo)
searchAction = do
  appCfg <- asks appConfiguration
  cfg <- appCfg
  q <- lookupParam "q"
  o <- getParam "o"
  qs <- getQuerySettings
  case q of
    Nothing -> return ((emptyResult, "No query to parse"), createQuery cfg "" qs $ toInt o)
    Just v  -> let qi = createQuery cfg v qs $ toInt o
               in return $ (query qi, qi)

