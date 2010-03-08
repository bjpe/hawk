module App.Controller.AjaxController where

import App.View.AjaxView
{-import App.HolumbusWrapper.Types
import App.HolumbusWrapper.QuerySettingsHelper
import App.HolumbusWrapper.HolumbusWrapper-}
import App.HolWrapper

-- import Config.Types

import Hawk.Controller
import Hawk.View

-- import Holumbus.Query.Result

-- import Control.Monad.Reader (asks)

routes :: [Routing]
routes = 
  [ ("index", indexAction >>= render (jsonView indexJson))
  , ("search", searchAction >>= render (jsonView searchJson))
  ]

indexAction :: StateController JSON
indexAction = return $ jObject [("status", jString "here i am")]

searchAction :: StateController SearchResult -- (HayooResult, QueryInfo)
searchAction = do
  qi <- mkQueryInfo
  case qi of
    Nothing -> return $ Left "No query to parse." -- ((emptyResult, "No query to parse"), createQuery cfg "" qs $ toInt o)
    Just v -> return $ query v customParser customRanking
{-  appCfg <- asks appConfiguration
  cfg <- appCfg
  q <- lookupParam "q"
  o <- getParam "o"
  qs <- getQuerySettings
  case q of
    Nothing -> return ((emptyResult, "No query to parse"), createQuery cfg "" qs $ toInt o)
    Just v  -> let qi = createQuery cfg v qs $ toInt o
               in return $ (query qi, qi)
-}
