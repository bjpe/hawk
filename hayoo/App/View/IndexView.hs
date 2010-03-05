{-# LANGUAGE TemplateHaskell #-}
module App.View.IndexView where

import Hawk.Controller
import Hawk.View.Template.DataType
import qualified Hawk.View.Template.HtmlHelper as H

import Holumbus.Query.Result (Result (..))

import qualified App.HolumbusWrapper.Types as T
import App.HolumbusWrapper.HolumbusWrapper
import App.View.Util



$(viewDataType "Index" "index")
$(viewDataTypeWithPrefix "Search" "Index" "search")
$(viewDataTypeWithPrefix "Config" "Index" "config")

-- version with data type
indexXhtml :: String -> StateController IndexIndex
indexXhtml = defaultIndexPage

searchXhtml :: (T.HayooResult, T.QueryInfo, String) -> StateController IndexSearch
searchXhtml ((r, e), qi, user) =
  let o = T.offset qi
      q = T.queryString qi
      c = T.cache qi
  in return IndexSearch
    { searchTitle = pageTitle
    , searchMystatus = if null e then formatStatus r else [H.text e]
    , searchCloud = formatCloud r
    , searchList = formatOffsetList r o c
    , searchLogin = showLogin user
    , searchSettings = showSettings user
    , searchQuerytext = mkQueryText q
    , searchToppm = formatPM r
    , searchPages = formatPages r o q
    } 

configXhtml :: (String, String) -> StateController IndexConfig
configXhtml (q, user) =
  return IndexConfig
    { configTitle = pageTitle
    , configMystatus = statusDefaultText
    , configLogin = showLogin user
    , configSettings = showSettings user
    , configForm = singleRequestConfig q
    , configQuerytext = mkQueryText q
    }

helpXhtml :: String -> StateController IndexIndex
helpXhtml = defaultIndexPage

aboutXhtml :: String -> StateController IndexIndex
aboutXhtml = defaultIndexPage

defaultIndexPage :: String -> StateController IndexIndex
defaultIndexPage user = 
  return IndexIndex
    { title = pageTitle
    , mystatus = statusDefaultText
    , login = showLogin user
    , settings = showSettings user
    , querytext = mkQueryText ""
    }

