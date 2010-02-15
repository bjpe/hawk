{-# LANGUAGE TemplateHaskell #-}
module App.View.IndexView where

import Hawk.Controller
import Hawk.View.Template.DataType

import Holumbus.Query.Result

import qualified App.HolumbusWrapper.Types as T
import App.HolumbusWrapper.HolumbusWrapper

import App.View.Util

$(viewDataType "Index" "index")
$(viewDataTypeWithPrefix "Search" "Index" "search")
$(viewDataTypeWithPrefix "Config" "Index" "config")

-- version with data type
indexXhtml :: a -> StateController IndexIndex
indexXhtml _ = defaultIndexPage

searchXhtml :: (Result T.FunctionInfo, T.QueryInfo) -> StateController IndexSearch
searchXhtml (r, qi) = do
  let o = T.offset qi
      q = T.queryString qi
  login <- showLogin
  settings <- showSettings
  return IndexSearch
    { searchTitle = pageTitle
    , searchMystatus = formatStatus r
    , searchCloud = formatCloud r
    , searchList = formatOffsetList r o
    , searchLogin = login
    , searchSettings = settings
    , searchQuerytext = mkQueryText q
    , searchToppm = formatPM r
    , searchPages = formatPages r o q
    } 

configXhtml :: String -> StateController IndexConfig
configXhtml q = do
  login <- showLogin
  settings <- showSettings
  form <- singleRequestConfig
  return IndexConfig
    { configTitle = pageTitle
    , configMystatus = statusDefaultText
    , configLogin = login
    , configSettings = settings
    , configForm = form
    , configQuerytext = mkQueryText q
    }

helpXhtml :: a -> StateController IndexIndex
helpXhtml _ = defaultIndexPage

aboutXhtml :: a -> StateController IndexIndex
aboutXhtml _ = defaultIndexPage

defaultIndexPage :: StateController IndexIndex
defaultIndexPage = do
  loginT <- showLogin
  settingsT <- showSettings
  return IndexIndex
    { title = pageTitle
    , mystatus = statusDefaultText
    , login = loginT
    , settings = settingsT
    , querytext = mkQueryText ""
    }

