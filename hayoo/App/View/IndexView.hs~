{-# LANGUAGE TemplateHaskell #-}
module App.View.IndexView where

import Hawk.Controller
import Hawk.View.Template.DataType

import Holumbus.Query.Result

import App.HolumbusWrapper.Types
import App.HolumbusWrapper.HolumbusWrapper

import App.View.Util

$(viewDataType "Index" "index")
$(viewDataTypeWithPrefix "Search" "Index" "search")
$(viewDataTypeWithPrefix "Config" "Index" "config")

-- version with data type
indexXhtml :: a -> StateController IndexIndex
indexXhtml _ = defaultIndexPage

searchXhtml :: (Result FunctionInfo, String) -> StateController IndexSearch
searchXhtml (r,q) = do
  login <- showLogin
  settings <- showSettings
  return IndexSearch
    { searchTitle = pageTitle
    , searchMystatus = formatStatus r q
    , searchCloud = formatCloud r
    , searchList = formatList r
    , searchLogin = login
    , searchSettings = settings
    } 

configXhtml :: a -> StateController IndexConfig
configXhtml _ = do
  login <- showLogin
  settings <- showSettings
  form <- singleRequestConfig
  return IndexConfig
    { configTitle = pageTitle
    , configMystatus = statusDefaultText
    , configLogin = login
    , configSettings = settings
    , configForm = form
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
    }

