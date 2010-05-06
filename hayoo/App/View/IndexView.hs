{-# LANGUAGE TemplateHaskell #-}
module App.View.IndexView where

import Hawk.Controller
import Hawk.View.Template.DataType
import qualified Hawk.View.Template.HtmlHelper as H

--import Holumbus.Query.Result (Result (..))

{-import qualified App.HolumbusWrapper.Types as T
import App.HolumbusWrapper.HolumbusWrapper-}
import App.HolWrapper
import App.View.Util

$(viewDataType "Index" "index")
$(viewDataTypeWithPrefix "Search" "Index" "search")
$(viewDataTypeWithPrefix "Config" "Index" "config")

-- version with data type
indexXhtml :: String -> StateController IndexIndex
indexXhtml = defaultIndexPage

searchXhtml :: (SearchResult, String) -> StateController IndexSearch
searchXhtml (Left r, user) = return IndexSearch
  { searchTitle = pageTitle
  , searchMystatus = [H.text r]
  , searchCloud = [H.text ""]
  , searchList = [H.text ""]
  , searchLogin = showLogin user
  , searchSettings = showSettings user
  , searchQuerytext = mkQueryText ""
  , searchToppm = [H.text ""]
  , searchPages = [H.text ""]
  } 
searchXhtml (Right r, user) = return IndexSearch
  { searchTitle = pageTitle
  , searchMystatus = formatStatus r
  , searchCloud = formatCloud r
  , searchList = formatOffsetList r
  , searchLogin = showLogin user
  , searchSettings = showSettings user
  , searchQuerytext = mkQueryText $ getSearchString r
  , searchToppm = formatPM r
  , searchPages = formatPages r
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

