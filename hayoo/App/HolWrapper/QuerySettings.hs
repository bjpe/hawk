module App.HolWrapper.QuerySettings
  ( mkQuerySettings
  ) where

import App.HolWrapper.Types (QuerySettings (..))

import qualified App.Model.User as U

import Hawk.Controller

import Holumbus.Query.Fuzzy (FuzzyConfig (..))
import Holumbus.Query.Process (ProcessConfig (..))

import qualified Data.List as L
import qualified Data.Map as M

-- | Create QuerySettings date from Request and\or Storage
-- / (to be customized)
mkQuerySettings :: String -> StateController QuerySettings
mkQuerySettings q = do
  o <- getParam "o"
  return $ querySettingsByRequest [q,o]

-- ## private

querySettingsByRequest :: [String] -> StateController QuerySettings
querySettingsByRequest l = do
  m <- getParams
  if not isRequestConfig
    then querySettingsBySession l
    else return $ listToQuerySettings $ l ++ tLL 
      where tLL = toLookupList m settingElems
  where isRequestConfig = toBool $ M.findWithDefault "" "singleConfig" m

querySettingsBySession :: [String] -> StateController QuerySettings
querySettingsBySession l = 
  username <- isAuthedAs
  if null username
    then querySettingsByDefault l
    else do
      qrs <- getQuerySettingsFromSession settingElems
      let qs = filter (not . null) qrs
      if settingLength == length qs
        then return $ listToQuerySettings $ l ++ qs
        else querySettingsByDatabase l

querySettingsByDatabase :: [String] -> StateController QuerySettings
querySettingsByDatabase l = do
  username <- isAuthedAs
  if null username
    then querySettingsByDefault l
    else do
      user <- U.getUserByName username
      return $ listToQuerySettings $ l ++ (U.userToQSList user)

querySettingsByDefault :: [String] -> StateController QuerySettings
querySettingsByDefault (q:o:_) = QuerySettings q o defaultProcessConfig [] []

getQuerySettingsFromSession :: [String] -> StateController [String]
getQuerySettingsFromSession = mapM f
  where f x = do 
           v <- getSessionValue x
           return $ maybe [] id v

toLookupList :: M.Map String String -> [String] -> [String]
toLookupList m = L.map (\s -> M.findWithDefault "" s m) 

listToQuerySettings :: [String] -> QuerySettings
listToQuerySettings (q:o:c:oq:w:r:s:f:re:m:p:_) = QuerySettings
  { searchString  = q
  , offset        = toInt o
  , processConfig = ProcessConfig 
    { fuzzyConfig   = FuzzyConfig
      { applyReplacements  = toBool r
      , applySwappings     = toBool s
      , maxFuzzynes        = toFloat f
      , customReplacements = toReplacements re
      }
    , optimizeQuery = toBool oq
    , wordLimit     = toInt w
    }
  , modules       = toRConfig m
  , packages      = toRConfig p
  }

