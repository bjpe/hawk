-- | 
-- / (this whole module needs to be customized)
module App.HolWrapper.QuerySettings
  ( mkQuerySettings
  ) where

import App.HolWrapper.Types (QuerySettings (..))
import App.HolWrapper.Common

import qualified App.Model.User as U
import qualified App.Controller.UserController as UC

import Hawk.Controller

import Holumbus.Query.Fuzzy (FuzzyConfig (..))
import Holumbus.Query.Processor (ProcessConfig (..))

import qualified Data.List as L
import qualified Data.Map as M

-- | Create QuerySettings date from Request and\or Storage
-- / (to be customized)
mkQuerySettings :: String -> StateController QuerySettings
mkQuerySettings q = do
  o <- getParam "o"
  querySettingsByRequest [q,o]

-- ## private

querySettingsByRequest :: [String] -> StateController QuerySettings
querySettingsByRequest l = do
  m <- getParams
  if not $ toBool $ M.findWithDefault "" "singleConfig" m
    then querySettingsBySession l
    else return $ listToQuerySettings $ l ++ (tLL m)
      where tLL m = toLookupList m settingElems

querySettingsBySession :: [String] -> StateController QuerySettings
querySettingsBySession l = do
  username <- isAuthedAs
  if null $ maybe "" id username
    then querySettingsByDefault l
    else do
      qrs <- getQuerySettingsFromSession settingElems
      let qs = filter (not . null) qrs
      if settingLength == (length qs)
        then return $ listToQuerySettings $ l ++ qs
        else querySettingsByDatabase l

querySettingsByDatabase :: [String] -> StateController QuerySettings
querySettingsByDatabase l = do
  username <- isAuthedAs
  if null $ maybe "" id username
    then querySettingsByDefault l
    else do
      user <- UC.getCurUser
      return $ listToQuerySettings $ l ++ (userToQSList user)

querySettingsByDefault :: [String] -> StateController QuerySettings
querySettingsByDefault (q:o:_) = return $ QuerySettings q (toInt o) False defaultProcessConfig [] []

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
  , caseSensitive = toBool c
  , processConfig = ProcessConfig 
    { fuzzyConfig   = FuzzyConfig
      { applyReplacements  = toBool r
      , applySwappings     = toBool s
      , maxFuzziness       = toFloat f
      , customReplacements = toReplacements re
      }
    , optimizeQuery = toBool oq
    , wordLimit     = toInt w
    }
  , modules       = toRConfig m
  , packages      = toRConfig p
  }

userToQSList :: U.User -> [String]
userToQSList u = filter (not . null) $ L.map (getList $ U.toList u) settingElems
  where getList l s = maybe "" id $ L.lookup s l
