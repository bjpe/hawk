module App.HolumbusWrapper.QuerySettingsHelper 
  ( getQuerySettings
  , createQuery
  , toInt
  , toFloat )
  where

import App.HolumbusWrapper.Types
import App.Model.User

import Config.Types

import Hawk.Controller (HasState, getParams)
import Hawk.Controller.Util.Text

import Holumbus.Query.Fuzzy

import qualified Data.Map as M

createQuery :: AppConfig -> String -> QuerySettings -> Int -> QueryInfo
createQuery cfg q qs o = QueryInfo
  { queryString   = q
  , querySettings = qs
  , offset        = o
  , index         = hayooIndexHandler cfg
  , documents     = hayooDocsHandler cfg
  , cache         = hayooCacheHandler cfg
  }

getQuerySettings :: HasState m => m QuerySettings
getQuerySettings = do 
  p <- getParams
  let r = (tryGetQSfromParams p)
  case r of
    Nothing -> 
      case tryGetQSfromSession of
        Nothing -> 
          case tryGetQSfromDB of
            Nothing -> return defaultQSConfig
            Just qs -> return qs
        Just qs -> return qs
    Just qs -> return qs

tryGetQSfromParams :: M.Map String String -> Maybe QuerySettings
tryGetQSfromParams m = do
  b <- M.lookup "singleConfig" m
  let cs = M.findWithDefault "" "caseSensitive" m
      uf = M.findWithDefault "" "useFuzzy" m
      sc = M.findWithDefault "" "swapChars" m
      mf = M.findWithDefault "" "maxFuzzy" m
      rp = M.findWithDefault "" "replacements" m
      oq = M.findWithDefault "" "optimizeQuery" m
      wl = M.findWithDefault "" "wordLimit" m
      om = M.findWithDefault "" "onlyModules" m
--      dm = M.findWithDefault "" "disallowModules" m
      op = M.findWithDefault "" "onlyPackages" m
--      dp = M.findWithDefault "" "disallowPackages" m
  if (toBool b)
    then Just (toQS [cs,uf,sc,mf,rp,oq,wl,om,op])
    else Nothing
--  case [cs,uf,sc,mf,rp,oq,wl,om,dm,op,dp] of
--    [Just c, Just f, Just s, Just mm, Just r,Just q, Just l, Just m, Just d, Just p, Just dd] ->
--      Just (toQS [c,f,s,mm,r,q,l,m,d,p,dd])
--    _ -> Nothing

tryGetQSfromSession :: Maybe QuerySettings
tryGetQSfromSession = Nothing

tryGetQSfromDB :: Maybe QuerySettings
tryGetQSfromDB = Nothing

toQS :: [String] -> QuerySettings
toQS (l0:l1:l2:l3:l4:l5:l6:l7:l8:_) = --T.defaultQSConfig
  QuerySettings 
    (cbToBool l0)
    (cbToBool l5)
    (toInt  l6)
    (FuzzyConfig 
      (cbToBool l1)
      (cbToBool l2)
      (toFloat l3)
      (toReplacement l4) )
    (strToPMConfig l7)
    (strToPMConfig l8)

toBool :: String -> Bool
toBool "true" = True
toBool _ = False

cbToBool :: String -> Bool
cbToBool "true" = True
cbToBool "on" = True
cbToBool "false" = False
cbToBool "off" = False
cbToBool _ = False

toReplacement :: String -> Replacements
toReplacement "" = []
toReplacement "German" = germanReplacements
toReplacement "English" = englishReplacements

toInt :: String -> Int
toInt "" = 0
toInt s = hd $ reads s
  where hd [] = 0
        hd l = fst $ head l

toFloat :: String -> Float
toFloat "" = 0.0
toFloat s  = hd $ reads s
  where hd [] = 0.0
        hd l = fst $ head l

toPMConfig :: [String] -> [PMConfig]
toPMConfig [] = []
toPMConfig (x:xs) = (toPMConfig' x) : (toPMConfig xs)
  where
  toPMConfig' :: String -> PMConfig
  toPMConfig' ('(':ns) = do
                     let (n, r) = splitWhere (== ',') ns
                         f = toFloat $ init r
                     if f /= 0.0 then PMRank n f else PMName n
  toPMConfig' n = PMName n

pmcfgToList :: [PMConfig] -> [String]
pmcfgToList [] = []
pmcfgToList (x:xs) = toL x : pmcfgToList xs
  where toL (PMName s) = s
        toL (PMRank s _) = s

fromUser :: User -> QuerySettings
fromUser u = QuerySettings 
  (maybe False id $ useCase u)
  (optimizeQuery u)
  (wordLimit u)
  (FuzzyConfig
    (f_replace u)
    (f_swapChars u)
    (realToFrac $ f_max u)
    (maybe [] toReplacement $ f_replacements u)
  )
  (strToPMConfig $ maybe "" id $ modules u)
  (strToPMConfig $ maybe "" id $ packages u)

strToPMConfig :: String -> [PMConfig]
strToPMConfig s = toPMConfig $ splitAll (== ' ') s

pmCfgToStr :: [PMConfig] -> String
pmCfgToStr [] = ""
pmCfgToStr pm = tail $ concat $ map f pm
  where f (PMRank s r) = " (" ++ s ++ "," ++ show r ++ ")"
        f (PMName s) = ' ' : s

