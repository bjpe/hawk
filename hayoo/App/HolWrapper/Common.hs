module App.HolWrapper.Common 
  ( defaultProcessConfig
  , settingElems
  , settingLength
  , toBool
  , toInt
  , toFloat
  , toReplacements
  , toRConfig
  ) where

import Holumbus.Query.Fuzzy 
import Holumbus.Query.Processor (ProcessConfig (..))

import Hawk.Controller.Util.Text (splitAll, splitWhere)

import App.HolWrapper.Types

import Text.XML.HXT.DOM.Util (decimalStringToInt, stringToLower)

import qualified Data.List as L
import qualified Data.Map as M

-- | Create a hard coded ProcessConfig
defaultProcessConfig :: ProcessConfig
defaultProcessConfig = ProcessConfig defaultFuzzyConfig True 50

-- | Create a hard coded FuzzyConfig (Note: enabled fuzzy only works with a fitting parser)
defaultFuzzyConfig :: FuzzyConfig
defaultFuzzyConfig = FuzzyConfig True True 1.0 englishReplacements

settingElems :: [String]
settingElems = ["caseSensitive", "optimizeQuery", "wordLimit", "replace", 
                "swapChars", "maxFuzzy", "replacements", "modules", "packages"]

settingLength :: Int
settingLength = length $ settingElems

-- ## String -> a  functions
toBool :: String -> Bool
toBool "" = False
toBool s = toBool' . head . stringToLower $ s
  where toBool' c = (c == 't') || (c == 'o') 

toInt :: String -> Int
toInt = decimalStringToInt

toFloat :: String -> Float
toFloat s = encodeFloat (toInteger $ toInt fs) (toInt sc)
  where (fs, sc) = L.break (== '.') s

toReplacements :: String -> Replacements
toReplacements [] = []
toReplacements s | (stringToLower s) == "english" = englishReplacements
                 | (stringToLower s) == "german" = germanReplacements
                 | otherwise = [] -- or customized replacements here

toRConfig :: String -> [RConfig]
toRConfig s = L.map toRConfig' $ L.filter (not . null) $ splitAll (== ' ') s
  where 
    toRConfig' x = toRConfig'' $ splitWhere (== ',') x
     where
       toRConfig'' ([], _) = Name ""
       toRConfig'' (n, []) = Name n
       toRConfig'' (n, f) = Rank (L.tail n, toFloat $ L.init f)

