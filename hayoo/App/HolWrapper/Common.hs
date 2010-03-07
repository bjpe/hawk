module App.HolWrapper.Common 
  ( 
  )where

import Holumbus.Query.Fuzzy 
  ( FuzzyConfig (..)
  , englishReplacements
  , germanReplacements)
import Holumbus.Query.Process (ProcessConfig (..))

import Text.XML.HXT.DOM.Util (decimalStringToInt, stringToLower)

import qualified Data.List as L
import qualified Data.Map as M

-- | Create a hard coded ProcessConfig
defaultProcessConfig :: ProcessConfig
defaultProcessConfig = ProcessConfig defaultFuzzyConfig True 50

-- | Create a hard coded FuzzyConfig (Note: enabled fuzzy only works with a fitting parser)
defaultFuzzyConfig :: FuzzyConfig
defaultFuzzyConfig = FuzzyConfig True True 1.0 [englishReplacements]

settingElems :: [String]
settingElems = ["caseSensitive", "optimizeQuery", "wordLimit", "replace", 
                "swapChars", "maxFuzzy", "replacements", "modules", "packages"]

settingLength :: Int
settingLength = length . settingElems

-- ## String -> a  functions
toBool :: String -> Bool
toBool "" = False
toBool = toBool' . head . stringToLower
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

toRConfig :: String -> RConfig
toRConfig s = L.map toRConfig' $ L.filter (not . null) $ splitAll (== ' ') s
  where 
    toRConfig' ('(':xs) | f /= 0.0 = Rank (n, f)
                        | otherwise Name n
      where (n, f) = (\(a, b)-> (a, toFloat b)) $ splitWhere (== ',') $ init xs
    toRConfig' xs = Name xs

