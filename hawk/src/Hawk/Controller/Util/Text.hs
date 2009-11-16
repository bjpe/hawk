module Hawk.Controller.Util.Text
    ( firstLower
    , firstUpper
    , first
    , toUnderscore
    , toCamelCase
    , toAllLower
    , splitAll
    , splitWhere
    ) where

import Data.Char (toLower, toUpper, isUpper)

-- | convert the first char to a lower
firstLower :: String -> String
firstLower = first toLower

-- | convert the first char to a upper
firstUpper :: String -> String
firstUpper = first toUpper

-- | Apply a function to the first element of a list
first :: (a -> a) -> [a] -> [a]
first _ []     = []
first f (x:xs) = f x : xs

toUnderscore :: String -> String
toUnderscore [] = []
toUnderscore string@(c:cs)
  | isUpper c = '_' : toUnderscore (firstLower string)
  | otherwise =  c  : toUnderscore cs

toCamelCase :: Char -> String -> String
toCamelCase _ [] = []
toCamelCase sep (c:cs)
  | c == sep  = firstUpper $ toCamelCase sep cs
  | otherwise = c : toCamelCase sep cs

toAllLower :: String -> String
toAllLower = map toLower

splitAll :: (a -> Bool) -> [a] -> [[a]]
splitAll _ [] = []
splitAll p xs = f : splitAll p s
  where (f, s) = splitWhere p xs

splitWhere :: (a -> Bool) -> [a] -> ([a], [a])
splitWhere p xs = (f, drop 1 s)
  where (f, s) = break p xs
