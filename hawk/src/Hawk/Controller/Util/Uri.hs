module Hawk.Controller.Util.Uri
  ( escapeUri
  , unescapeUri
  , fromParamString
  , toParamString
  ) where

import Data.List (intercalate)
import Network.URI
    ( escapeURIString
    , isAllowedInURI
    , unEscapeString
    )
import Hawk.Controller.Util.Text 
  ( splitWhere
  , splitAll
  )

escapeUri :: String -> String
escapeUri = escapeURIString isAllowedInURI

unescapeUri :: String -> String
unescapeUri = unEscapeString . unescapeSpace

unescapeSpace :: String -> String
unescapeSpace = map (\c -> if c == '+' then ' ' else c)

fromParamString :: String -> [(String, String)]
fromParamString = map (unescapePair . splitWhere (== '=')) . splitAll (== '&')

unescapePair :: (String, String) -> (String, String)
unescapePair (a, b) = (unescapeUri a, unescapeUri b)

toParamString :: [(String, String)] -> String
toParamString [] = []
toParamString ls = '?' : intercalate "&" (map ((\(a, b) -> a ++ '=' : b) . escapePair) ls)

escapePair :: (String, String) -> (String, String)
escapePair (a, b) = (escapeUri a, escapeUri b)
