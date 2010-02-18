module App.View.AjaxView where

import Hawk.Controller
import Hawk.View

import Holumbus.Query.Result

import qualified App.HolumbusWrapper.Types as T
import App.HolumbusWrapper.HolumbusWrapper

import App.View.Util

--import Data.ByteString.UTF8 (ByteString)

indexJson :: JSON -> StateController JSON
indexJson = return

searchJson :: (Result T.FunctionInfo, T.QueryInfo) -> StateController JSON
searchJson (r, qi) = 
  let o = T.offset qi
      q = T.queryString qi
  in return $
  jObject 
    [ ("q", jString q)
    , ("offset", jInt o)
    , ("status", jXml (formatStatus r))
    , ("cloud", jXml (formatCloud r))
    , ("documents", jXml (formatOffsetList r o))
    , ("pages", jXml (formatPages r o q))
    , ("toppm", jXml ((test o q (maxScoreWordHits r)) : (formatPM r)))
    ]

test :: Int -> String -> Float -> XmlTree
test i s f = text ((show i) ++ s ++ (show f))
