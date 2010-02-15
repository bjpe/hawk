module App.View.AjaxView where

import Hawk.Controller
import Hawk.View

import Holumbus.Query.Result

import qualified App.HolumbusWrapper.Types as T
import App.HolumbusWrapper.HolumbusWrapper

import App.View.Util

--import Data.ByteString.UTF8 (ByteString)

indexJson :: JSON -> StateController ByteString
indexJson s = return $ jsonEncode s

searchJson :: (Result T.FunctionInfo, T.QueryInfo) -> StateController ByteString
searchJson (r, qi) = 
  let o = T.offset qi
  in return $ jsonEncode $
  jObject 
    [ ("q", jString $ T.queryString qi)
    , ("offset", jInt o)
    , ("status", jXml (formatStatus r))
    , ("cloud", jXml (formatCloud r))
    , ("documents", jXml (formatOffsetList r o))
    , ("pages", jXml (formatPages r o))
    , ("toppm", jXml (formatPM r))
    ]
