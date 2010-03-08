module App.View.AjaxView where

import Hawk.Controller
import Hawk.View

--import Holumbus.Query.Result

{-import qualified App.HolumbusWrapper.Types as T
import App.HolumbusWrapper.HolumbusWrapper
-}
import App.View.Util
import App.HolWrapper

--import Data.ByteString.UTF8 (ByteString)

indexJson :: JSON -> StateController JSON
indexJson = return

searchJson :: SearchResult -> StateController JSON
searchJson (Left s) = return $ jObject
  [ ("q", jString "")
  , ("offset", jInt 0)
  , ("status", jString s)
  , ("cloud", jString "")
  , ("documents", jString "")
  , ("pages", jString "")
  , ("toppm", jString "")
  ]
searchJson (Right r) = return $ jObject
  [ ("q", jString $ getSearchString r)
  , ("offset", jInt $ getOffset r)
  , ("status", jXml $ formatStatus r)
  , ("cloud", jXml $ formatCloud r)
  , ("documents", jXml $ formatOffsetList r)
  , ("pages", jXml $ formatPages r)
  , ("toppm", jXml $ formatPM r)
  ]

{-searchJson :: (T.HayooResult, T.QueryInfo) -> StateController JSON
searchJson ((r, e), qi) = 
  let o = T.offset qi
      q = T.queryString qi
      c = T.cache qi
  in return $
  jObject 
    [ ("q", jString q)
    , ("offset", jInt o)
    , ("status", if e == "" then jXml (formatStatus r) else jString e)
    , ("cloud", jXml (formatCloud r))
    , ("documents", jXml (formatOffsetList r o c))
    , ("pages", jXml (formatPages r o q))
    , ("toppm", jXml ((test o q (maxScoreWordHits r)) : (formatPM r)))
    ]
-}
{-test :: Int -> String -> Float -> XmlTree
test i s f = text ((show i) ++ s ++ (show f))-}
