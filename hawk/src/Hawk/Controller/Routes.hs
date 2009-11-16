-- --------------------------------------------------------------------------
{- |
   Module      :  Hawk.Core.Routes
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  NONE

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Main.hs 57 2009-05-29 11:33:59Z inf6254 $

   Routing
-}
-- --------------------------------------------------------------------------
module Hawk.Controller.Routes
  ( dispatch
  , simpleRouting
  , addUrlParams
  , urlFor
  , actionUrl
  ) where

import Control.Monad (liftM)
import qualified Data.Map as M
import Hawk.Controller.Request
import Hawk.Controller.Types
import Hawk.Controller.Util.Text
import Hawk.Controller.Util.Uri

defaultAction :: String
defaultAction = "index"

dispatch :: String -> Route
dispatch = withDefault . splitPath

withDefault :: Route -> Route
withDefault (Route c a) = Route c defaultA
  where defaultA = if null a then defaultAction else a

splitPath :: String -> Route
splitPath path = Route _controller _action
  where
    (_controller, _action) = splitWhere (== '/') $ drop 1 path
--    (_action, _)  = splitWhere (== '.') rem1

simpleRouting :: M.Map String (M.Map String Controller) -> Env -> Maybe Controller
simpleRouting m e = M.lookup c m >>= M.lookup a
  where (Route c a) = dispatch $ unescapeUri $ pathInfo e

addUrlParams :: [(String, String)] -> String -> String
addUrlParams ps = (++ toParamString ps)

urlFor :: HasState m => String -> m String
urlFor target = do
  sn <- getScriptName
  return $ escapeUri $ sn ++ target

actionUrl :: HasState m => String -> String -> [(String, String)] -> m String
actionUrl c a ps = addUrlParams ps `liftM` urlFor ('/':c ++ '/':a)