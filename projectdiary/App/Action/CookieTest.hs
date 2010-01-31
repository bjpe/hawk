-- --------------------------------------------------------------------------
{- |
   Module      :  App.Controller.CookieTest
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Main.hs 57 2009-05-29 11:33:59Z inf6254 $

   Controller for testing the cookie functionality.
-}
-- --------------------------------------------------------------------------
module App.Action.CookieTest where

import Hawk.Controller
import Hawk.View.TextView

controllers :: [Routing]
controllers =  [ ("create"   , createAction >>= render createView)
               , ("increment", incrementAction >>= render incrementView)
               , ("delete"   , deleteAction >>= render deleteView)
               ]

createAction :: StateController Int
createAction = set 0

createView :: TextView Int
createView = textView

incrementAction :: StateController Int
incrementAction = getCookieValue "counter" >>= maybe (set 0) (set . (+1) . read)

incrementView :: TextView Int
incrementView = TextView $ return . (++) "value=" . show

deleteAction :: StateController String
deleteAction = deleteCookie "counter" >> return "deleted"

deleteView :: TextView String
deleteView = textView

set :: HasState m => Int -> m Int
set value = setCookieValue "counter" (show value) >> return value
