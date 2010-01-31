module Hawk.Controller.CustomResponses where

import Control.Monad.Either
import Hawk.Controller.Responses
import Hawk.Controller.Request (getParamsAsList)
import Hawk.Controller.Routes (actionUrl)
import Hawk.Controller.Types

customResponse :: Response -> StateController a
customResponse = returnLeft

redirectToUrl :: String -> StateController a
redirectToUrl = customResponse . redirectResponse

redirectTo :: String -> String -> [(String, String)] -> StateController a
redirectTo c a ps = actionUrl c a ps >>= redirectToUrl

redirectToAction :: String -> String -> StateController a
redirectToAction c a = redirectTo c a []

redirectWithParams :: String -> String -> StateController a
redirectWithParams c a = getParamsAsList >>= redirectTo c a
