module Hawk.Model.Util where

import Hawk.Model.Criteria
import Hawk.Model.Criteria.Types (sqlExprPair)
import Hawk.Model.Exception

import Control.Exception (throw)

safeHead :: [x] -> Maybe x
safeHead (x:_) = Just x
safeHead []    = Nothing

unsafeHeadC :: Criteria -> [x] -> x
unsafeHeadC = unsafeHeadMsg . show . sqlExprPair

unsafeHead :: [x] -> x
unsafeHead = unsafeHeadMsg ""

unsafeHeadMsg :: String -> [x] -> x
unsafeHeadMsg _   (x:_) = x
unsafeHeadMsg msg []    = throw $ RecordNotFound msg

typeOf :: p -> p
typeOf _ = undefined

typeOfM :: m p -> p
typeOfM _ = undefined

typeOfM2 :: m (n p) -> p
typeOfM2 _ = undefined
