{-# LANGUAGE TypeFamilies #-}
module Hawk.View.JsonView
  ( JsonView (..)
  , jsonView
  ) where

import Data.ByteString.Lazy.Internal (ByteString)
import Hawk.Controller.Types
  ( StateController
  , View (..)
  )
--import Text.Show.Bytestring (Show (..), show)


data JsonView a = JsonView {toJson :: a -> StateController ByteString}

jsonView :: Show a => JsonView a
jsonView = undefined -- JsonView $ return . show

instance View (JsonView a) where
  type Target (JsonView a) = a
  render jv = toJson jv
