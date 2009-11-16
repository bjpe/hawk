{-# LANGUAGE TypeFamilies #-}
module Hawk.View.TextView
  ( TextView (..)
  , textView
  ) where

import Control.Monad (liftM)
import Data.ByteString.Lazy.UTF8 (fromString)
import Hawk.Controller.Types
  ( StateController
  , View (..)
  )

data TextView a = TextView { toText :: a -> StateController String }

textView :: Show a => TextView a
textView = TextView $ return . show

instance View (TextView a) where
  type Target (TextView a) = a
  render tv = liftM fromString . toText tv
