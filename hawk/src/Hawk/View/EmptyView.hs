{-# LANGUAGE TypeFamilies #-}
module Hawk.View.EmptyView
  ( EmptyView
  , emptyView
  ) where

import Data.ByteString.Lazy ( empty )
import Hawk.Controller.Types ( View (..) )

data EmptyView a = EmptyView

emptyView :: EmptyView a
emptyView = EmptyView

{-
instance View EmptyView a where
  render _ _ = return empty
-}

instance View (EmptyView a) where
  type Target (EmptyView a) = a
  render _ _ = return empty
