{-# LANGUAGE TypeFamilies #-}
module Hawk.View.JsonView
  ( JsonView (..)
  , jsonView
  , jsonDecode
  , jsonEncode
  , JSON (..)
  ) where

import Hawk.Controller.Types
  ( StateController
  , View (..)
  )

import Data.ByteString.UTF8 ( ByteString )
--import Data.ByteString.Lazy.Internal ( ByteString )

import Text.JSONb.Simple ( JSON (..) )
import Text.JSONb.Decode ( decode )
import Text.JSONb.Encode ( encode, Style (..) )

data JsonView a = JsonView {toJson :: a -> StateController ByteString}

jsonView :: Show a => (a -> StateController ByteString) -> JsonView a
jsonView = JsonView

instance View (JsonView a) where
  type Target (JsonView a) = a
  -- :: a -> Target a -> StateController ByteString
  render jv = toJson jv

jsonEncode :: JSON -> ByteString
jsonEncode = encode Compact

--decode :: ByteString -> Either (ParseError, ByteString) JSON
jsonDecode :: ByteString -> (String, JSON)
jsonDecode s = either l r $ decode s
  where l = (\(e,_) -> (e, Null))
        r = (\j -> ("", j))
