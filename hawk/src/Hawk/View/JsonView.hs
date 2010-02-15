-- --------------------------------------------------------------------------
{- |
   Module      :  $Header$
   Copyright   :  Copyright (C) 2010 Alexander Treptow
   License     :  BSD3

   Maintainer  :  {inf6866}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  

   
-}
-- --------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
module Hawk.View.JsonView
  ( JsonView (..)
  , jsonView
  , jsonDecode
  , jsonEncode
  , JSON (..)
  , jObject
  , jArray
  , jString
  , jXml
  , jNumber
  , jInt
  , jInteger
  , jDouble
  , jBool
  , jNull
  ) where

import Hawk.Controller.Types
  ( StateController
  , View (..)
  )
import Hawk.View.Template.HtmlHelper (XmlTrees)

--import Data.ByteString.UTF8 ( ByteString )
import Data.ByteString.Lazy.Internal ( ByteString )
import qualified Data.ByteString.UTF8 as U
import Data.Ratio

import Data.Trie

import Text.JSONb.Simple ( JSON (..) )
import Text.JSONb.Decode ( decode )
import Text.JSONb.Encode ( encode, Style (..) )

data JsonView a = JsonView {toJson :: a -> StateController ByteString}

jsonView :: (a -> StateController ByteString) -> JsonView a
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

jObject :: [(String, JSON)] -> JSON
jObject [] = Object empty
jObject l = Object $ fromList $ ol l
  where 
  ol :: [(String, JSON)] -> [(KeyString, JSON)]
  ol [] = []
  ol ((s,j):xs) = (U.fromString s, j) : (ol xs)

jArray :: [JSON] -> JSON
jArray = Array

jString :: String -> JSON
jString s = String $ U.fromString s

jXml :: XmlTrees -> JSON
jXml x = jString $ show x

jNumber :: Rational -> JSON
jNumber = Number

jInt :: Int -> JSON
jInt i = jNumber $ (toInteger i) % 1

jInteger :: Integer -> JSON
jInteger i = jNumber (i % 1)

jDouble :: Double -> JSON
jDouble d = jNumber $ approxRational d 0.000001

jBool :: Bool -> JSON
jBool = Boolean

jNull :: JSON
jNull = Null
