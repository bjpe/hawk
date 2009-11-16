-- --------------------------------------------------------------------------
{- |
   Module      :  Hawk.Template.Helper.TagHelper
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Main.hs 57 2009-05-29 11:33:59Z inf6254 $

   Helper functions for creating xml tags.
-}
-- --------------------------------------------------------------------------

module Hawk.View.Template.Helper.TagHelper
  ( module Text.XML.HXT.DOM.XmlNode
  , module Text.XML.HXT.DOM.TypeDefs
  , contentTag
  , tag
  , text
  , showtext
  ) where

import Text.XML.HXT.DOM.XmlNode
import Text.XML.HXT.DOM.TypeDefs
import qualified Data.Map as M

text :: String -> XmlTree
text = mkText

showtext :: Show a => a -> XmlTree
showtext = text . show

contentTag :: String -> Attributes -> XmlTrees -> XmlTree
contentTag name attrs children = mkElement qn al children
  where
    qn = mkName name
    al = map (\(an, av) -> mkAttr (mkName an) [mkText av]) $ unique attrs
    unique = M.toList . M.fromList

tag :: String -> Attributes -> XmlTree
tag name attrs = contentTag name attrs []


