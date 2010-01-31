{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances, TypeSynonymInstances #-}
module Hawk.View.Template.ToXhtml where

import Hawk.View.Template.HtmlHelper
import Data.Stringable

class ToXhtml a where
    toXhtml :: a -> [XmlTree]

instance Stringable a => ToXhtml a where
    toXhtml = toXhtml . text . toString

instance ToXhtml XmlTree where
    toXhtml = (:[])

instance ToXhtml [XmlTree] where
    toXhtml = id
