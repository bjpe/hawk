module App.Helper.Format where

import qualified Text.XML.HXT.DOM.XmlNode as XN

formatCompleteness :: XN.XmlNode a => Double -> a
formatCompleteness = XN.mkText . show . (round:: Double -> Integer)
