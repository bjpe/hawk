module App.View.HtmlCommon where

import Hawk.View.Template.HtmlHelper

divX :: XmlTrees -> XmlTree
divX = contentTag "div" []

divId :: String -> XmlTrees -> XmlTree
divId attr = contentTag "div" [("id",attr)]

divClass :: String -> XmlTrees -> XmlTree
divClass attr = contentTag "div" [("class", attr)]

span :: XmlTrees -> XmlTree
span = contentTag "span" []

spanId :: String -> XmlTrees -> XmlTree
spanId attr = contentTag "span" [("id", attr)]

spanClass :: String -> XmlTrees -> XmlTree
spanClass attr = contentTag "span" [("class", attr)]

linkId :: String -> String -> XmlTrees -> XmlTree
linkId href attr = linkA href [("id", attr)]

linkClass :: String -> String -> XmlTrees -> XmlTree
linkClass href attr = linkA href [("class", attr)]

tdClass :: String -> XmlTrees -> XmlTree
tdClass attr = contentTag "td" [("class", attr)]

trClass :: String -> XmlTrees -> XmlTree
trClass attr = contentTag "tr" [("class", attr)]

