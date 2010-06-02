module App.View.Common 
  ( module Hawk.View.Template.HtmlHelper
  , titleT
  , mainLCR
  , mainLC
  , lHead
  , lList
  , lList'
  , cHead
  , cHead'
  , cBody
  , cBodyS
  , rHead
  , rBody
  , divId
  , divClass
  , spanClass
  , img
  , anchor
  , linkN
  , linebreak
  , note
  , code
  , code'
  , codei
  , shell
  , shell'
  , filehead
  , pre
  ) where

import Hawk.View.Template.HtmlHelper

titleT :: String
titleT = "Hawk: Haskell Web Application Kit"

-- | Left -> Center -> Right -> Main
mainLCR :: XmlTrees -> XmlTrees -> XmlTrees -> XmlTrees
mainLCR l c r = [ divId "leftlcr" l
                , divId "rightlcr" r
                , divId "mainlcr" c
                ]
mainLC :: XmlTrees -> XmlTrees -> XmlTrees
mainLC l c = [ divId "leftlc" l
             , divId "mainlc" c
             ]

-- ------------------------------------------------------------------------------
-- formatting functions
-- ------------------------------------------------------------------------------
-- | Make a head for a short link list
lHead :: String -> XmlTrees
lHead h = [ divId "lefthead" [text h] ]

-- | Make the short link list from a [(name,url)], String defines the class and therefore the submenu layer
lList :: [(String, String)] -> String -> XmlTrees
lList l i = lList' (map anc l) i
  where anc (x,y) = (x,'#':y)

lList' :: [(String, String)] -> String -> XmlTrees
lList' l i = map sl l
  where sl (x,y) = divClass ("leftlink"++i) [link y [text x]]

cHead :: String -> String -> XmlTree
cHead h a = divClass "contenthead" [anchor a, text h]

cHead' :: String -> String -> XmlTree
cHead' h a = divClass "contenthead1" [anchor a, text h]

cBody :: XmlTrees -> XmlTree
cBody c = divClass "contentbody" c

cBodyS :: String -> XmlTree
cBodyS c = cBody [text c]

rHead :: String -> XmlTree
rHead h = divClass "righthead" [text h]

rBody :: XmlTrees -> XmlTree
rBody c = divClass "rightbody" c

-- ------------------------------------------------------------------------------
-- html functions
-- ------------------------------------------------------------------------------
divId :: String -> XmlTrees -> XmlTree
divId i = contentTag "div" [("id",i)]

divClass :: String -> XmlTrees -> XmlTree
divClass c = contentTag "div" [("class",c)]

spanClass :: String -> XmlTrees -> XmlTree
spanClass c = contentTag "span" [("class",c)]

img :: String -> XmlTree
img s = tag "image" [("class","img"),("src",s)]

linkN :: String -> XmlTrees -> XmlTree
linkN u c = linkA u [("target","_blank")] c

anchor :: String -> XmlTree
anchor name = contentTag "a" [("name",name)] []

linebreak :: XmlTree
linebreak = tag "br" []

note :: String -> XmlTree
note s = divClass "note" [text s]

code :: [String] -> XmlTree
code s = contentTag "pre" [("class","code")] (strToText s)

code' :: String -> XmlTree
code' s = contentTag "pre" [("class","code")] [text s]

codei :: String -> XmlTree
codei s = contentTag "span" [("class","codei")] [text s]

shell :: [String] -> XmlTree
shell s = contentTag "pre" [("class","shell")] (strToText s)

shell' :: String -> XmlTree
shell' s = contentTag "pre" [("class","shell")] [text s]

filehead :: String -> XmlTree
filehead s = contentTag "p" [("class","filehead")] [text s]

pre :: [String] -> XmlTree
pre s = contentTag "pre" [] (strToText s)

strToText :: [String] -> XmlTrees
strToText = map (\x -> text (x++"\n"))
