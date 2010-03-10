{-# LANGUAGE TemplateHaskell #-}
module App.View.IndexView where

import Hawk.Controller
import Hawk.View.Template.DataType
import Hawk.View.Template.HtmlHelper

$(viewDataType "Index" "index")

indexXhtml :: a -> StateController IndexIndex
indexXhtml _ = return IndexIndex
    { title = [text titleT]
    ,  main = mainC homeSL homeC homeN
    }

startXhtml :: a -> StateController IndexIndex
startXhtml _ = return IndexIndex
    { title = [text (titleT ++ " - Getting Started")]
    ,  main = [text "getting startet, link to faq"]
    }

downloadXhtml :: a -> StateController IndexIndex
downloadXhtml _ = return IndexIndex
    { title = [text (titleT ++ " - Download")]
    ,  main = [text "link to hackage, how to install with cabal, download current stable, and current development"]
    }

apiXhtml :: a -> StateController IndexIndex
apiXhtml _ = return IndexIndex
    { title = [text (titleT ++ " - API Documentation")]
    ,  main = [text "link to faq, link to haddock docu"]
    }

-- ------------------------------------------------------------------------------
-- private functions
-- ------------------------------------------------------------------------------
titleT :: String
titleT = "Hawk: Haskell Web Application Kit"

-- | Shortlinks -> Content -> Main
mainC :: XmlTrees -> XmlTrees -> XmlTrees -> XmlTrees
mainC sl c n = [ divId "shortlinks" sl
               , divId "news" n
               , divId "maincontent" c
               ]

homeSL :: XmlTrees
homeSL = (slHead "Overview") ++ (slList [ ("What is Hawk?","about")
                                        , ("How to start?","start")
                                        , ("Hayoo! API Search","hayoo")
                                        ])

homeC :: XmlTrees
homeC = [ cHead "What is Hawk?" "#about"
        , cBody "Hawk is a Web-Application Framework written in the pure functional programming language Haskell. It was developed for easy and fast to write Web-Applications. Haskell is system-intependent and has strict typing, for that a application that compiles will work correct in most cases. Compiled code is much faster than scripted code."
        , cHead "How to start?" "#start"
        , cBody "Spend 20 minutes to the \"Getting Started\" section and learn how to develop a small guestbook application with Hawk."
        , text "this is hawk, about this project, honours to prof., hxt, haskell, fh-wedel .. beispiel hayoo .. write your first hawk application, getting started with guestbook"]

homeN :: XmlTrees
homeN = [ nHead "Test"
        , nBody "here are some great news"
        ]

-- | Make a head for a short link list
slHead :: String -> XmlTrees
slHead h = [ divId "shortlinkhead" [text h] ]

-- | Make the short link list from a [(name,url)]
slList :: [(String, String)] -> XmlTrees
slList = map sl
  where sl (x,y) = divClass "shortlink" [link y [text x]]

cHead :: String -> String -> XmlTree
cHead h a = divClass "contenthead" [anchor a, text h]

cBody :: String -> XmlTree
cBody c = divClass "contentbody" [text c]

nHead :: String -> XmlTree
nHead h = divClass "newshead" [text h]

nBody :: String -> XmlTree
nBody c = divClass "newsbody" [text c]

-- ------------------------------------------------------------------------------
-- html functions
-- ------------------------------------------------------------------------------
divId :: String -> XmlTrees -> XmlTree
divId i = contentTag "div" [("id",i)]

divClass :: String -> XmlTrees -> XmlTree
divClass c = contentTag "div" [("class",c)]

anchor :: String -> XmlTree
anchor name = contentTag "a" [("name",name)] []
