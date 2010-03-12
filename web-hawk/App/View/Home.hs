module App.View.Home where

import App.View.Common

-- ------------------------------------------------------------------------------
-- Home / Index
-- ------------------------------------------------------------------------------
homeL :: XmlTrees
homeL = (lHead "Overview") ++ (lList [ ("What is Hawk?","about")
                                     , ("How to start?","start")
                                     , ("Hayoo!","hayoo")
                                     ] "")

homeC :: XmlTrees
homeC = [ cHead "What is Hawk?" "about"
        , cBodyS "Hawk is a Web-Application Framework written in the pure functional programming language Haskell. It was developed for easy and fast to write Web-Applications. Haskell is system-intependent and has strict typing, for that a application that compiles will work correct in most cases. Compiled code is much faster than scripted code."
        , cHead "How to start?" "start"
        , cBodyS "Spend 20 minutes to the \"Getting Started\" section and learn how to develop a small guestbook application with Hawk."
        , cHead "Hayoo! Haskell API Search" "hayoo"
        , text "this is hawk, about this project, honours to prof., hxt, haskell, fh-wedel .. beispiel hayoo .. write your first hawk application, getting started with guestbook"]

homeR :: XmlTrees
homeR = [ rHead "Test"
        , rBody "here are some great news"
        ]

