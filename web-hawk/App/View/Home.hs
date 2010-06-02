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
        , cBodyS "Spend 20 minutes to the \"Getting Started\" section and learn how to develop a cool guestbook application with Hawk."
        , cHead "Hayoo! Haskell API Search" "hayoo"
        , text ""]

homeR :: XmlTrees
homeR = [ rHead "Now on Hackage"
        , rBody [text "Hawk now is available from Hackage via Cabal or as gzip."]
        , rHead "Download Links"
        , rBody [link "/packages/hawk-latest.tar.gz" [text "Latest Hawk Version"]
                ,link "/packages/create-project.tar.gz" [text "Project Creator"]
                ,link "/packages/example.tar.gz" [text "Example Application"]
                ]
        ]

