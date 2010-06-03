module App.View.API where

import App.View.Common

apiL :: XmlTrees
apiL = (lHead "Documentation")
    ++ (lList [ ("API", "api")
              , ("FAQ", "faq")
              ] "")

apiC :: XmlTrees
apiC = [ cHead "Documentation" ""
       , cBody [ text "Hawk was developed in the "
               , link "/thesis/thesis-hawk.pdf" [text "Master's Thesis of Björn Peemöller and Stefan Roggensack"]
               , text ". The "
               , link "/thesis/thesis-ext-hawk.pdf" [text "Master's Thesis of Alexander Treptow"]
               , text " improved the Framework."
               ]
       , cHead "Haddock generated API Documentation" "api"
       , cBody [ text "You can find the generated API Documentation "
               , linkN "/haddock/index.html" [text "here"]
               , text "."
               ]
       , cHead "Frequently Asked Questions" "faq"
       , cBody [ text "If you want to get more information about a special topic please visit our "
               , link "/index/faq" [text "FAQ"]
               , text " or just "
               , link "mailto:" [text "mail us (comming soon)"]
               , text "."
               ]
       ]
