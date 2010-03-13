module App.View.API where

import App.View.Common

apiL :: XmlTrees
apiL = (lHead "Documentation")
    ++ (lList [ ("API", "api")
              , ("FAQ", "faq")
              ] "")

apiC :: XmlTrees
apiC = [ cHead "Documentation" ""
       , cBody [ text ""
               ]
       , cHead "Haddock generated API Documentation" "api"
       , cBody [ text "You can find the generated API Documentation "
               , linkN "" [text "here"]
               , text "."
               ]
       , cHead "Frequently Asked Questions" "faq"
       , cBody [ text "If you want to get more information about a special topic please visit our "
               , link "/index/faq" [text "FAQ"]
               , text " or just "
               , link "mailto:mail@hawk-project.org" [text "mail us"]
               , text "."
               ]
       ]
