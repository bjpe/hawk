module App.View.FAQ where

import App.View.Common

-- ------------------------------------------------------------------------------
-- Getting Started
-- ------------------------------------------------------------------------------
faqL :: XmlTrees
faqL = (lHead "FAQ") 
    ++ (lList [ ("Installation", "install")
              , ("Overall", "overall")
              , ("Model", "model")
              , ("View", "view")
              , ("Controller", "controller")
              , ("Application", "app")
              ] "")

faqC :: XmlTrees
faqC = [ cHead "Frequently Asked Questions (FAQ)" ""
       , cBody [ text "" ]
       , cHead "Installation" "install"
       , cBody [ text "all about installation" ]
       , cHead "Overall" "overall"
       , cBody [ text "all various about hawk, how requests go thru etc." ]
       , cHead "Model" "model"
       , cBody [ text "all about sql, criteria, config, mapping" ]
       , cHead "View" "view"
       , cBody [ text "all about views" ]
       , cHead "Controller" "controller"
       , cBody [ text "all about controller, routing, flash, config, statecontroller, auth, appconfig" ]
       , cHead "Application" "app"
       , cBody [ text "all about MVC independent, like logging, test cases, exception handling, appconfig" ]
{-       , cBodyS (  "was is hawk?"
                ++ "- struktur"
                ++ "- wie läuft eine typische abfrage ab?"
                ++ "routing bei mehreren controllern"
                ++ "arten von views"
                ++ "- verschiedene arten ajax anfragen zu stellen und json oder xml als antwort zu erhalten"
                ++ "flash messages allgemein"
                ++ "- flash messages not work"
                ++ "configuration"
                ++ "- session handling (no, cookie, db)"
                ++ "- - eigene session handlings schreiben"
                ++ "sql requests (z.b. wie sql inserts mit html oder steuerzeichen?)"
                ++ "criteria"
                ++ "exception handling / eigene exceptions"
                ++ "parameter auslesen aus state-/requestcontroller"
                ++ "wie verwendet man den logger, wie debugt man / hunit tests"
                ++ "wie verwende ich authorization - config möglichkeiten, vernwdungsbeispiele http basic / db"
                ++ "wie verwende ich meine eigene configuration, z.b. laden von indices für suchmaschinen"
                ++ "db mapping - foreign key handling (nur ein prim key pro table mögl.)"
               )-}
       ]

