module App.View.FAQ where

import App.View.Common

-- ------------------------------------------------------------------------------
-- Getting Started
-- ------------------------------------------------------------------------------
faqL :: XmlTrees
faqL = (lHead "FAQ") 
      ++ (lList [("Frequently Asked", "faq")] "")
      ++ (lList
          [ ("Install GHC", "installghc")
          , ("Install Libs", "installlibs")
          , ("Install Hawk", "installhawk")
          , ("Run Example", "runexample")
          ] "1")
      ++ (lList
          [ ("Project Structure", "structure")
          , ("First Steps", "steps")
          ] "")
      ++ (lList
          [ ("Main Application", "mainapp")
          , ("Cabal Package", "cabal")
          , ("Configuration", "config")
          , ("Routing", "routing")
          , ("Controller", "controller")
          , ("Model", "model")
          , ("View", "view")
          , ("Templates", "templates")
          , ("Start it!", "start")
          ] "1")
      ++ (lList [("FAQ", "/index/faq")] "")

faqC :: XmlTrees
faqC = [ cHead "Frequently Asked Questions (FAQ)" "faq"
       , cBodyS (  "was is hawk?"
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
               )
       ]

