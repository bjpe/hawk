module App.View.Util where

import Hawk.View.Template.HtmlHelper
import Hawk.Controller

pageTitle :: XmlTrees
pageTitle = [text "Hayoo! - Haskell API Search"]

statusDefaultText :: XmlTrees
statusDefaultText = [text "Enter some search terms above to start a search."]

showSettings :: StateController XmlTrees
showSettings = do
  a <- isAuthed
  if a
    then return [(contentTag "a" [("href","/user")] [text "Settings"]), (text " |")]
    else return []

showLogin :: StateController XmlTrees
showLogin = do
  a <- getSessionAuth
  case a of
    Nothing -> return 
      ([form "loginform" "post" "/user/login" [] 
        [(textfield "username" "" []), 
         (password "password" "" [("id","password")]),
         (formButton "authbutton" "Login" [])
        ]
       ])
    Just v  -> return 
      ([contentTag "div" 
        [("id","loginform")] 
        [(text "Logged in as "), 
         (contentTag "a" [("href","/user/logout")] [text v])
        ]
       ])

singleRequestConfig :: StateController XmlTrees
singleRequestConfig = do
  query <- getParam "q"
  return [contentTag "div" [("id", "configSearch")]
    [form "configform" "post" "/index/search" []
     (   [formElementT "Search-Term" (textfield "q" query [])]
      ++ (configFormContent [])
      ++ [formButton "search" "Send Request" []]
     )
    ]]

generateConfigForm :: [(String, String)]  -> StateController XmlTrees
generateConfigForm values = do
  return
    [form "configform" "post" "/user/edit" []
     (  configFormContent []
     )
    ]

formElement :: String -> XmlTree -> XmlTree
formElement label elem = do
  contentTag "div" [] 
    [contentTag "span" [("id", label), ("class","formname")] [text (label ++ " :")],
     contentTag "span" [("class","formfield")] [elem]
    ]

formElementT :: String -> XmlTree -> XmlTree
formElementT label elem = do
  contentTag "div" []
    [contentTag "div" [("id", label), ("class","formnameT")] [text (label ++ " :")],
     contentTag "div" [("class","formfieldT")] [elem]
    ]

configFormContent :: [(String, String)] -> XmlTrees
configFormContent l = do
  [ formElement "Case-Sensitive" (checkbox "caseSensitive" "" False [])
   ,formElement "Fuzzy search" (checkbox "useFuzzy" "" False [])
   ,formElement "Swap Charakters" (checkbox "swapChars" "" False [])
   ,formElement "Optimize Query" (checkbox "optimizeQuery" "" True [])
   ,formElement "Max. Fuzzyness" (
      select "maxFuzzy" (
         optionsFromString [("1.0", "1.0"), ("0.9", "0.9"), ("0.8", "0.8"), 
                            ("0.7", "0.7"), ("0.6", "0.6"), ("0.5", "0.5"), 
                            ("0.4", "0.4"), ("0.3", "0.3"), ("0.2", "0.2"), ("0.1", "0.1")]
         ) []
      )
   ,formElement "Custom Replacement" (
      select "replacements" (
         optionsFromString [("",""), ("English", "English"), ("German","German")]
         ) []
      )
   ,formElement "Word Limit" (
      select "wordLimit" (
         optionsFromString [("0", "0"), ("10", "10"), ("20", "20"), ("40", "40"), 
                            ("60", "60"), ("100", "100"), ("200", "200")]
         ) []
      )
   ,formElementT "Only this Modules" (textarea "onlyModules" "" [("cols", "40")]) -- if "only this" is not empty, the disallowed modules/packages will be ignored
   ,formElementT "Only this Packages" (textarea "onlyPackages" "" [("cols", "40")])
   ,hidden "singleConfig" "true" []
   ]

formButton name value attrs = submitWithName name value attrs'
  where attrs' = ("class","formButton") : attrs

mkQueryText :: String -> XmlTrees
mkQueryText q = [textfield "q" q [("autocomplete","off")]]

