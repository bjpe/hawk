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
  return
    [form "configform" "post" "/index/search" []
     (   [formElement "Search-Term" (textfield "q" query [])]
      ++ (configFormContent [])
      ++ [formButton "search" "Send Request" []]
     )
    ]

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

configFormContent :: [(String, String)] -> XmlTrees
configFormContent l = do
  [ formElement "Case-Sensitive" (checkbox "" "caseSensitive" False [])
   ,formElement "Fuzzy search" (checkbox "" "useFuzzy" False [])
   ,formElement "Optimize Query" (checkbox "" "optimizeQuery" True [])
   ,formElement "Word Limit" (textfield "wordLimit" "20" [])
   ,formElement "Only this Modules" (textarea "onlyModules" "" []) -- if "only this" is not empty, the disallowed modules/packages will be ignored
   ,formElement "Disallowed Modules" (textarea "disallowModules" "" [])
   ,formElement "Only this Packages" (textarea "onlyPackages" "" [])
   ,formElement "Disallowed Packages" (textarea "disallowPackages" "" [])
   ]

formButton name value attrs = submitWithName name value attrs'
  where attrs' = ("class","formButton") : attrs

mkQueryText :: String -> XmlTrees
mkQueryText q = [textfield "q" q [("autocomplete","off")]]

