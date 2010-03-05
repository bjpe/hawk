module App.View.Util where

import Hawk.View.Template.HtmlHelper

import qualified App.Model.User as U
import App.View.HtmlCommon

pageTitle :: XmlTrees
pageTitle = [text "Hayoo! - Haskell API Search"]

statusDefaultText :: XmlTrees
statusDefaultText = [text "Enter some search terms above to start a search."]

showSettings :: String -> XmlTrees
showSettings s | null s = [(textlink "/user/register" "Register"), (text " |")]
               | otherwise = [(textlink "/user" "Settings"), (text " |")]

showLogin :: String -> XmlTrees
showLogin [] = [form "loginform" "post" "/user/login" [] 
                [(textfield "username" "Username" []), 
                 (password "password" "Password" [("id","password")]),
                 (formButton "authbutton" "Login" [])
                ]
               ]
showLogin v = [divId "loginform" [(text "Logged in as "), (textlink "/user/logout" v)]]

showUser :: U.User -> XmlTrees
showUser = generateConfigForm

singleRequestConfig :: String -> XmlTrees
singleRequestConfig query =
  [divId "configSearch"
    [form "configform" "post" "/index/search" []
     (   [formElementT "Search-Term" (textfield "q" query [])]
      ++ (configFormContent Nothing)
      ++ [hidden "singleConfig" "true" []]
      ++ [formButton "search" "Send Request" []]
     )
    ]]

generateConfigForm :: U.User -> XmlTrees
generateConfigForm u = 
  [divId "configSearch"
    [(spanClass "configTitle" [text ((U.username u) ++ "'s search configuration settings")])
    ,(form "configform" "post" "/user/edit" [] 
      (  (configFormContent $ Just u)
      ++ [hidden "uid" (show $ U._uid u) []]
      ++ [formButton "edit" "Save Configuration" []] 
      ++ [textlink ("/user/delete?id=" ++ (show $ U._uid u)) "Delete Account"]
      )
     )
    ]]

formElement :: String -> XmlTree -> XmlTree
formElement label elem = divX 
    [contentTag "span" [("id", label), ("class","formname")] [text (label ++ " :")],
     spanClass "formfield" [elem]
    ]

formElementT :: String -> XmlTree -> XmlTree
formElementT label elem = divX
    [contentTag "div" [("id", label), ("class","formnameT")] [text (label ++ " :")],
     divClass "formfieldT" [elem]
    ]

configFormContent :: Maybe U.User -> XmlTrees
configFormContent u = 
  [ formElement "Case-Sensitive" (checkbox "caseSensitive" "" getCaseSensitive [])
   ,formElement "Optimize Query" (checkbox "optimizeQuery" "" getOptimizeQuery [])
   ,formElement "Word Limit" (
      select "wordLimit" (
        optionsWithSelected id id getWordLimit ["0", "5", "10", "20", "40", "60", "100", "200"]
      ) []
    )
   ,formElement "Fuzzy search" (checkbox "useFuzzy" "" getUseFuzzy [])
   ,formElement "Swap Charakters" (checkbox "swapChars" "" getSwapChars [])
   ,formElement "Max. Fuzzyness" (
      select "maxFuzzy" (
        optionsWithSelected id id getMaxFuzzy ["1.0", "0.9", "0.8", "0.7", "0.6", "0.5", "0.4", "0.3", "0.2", "0.1"]
      ) []
    )
   ,formElement "Custom Replacement" (
      select "replacements" (
        optionsWithSelected id id getReplacements ["", "English", "German"]
      ) []
    )
   ,formElementT "Only this Modules" (textarea "onlyModules" getModules [("cols", "40")]) -- if "only this" is not empty, the disallowed modules/packages will be ignored
   ,formElementT "Only this Packages" (textarea "onlyPackages" getPackages [("cols", "40")])
   ]
   where getCaseSensitive = maybe False (\v -> maybe False id $ U.useCase v) u
         getOptimizeQuery = maybe True U.optimizeQuery u
         getWordLimit = maybe "0" (show . U.wordLimit) u
         getUseFuzzy = maybe False U.f_replace u
         getSwapChars = maybe False U.f_swapChars u
         getMaxFuzzy = maybe "1.0" (show . U.f_max) u
         getReplacements = maybe "" (\v -> maybe "" id $ U.f_replacements v) u
         getModules = maybe "" (\v -> maybe "" id $ U.modules v) u
         getPackages = maybe "" (\v -> maybe "" id $ U.packages v) u

formButton name value attrs = submitWithName name value attrs'
  where attrs' = ("class","formButton") : attrs

mkQueryText :: String -> XmlTrees
mkQueryText q = [textfield "q" q [("autocomplete","off"),("onkeyup","offsetQuery(0)")]]


