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
      ++ [textlink "/user/delete" "Delete Account"]
      )
     )
    ]]

formElement :: String -> XmlTree -> XmlTree
formElement labl input = divX 
    [contentTag "span" [("id", labl), ("class","formname")] [text (labl ++ " :")],
     spanClass "formfield" [input]
    ]

formElementT :: String -> XmlTree -> XmlTree
formElementT labl input = divX
    [contentTag "div" [("id", labl), ("class","formnameT")] [text (labl ++ " :")],
     divClass "formfieldT" [input]
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
   ,formElement "Fuzzy search" (checkbox "replace" "" getReplace [])
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
   ,formElementT "Only this Modules" (textarea "modules" getModules [("cols", "40")]) -- if "only this" is not empty, the disallowed modules/packages will be ignored
   ,formElementT "Only this Packages" (textarea "packages" getPackages [("cols", "40")])
   ]
   where getCaseSensitive = maybe False (\v -> maybe False id $ U.caseSensitive v) u
         getOptimizeQuery = maybe True U.optimizeQuery u
         getWordLimit = maybe "0" (show . U.wordLimit) u
         getReplace = maybe False U.replace u
         getSwapChars = maybe False U.swapChars u
         getMaxFuzzy = maybe "1.0" (fuzzShow . U.maxFuzzy) u
           where fuzzShow f | f < 0.15 = "0.1" | f < 0.25 = "0.2" | f < 0.35 = "0.3"
                            | f < 0.45 = "0.4" | f < 0.55 = "0.5" | f < 0.65 = "0.6"
                            | f < 0.75 = "0.7" | f < 0.85 = "0.8" | f < 0.95 = "0.9"
                            | otherwise = "1.0"
         getReplacements = maybe "" (\v -> maybe "" id $ U.replacements v) u
         getModules = maybe "" (\v -> maybe "" id $ U.modules v) u
         getPackages = maybe "" (\v -> maybe "" id $ U.packages v) u

formButton :: String -> String -> [(String, String)] -> XmlTree
formButton name value attrs = submitWithName name value attrs'
  where attrs' = ("class","formButton") : attrs

mkQueryText :: String -> XmlTrees
mkQueryText q = [textfield "q" q [("autocomplete","off"),("onkeyup","offsetQuery(0)")]]


