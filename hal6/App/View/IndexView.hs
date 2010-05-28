{-# LANGUAGE TemplateHaskell #-}
module App.View.IndexView where

import App.Model.User

import Hawk.Controller
import Hawk.View.Template.DataType
import qualified Hawk.View.Template.HtmlHelper as H

import Data.Time
import System.Locale

formatLocale :: UTCTime -> [H.XmlTree]
formatLocale t = [H.text (formatTime defaultTimeLocale "%R %D" t)]

$(viewDataType "Index" "user")
$(viewDataType "Index" "index")

indexXhtml :: ([User], Maybe User) -> StateController IndexIndex
indexXhtml (l,u) = do
  ul <- mapM singleUser l
  return IndexIndex
    { title = [H.text "HaL6 Anmeldung"]
    , regform = [H.contentTag "div" [("id","regdiv")] (formatForm u)]
    , indexUser = ul
    , num = [H.text $ show $ length ul]
    , loginform = [H.contentTag "div" [("id","logindiv")] (formatLogin u)]
    }

singleUser :: User -> StateController IndexUser
singleUser u = 
  return IndexUser
    { uname = [H.text (username u)]
    , udate = created u
    , ustudent = [H.text (formatPart $ student u)]
    , ututorial = [formatState $ tutorial u]
    , uworkshop = [formatState $ workshop u]
    , uparty = [formatState $ party u]
    }

formatPart :: Bool -> String
formatPart True = "Student"
formatPart False = "berufstätig"

formatState :: Bool -> H.XmlTree
formatState True = H.image "ok" "/images/ok.png" []
formatState False = H.image "not checked" "/images/x.png" []

formatLogin :: Maybe User -> H.XmlTrees
formatLogin Nothing = [H.form "loginform" "POST" "/index/login" []
                       [H.textfield "username" "Full Name" []
                       ,H.password "password" "Password" []
                       ,H.submitWithName "submit" "Login" []
                       ]]
formatLogin (Just u) = [H.text ("You are logged in as " ++ (username u) ++ ".")
                     ,H.contentTag "div" [] [H.textlink "/index/logout" "Logout"]
                     ,H.contentTag "div" [] [H.textlink "/index/delete" "Unregister"]]

formatForm :: Maybe User -> H.XmlTrees
formatForm u = [H.form "saveform" "POST" "/index/save" []
                [formElem "Full Name" (H.textfield "username" (maybe "" username u) [])
                ,formElem "Password" (H.password "password" (maybe "" password u) [])
                ,formElem "Student" (H.checkbox "student" "" (maybe False student u) [])
                ,formElem "Attend Tutorials" (H.checkbox "tutorial" "" (maybe False tutorial u) [])
                ,formElem "Attend Workshops" (H.checkbox "workshop" "" (maybe False workshop u) [])
                ,formElem "Attend Party" (H.checkbox "party" "" (maybe False party u) [])
                ,H.submitWithName "submit" (maybe "Attend" (\_ -> "Save") u) []
                ]
               ]
             where formElem label inputf = H.contentTag "div" [] [H.slabel label [], inputf]
