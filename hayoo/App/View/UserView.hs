{-# LANGUAGE TemplateHaskell #-}
module App.View.UserView where

import Hawk.Controller
import Hawk.View.Template.DataType
--import Hawk.View.Template.HtmlHelper

import App.View.Util
import App.Model.User

$(viewDataType "User" "index")
$(viewDataTypeWithPrefix "Register" "User" "register")

indexXhtml :: User -> StateController UserIndex
indexXhtml u =
  return UserIndex
    { title = pageTitle
    , mystatus = statusDefaultText
    , login = showLogin $ username u -- user is logged in if he can see this, so only show his name
    , settings = showSettings $ username u   --  -||-
    , querytext = mkQueryText ""
    , content = showUser u
    }

registerXhtml :: a -> StateController UserRegister
registerXhtml _ =
  return UserRegister
    { registerTitle = pageTitle
    , registerMystatus = statusDefaultText
    , registerLogin = showLogin ""
    , registerSettings = showSettings ""
    , registerQuerytext = mkQueryText ""
    }
