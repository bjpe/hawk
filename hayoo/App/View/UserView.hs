{-# LANGUAGE TemplateHaskell #-}
module App.View.UserView where

import Hawk.Controller
import Hawk.View.Template.DataType

import App.View.Util

$(viewDataType "User" "index")
-- $(viewDataType "User" "register")

indexXhtml :: a -> StateController UserIndex
indexXhtml _ = defaultUserPage

registerXhtml :: a -> StateController UserIndex
registerXhtml _ = defaultUserPage

defaultUserPage :: StateController UserIndex
defaultUserPage = do
  loginT <- showLogin
  settingsT <- showSettings
  return UserIndex
    { title = pageTitle
    , mystatus = statusDefaultText
    , login = loginT
    , settings = settingsT
    , querytext = mkQueryText ""
    }

