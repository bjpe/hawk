module App.Controller.AjaxController where

import App.View.AjaxView

import Hawk.Controller
import Hawk.View

import qualified Data.ByteString.UTF8 as U

routes :: [Routing]
routes = 
  [ ("index", indexAction >>= render (jsonView indexJson))
  ]

indexAction :: StateController JSON
indexAction = return $ String bb

bb :: U.ByteString
bb = U.fromString "i m here"
