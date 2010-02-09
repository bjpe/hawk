module App.Controller.IndexController where

import App.View.IndexView

import Hawk.Controller
import Hawk.View

routes :: [Routing]
routes = 
  [ ("index",indexAction >>= render (typedView "index" indexXhtml))
  ]

indexAction :: StateController ()
indexAction = return ()
