module App.Controller.IndexController where

import App.View.IndexView

import Hawk.Controller
import Hawk.View

routes :: [Routing]
routes = 
  [ ("index",return () >>= render (typedView "index" indexXhtml))
  , ("start",return () >>= render (typedView "index" startXhtml))
  , ("download",return () >>= render (typedView "index" downloadXhtml))
  , ("api",return () >>= render (typedView "index" apiXhtml))
  , ("faq",return () >>= render (typedView "index" faqXhtml))
  ]

