module Config.Routes where

import qualified App.Controller.IndexController as IC

import Hawk.Controller.Types (Controller)
import qualified Data.Map as M

routing :: M.Map String (M.Map String Controller)
routing =  M.singleton "index" (M.fromList IC.routes)

