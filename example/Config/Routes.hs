module Config.Routes where

import qualified App.View.CustomerView as CV
import Hawk.Controller.Types (Controller)
import qualified Data.Map as M

routing :: M.Map String (M.Map String Controller)
routing = M.singleton "customer" $ M.fromList CV.routes
