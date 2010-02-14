module Config.Routes where

import qualified App.Controller.IndexController as IC
import qualified App.Controller.UserController as UC
import qualified App.Controller.AjaxController as AC

import Hawk.Controller.Types (Controller)
import qualified Data.Map as M

routing :: M.Map String (M.Map String Controller)
routing =         M.singleton "hayoo" (M.fromList IC.routes)
        `M.union` M.singleton "index" (M.fromList IC.routes)
        `M.union` M.singleton "user"  (M.fromList UC.routes)
        `M.union` M.singleton "ajax"  (M.fromList AC.routes)
