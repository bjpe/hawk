module Config.Routes where

import qualified App.Controller.IndexController as IC
import qualified App.Controller.UserController as UC
import qualified App.Controller.AjaxController as AC

import Hawk.Controller.Types (Controller)
import Hawk.Controller.Routes (combine)
import qualified Data.Map as M

routing :: M.Map String (M.Map String Controller)
routing = M.fromList $ (combine M.fromList)
        [ ("hayoo", IC.routes)
        , ("index", IC.routes)
        , ("user" , UC.routes)
        , ("ajax" , AC.routes)
        ]
