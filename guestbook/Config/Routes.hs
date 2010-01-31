module Config.Routes (routing) where

import qualified App.Controller.GuestbookController as GC
import Hawk.Controller.Types (Controller)
import qualified Data.Map as M

routing :: M.Map String (M.Map String Controller)
routing = M.singleton "guestbook" $ M.fromList GC.routes

