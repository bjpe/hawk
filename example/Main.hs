module Main (main) where

import Config.Config
import Hawk.Controller.Initializer (getApplication)
import Hawk.Controller.Types (AppConfiguration (..))
import Hack.Handler.SimpleServer as Server (run)

main :: IO ()
main = run 3000 $ getApplication getInstance development configuration
