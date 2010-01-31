module Main (main) where

import Config.Config (configuration, development)
import Config.Types
import Hawk.Controller.Initializer (getApplication)
import Hack.Handler.SimpleServer as Server (run)

import Hawk.Controller.Types (AppConfiguration (..))

main :: IO ()
main = run 3000 $ getApplication getInstance development configuration
