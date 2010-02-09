module Main (main) where

import Config.Config (configuration, development)

import Hawk.Controller.Initializer (getApplication)
import Hawk.Controller.Types (AppConfiguration (..))

import Hack.Handler.SimpleServer as Server (run)
import Control.Monad.Trans

main :: IO ()
main = run 3000 $ getApplication getInstance development configuration
