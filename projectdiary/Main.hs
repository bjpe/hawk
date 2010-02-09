module Main where

import Config.Config

import Hawk.Controller.Initializer (getApplication)

import Hack.Handler.SimpleServer (run)
import Hawk.Controller.Types (AppConfiguration (..))

main :: IO ()
main = run 3000 $ getApplication getInstance development configuration
