module Main where

import Config.Config

import Hawk.Controller.Initializer (getApplication)

import Hack.Handler.SimpleServer (run)

main :: IO ()
main = run 3000 $ getApplication development configuration
