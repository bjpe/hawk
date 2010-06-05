module Main (main) where

import Config.Config (configuration)

import Hawk.Controller.Initializer (getApplication)

import Hack.Handler.SimpleServer as Server (run)

import Control.Monad (liftM)

main :: IO ()
main = (getApplication `liftM` configuration)
   >>= run 3000
