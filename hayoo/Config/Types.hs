module Config.Types 
  ( module App.HolWrapper --AppConfig (..)
  )where

import App.HolWrapper (loadHayooConfig, HayooConfig)

import Hawk.Controller.Types (AppConfiguration (..))

--data AppConfig = AppConfig HayooConfig
{-
instance AppConfiguration HayooConfig where --AppConfig where
--  getInstance :: (AppConfiguration a, MonadIO m) => m a
  getInstance = loadHayooConfig -- >>= (return . AppConfig)
-}
