module App.HolWrapper.QueryInfo
  ( mkQueryInfo
  ) where

import App.HolWrapper.QuerySettings (mkQuerySettings)
import App.HolWrapper.Types

import Config.Types (AppConfig (..))

import Hawk.Controller

import Control.Monad.Reader (asks)

-- | Creates a QueryInfo date from AppConfiguration and QuerySettings
-- / (to be customized)
mkQueryInfo :: StateController (Maybe QueryInfo)
mkQueryInfo = do
  acfg <- asks appConfiguration
  q <- lookupParam "q"
  case q of
    Nothing -> return Nothing
    Just qr -> do
      qs <- mkQuerySettings qr
      return $ Just $ QueryInfo 
      { querySettings = qs
      , index         = hayooIndexHandler cfg
      , documents     = hayooDocsHandler cfg
      , cache         = hayooCacheHandler cfg
      }
