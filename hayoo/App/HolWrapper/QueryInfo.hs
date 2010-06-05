module App.HolWrapper.QueryInfo
  ( mkQueryInfo
  ) where

import App.HolWrapper.QuerySettings (mkQuerySettings)
import App.HolWrapper.Types

import Hawk.Controller

import Control.Monad.Reader (asks)

-- | Creates a QueryInfo date from AppConfiguration and QuerySettings
-- / (to be customized)
mkQueryInfo :: StateController (Maybe QueryInfo)
mkQueryInfo = do
  acfg <- asks (appData . configuration)
  cfg <- acfg
  q <- lookupParam "q"
  case q of
    Nothing -> return Nothing
    Just qr -> do
      qs <- mkQuerySettings qr
      return $ Just $ QueryInfo 
        { querySettings = qs
        , index         = indexHandler cfg
        , documents     = docsHandler cfg
        , cache         = cacheHandler cfg
        }
