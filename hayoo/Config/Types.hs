module Config.Types where

import App.HolumbusWrapper.Types

import Hawk.Controller.Types (AppConfiguration (..))

import Control.Monad.Trans

import Holumbus.Index.Common
import Holumbus.Index.Inverted.OneFile (Persistent)
import Holumbus.Index.SmallDocuments
import Holumbus.Index.Cache

data AppConfig = AppConfig
  { hayooIndexHandler :: Persistent
  , hayooDocsHandler  :: SmallDocuments FunctionInfo
  , hayooCacheHandler :: Cache
  }

loadIndex :: MonadIO m => m Persistent
loadIndex = liftIO $ loadFromFile "./indexes/hayoo-index.bin"

loadDocs :: MonadIO m => m (SmallDocuments FunctionInfo)
loadDocs = liftIO $ loadFromFile "./indexes/docs-small.bin"

loadCache :: MonadIO m => m Cache
loadCache = liftIO $ createCache "./indexes/cache.db"

instance AppConfiguration AppConfig where
--  getInstance :: (AppConfiguration a, MonadIO m) => m a
  getInstance = do
     idx <- loadIndex
     dcs <- loadDocs
     cah <- loadCache
     return AppConfig 
      { hayooIndexHandler = idx
      , hayooDocsHandler  = dcs
      , hayooCacheHandler = cah
      }

