module Config.Types where

import App.HolumbusWrapper.Types

import Hawk.Controller.Types (AppConfiguration (..))

import System.IO.Unsafe
import Control.Monad.Trans

import Holumbus.Query.Result
import Holumbus.Query.Processor
import Holumbus.Query.Fuzzy
import Holumbus.Query.Language.Parser
import Holumbus.Query.Language.Grammar
import Holumbus.Index.Common
import Holumbus.Index.Inverted.OneFile (Persistent)
import Holumbus.Index.SmallDocuments

data AppConfig = AppConfig
  { hayooIndexHandler :: Persistent
  , hayooDocsHandler  :: SmallDocuments FunctionInfo
  }

loadIndex :: MonadIO m => m Persistent
loadIndex = liftIO (loadFromFile "./indexes/hayoo-index.bin")

loadDocs :: MonadIO m => m (SmallDocuments FunctionInfo)
loadDocs = liftIO (loadFromFile "./indexes/docs-small.bin")

{-main :: IO ()
main = do
  print "qry: start"
  i <- loadIndex
  print "qry: index loaded"
  d <- loadDocs
  print "qry: docs loaded"
  case q of
    Left err -> print err >> print (emptyResult :: Result FunctionInfo)
    Right q -> print q >> print (processQuery cfg i d q :: Result FunctionInfo)
  where 
    cfg = ProcessConfig (FuzzyConfig False True 1.0 []) True 50
    q = parseQuery "to"
-}
instance AppConfiguration AppConfig where
--  getInstance :: (AppConfiguration a, MonadIO m) => m a
  getInstance = do
     idx <- loadIndex
     dcs <- loadDocs
     return AppConfig 
      { hayooIndexHandler = idx
      , hayooDocsHandler  = dcs
      }

