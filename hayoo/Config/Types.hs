module Config.Types where

--import Holumbus.Index.Inverted.OneFile
import Holumbus.Index.Inverted.OneFile (Persistent)
import Holumbus.Index.SmallDocuments
import Holumbus.Index.Common

import App.HolumbusWrapper.Types

import Hawk.Controller.Types (AppConfiguration (..))

import System.IO.Unsafe

import Holumbus.Query.Result
import Holumbus.Query.Processor
import Holumbus.Query.Fuzzy
import Holumbus.Query.Language.Parser
import Holumbus.Query.Language.Grammar
import Holumbus.Index.Common

data AppConfig = AppConfig
  { hayooIndexHandler :: Persistent
  , hayooDocsHandler  :: SmallDocuments FunctionInfo
  , test              :: String 
--  , authConfigHandler :: AuthDBConfig
--  , dbHandler       :: ConnWrapper
  }

-- TODO get rid of unsafePerformIO
loadIndex :: Persistent
loadIndex = unsafePerformIO (loadFromFile "./indexes/hayoo-index.bin")

loadDocs :: SmallDocuments FunctionInfo
loadDocs = unsafePerformIO (loadFromFile "./indexes/docs-small.bin")

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
  getInstance = --do 
    -- load index and document file
    -- index <- loadIndex
    -- docs <- loadDocs
    -- initialize authentification
    -- create result type
    AppConfig 
      { hayooIndexHandler = loadIndex
      , hayooDocsHandler  = loadDocs
      , test = "mein test string"
      }

