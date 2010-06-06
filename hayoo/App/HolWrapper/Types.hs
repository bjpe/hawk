{-# LANGUAGE BangPatterns, TemplateHaskell #-}
module App.HolWrapper.Types
  ( SearchResult
  , ResultTuple
  , QueryParser
  , Ranking
  , FunctionInfo (..)
  , QueryInfo (..)
  , QuerySettings (..)
  , RConfig (..)
  
  , HayooConfig (..)
  , loadHayooConfig
  ) where

import Hawk.Controller.Types (AppConfiguration (..))

import Holumbus.Index.Cache (Cache, createCache)
import Holumbus.Index.Common
import Holumbus.Index.Inverted.OneFile (Persistent)
import Holumbus.Index.SmallDocuments (SmallDocuments)
import Holumbus.Query.Language.Grammar (Query)
import Holumbus.Query.Processor (ProcessConfig)
import Holumbus.Query.Ranking (RankConfig)
import Holumbus.Query.Result (Result, Score)

import Data.Binary
import Data.ByteString.UTF8 as B

import Control.Monad.Trans (liftIO, MonadIO)
import Control.Monad (liftM)

import Text.XML.HXT.Arrow

import qualified System.Log.Logger as Logger
import System.Log.Logger.TH ( deriveLoggers )

$(deriveLoggers "Logger" [Logger.DEBUG])

type SearchResult = Either String ResultTuple
type ResultTuple = (Result FunctionInfo, QueryInfo)

type QueryParser = QuerySettings -> Either String Query

type Ranking = QuerySettings -> Maybe (RankConfig FunctionInfo)

-- | Additional information about a function.
--   Your custom document information from the search-engine
data FunctionInfo = FunctionInfo 
  { moduleName :: ByteString       -- ^ The name of the module containing the function, e.g. Data.Map
  , signature  :: ByteString       -- ^ The full signature of the function, e.g. Ord a => a -> Int -> Bool
  , package    :: ByteString       -- ^ The name of the package containing the module, e.g. containers
  , sourceURI  :: Maybe ByteString -- ^ An optional URI to the online source of the function.
  } 
  deriving (Show, Eq)

data QueryInfo = QueryInfo
  { querySettings :: QuerySettings
  , index         :: Persistent
  , documents     :: SmallDocuments FunctionInfo
  , cache         :: Cache
  }

data QuerySettings = QuerySettings 
  { searchString  :: String        -- ^ The query String to search for
  , offset        :: Int           -- ^ The page offset e.g. 50 => skip the first 50 results
  , caseSensitive :: Bool
  , processConfig :: ProcessConfig -- ^ ProcessConfig for Holumbus.Query.Process processQuery
  , modules       :: [RConfig]     -- ^ Additional module restrictions and maybe ranking
  , packages      :: [RConfig]     -- ^ Additional package restrictions and maybe ranking
  }

-- | RConfig is either a (String, Float) or only a String
data RConfig = Rank (String, Score)
             | Name String
             deriving (Show, Eq)

-- Configuration for application configuration

data HayooConfig = HayooConfig
  { indexHandler :: Persistent
  , docsHandler  :: SmallDocuments FunctionInfo
  , cacheHandler :: Cache
  }

instance AppConfiguration HayooConfig where --AppConfig where
--  getInstance :: (AppConfiguration a, MonadIO m) => m a
  getInstance = loadHayooConfig -- >>= (return . AppConfig)

loadIndex :: MonadIO m => m Persistent
loadIndex = liftIO $ (putStrLn "blub") >> loadFromFile "./indexes/hayoo-index.bin"

loadDocs :: MonadIO m => m (SmallDocuments FunctionInfo)
loadDocs = liftIO $ loadFromFile "./indexes/docs-small.bin"

loadCache :: MonadIO m => m Cache
loadCache = liftIO $ createCache "./indexes/cache.db"

loadHayooConfig :: MonadIO m => m HayooConfig
loadHayooConfig = do
  !i <- loadIndex
  d <- loadDocs
  c <- loadCache
  return $ HayooConfig
    { indexHandler = i
    , docsHandler  = d
    , cacheHandler = c
    }

-- instances for loading xml or bin files

instance XmlPickler FunctionInfo where
  xpickle = xpWrap (fromTuple, toTuple) xpFunction
    where
    fromTuple (m, s, p, r) = FunctionInfo (B.fromString m) (B.fromString s) (B.fromString p) (liftM B.fromString $ r)
    toTuple (FunctionInfo m s p r) = (B.toString m, B.toString s, B.toString p, liftM B.toString $ r)
    xpFunction = xp4Tuple xpModule xpSignature xpPackage xpSource
      where -- We are inside a doc-element, therefore everything is stored as attribute.
      xpModule = xpAttr "module" xpText0
      xpSignature = xpAttr "signature" xpText0
      xpPackage = xpAttr "package" xpText0
      xpSource = xpOption (xpAttr "source" xpText0)

instance Binary FunctionInfo where
  put (FunctionInfo m s p r) = put m >> put s >> put p >> put r
-- TH 12.08.2008 De-serialize more strict
--  get = liftM4 FunctionInfo get get get get
  get = do
        !m <- get
        !s <- get
        !p <- get
        !r <- get
        return $! FunctionInfo m s p r

