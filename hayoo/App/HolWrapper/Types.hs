module App.HolWrapper.Types
  ( SearchResult
  , FunctionInfo (..)
  , QueryInfo (..)
  , QuerySettings (..)
  , RConfig (..)
  ) where

import Holumbus.Index.Cache (Cache)
import Holumbus.Index.Inverted.OneFile (Persistent)
import Holumbus.Index.SmallDocuments (SmallDocuments)
import Holumbus.Query.Process (ProcessConfig)
import Holumbus.Query.Result (Result, Score)

import Data.ByteString.UTF8 (ByteString)

type SearchResult = (Result FunctionInfo, QueryInfo)

-- | Additional information about a function.
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
  { searchString  :: String
  , offset        :: Int
  , processConfig :: ProcessConfig
  , modules       :: [RConfig]
  , packages      :: [RConfig]
  }

data RConfig = Rank (String, Score)
             | Name String
