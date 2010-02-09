{-# LANGUAGE BangPatterns #-}
module App.HolumbusWrapper.Types where

import Holumbus.Index.Inverted.OneFile (Persistent)
import Holumbus.Index.SmallDocuments (SmallDocuments)
import Holumbus.Query.Fuzzy

import Data.Binary

import Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as B

import Text.XML.HXT.Arrow

import Control.Monad (liftM)

data QueryInfo = QueryInfo
  { queryString   :: String
  , querySettings :: QuerySettings
  , index         :: Persistent
  , documents     :: SmallDocuments FunctionInfo
  }

data QuerySettings = QuerySettings 
  { caseSensitive    :: Bool -- default is False
  , optimizeQry      :: Bool -- default is True
  , wordLmt          :: Int
  , fuzzyCfg         :: FuzzyConfig
  , useModules       :: [PMConfig] -- default is empty List
  , disallowModules  :: [String]
  , usePackages      :: [PMConfig]
  , diallowPackages  :: [String]
  }
  | NoSettings

data PMConfig = PMRank
  { name    :: String
  , ranking :: Int
  }
  | PMName { name :: String }

defaultHConfig :: QuerySettings
defaultHConfig = QuerySettings False True 50 fcfg [] [] [] []
  where fcfg = FuzzyConfig False True 1.0 []

-- | Additional information about a function.
data FunctionInfo = FunctionInfo 
  { moduleName :: ByteString      -- ^ The name of the module containing the function, e.g. Data.Map
  , signature :: ByteString       -- ^ The full signature of the function, e.g. Ord a => a -> Int -> Bool
  , package   :: ByteString       -- ^ The name of the package containing the module, e.g. containers
  , sourceURI :: Maybe ByteString -- ^ An optional URI to the online source of the function.
  } 
  deriving (Show, Eq)

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

