module App.HolWrapper
  ( module App.HolWrapper.Common
  , module App.HolWrapper.Parser
  , module App.HolWrapper.Print
  , module App.HolWrapper.QueryInfo
  , module App.HolWrapper.QuerySettings
  , module App.HolWrapper.Ranking
  , module App.HolWrapper.Types 
{-         ( SearchResult
         , ResultTuple
         , HayooConfig
         , loadHayooConfig) -- do not use QueryInfo, QuerySettings or FunctionInfo in your application
  -}
  , query
  
  , numResults
  , numWordCompletitions
  , getSearchString
  , getOffset
  ) where

import App.HolWrapper.Common
import App.HolWrapper.Parser
import App.HolWrapper.Print
import App.HolWrapper.QueryInfo
import App.HolWrapper.QuerySettings
import App.HolWrapper.Ranking
import App.HolWrapper.Types

import Holumbus.Query.Processor (processQuery)
import Holumbus.Query.Ranking (rank)
import Holumbus.Query.Result

--import Data.Either --(either)

-- ## Query Functions
query :: QueryInfo    -- ^ 
      -> QueryParser 
      -> Ranking 
      -> SearchResult
query qi qp r = either Left right parse
  where 
    parse = qp $ qs qi
    right q = Right (maybe proc (\x -> rank x proc) (r $ qs qi), qi) 
      where proc = processQuery (processConfig $ qs qi) (index qi) (documents qi) q

qs :: QueryInfo -> QuerySettings
qs = querySettings

-- ## Standard Print functions
numResults :: ResultTuple -> Int
numResults (r, _) = sizeDocHits r

numWordCompletitions :: ResultTuple -> Int
numWordCompletitions (r, _) = sizeWordHits r

getSearchString :: ResultTuple -> String
getSearchString (_, i) = searchString $ qs i

getOffset :: ResultTuple -> Int
getOffset (_, i) = offset $ qs i
