module App.HolWrapper
  ( module App.HolWrapper.Parser
  , module App.HolWrapper.Print
  , module App.HolWrapper.QueryInfo
  , module App.HolWrapper.QuerySettings
  , module App.HolWrapper.Ranking
  , module App.HolWrapper.Types (SearchResult) -- do not use QueryInfo, QuerySettings or FunctionInfo in your application
  
  , query
  
  , numResults
  , numWordCompletitions
  , getSearchString
  ) where

import App.HolWrapper.Parser
import App.HolWrapper.Print
import App.HolWrapper.QueryInfo
import App.HolWrapper.QuerySettings
import App.HolWrapper.Ranking
import App.HolWrapper.Types

import Data.Either (either)

-- ## Query Functions
query :: QueryInfo    -- ^ 
      -> QueryParser 
      -> Maybe Ranking 
      -> Either String SearchResult
query qi qp r = 
  let qs = querySettings qi
  in either id right parse
  where 
    parse = qp qs
    right q = 
      (maybe proc (\x -> rank x proc) r, qs) 
      where proc = processQuery (processConfig qs) (index qi) (documents qi) q

-- ## Standard Print functions
numResults :: SearchResult -> Int

numWordCompletitions :: SearchResult -> Int

getSearchString :: SearchResult -> String
