module App.HolWrapper.Parser
  ( customParser
  , defaultParser
  ) where

import Holumbus.Query.Language.Parser (parseQuery)

-- | Parse a Query as you want
-- / (to be customized)
customParser :: QuerySettings -> Either String Query
customParser = 

-- | Use the default Holumbus parser for queries 
defaultParser :: String -> Either String Query
defaultParser = parseQuery
