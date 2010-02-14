module App.HolumbusWrapper.Query where

import Holumbus.Query.Result
import Holumbus.Query.Processor
import Holumbus.Query.Fuzzy
--import Holumbus.Query.Language.Parser
import App.HolumbusWrapper.Parser
import Holumbus.Query.Language.Grammar
import Holumbus.Index.Common

import App.HolumbusWrapper.Types

query :: QueryInfo -> Result FunctionInfo
query (QueryInfo q s i d)  =  do
  let parsedQuery = parseQry s q
  let config = mkConfig s
  processQuery config i d parsedQuery 

mkConfig :: QuerySettings -> ProcessConfig
--mkConfig NoSettings = ProcessConfig (FuzzyConfig False True 1.0 []) True 50
mkConfig s = ProcessConfig (fuzzyCfg s) (optimizeQry s) (wordLmt s)

parseQry :: QuerySettings -> String -> Query
parseQry NoSettings s = eitherQry (parseQuery s) Word
parseQry qs s = eitherQry (parseQuery s) Word

{-             case (parseQuery s) of
                   Left err -> Word err -- TODO redirect, set flash
                   Right q -> q
-}
eitherQry :: Either a b -> (a -> b) -> b
eitherQry (Left e) s  = s e
eitherQry (Right v) _ = v
