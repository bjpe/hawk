module App.HolumbusWrapper.Query 
 ( query
 , rankedQuery
 ) where

import Holumbus.Query.Result
import Holumbus.Query.Ranking
import Holumbus.Query.Processor
import Holumbus.Query.Fuzzy
--import Holumbus.Query.Language.Parser
import Holumbus.Query.Language.Grammar
import Holumbus.Index.Common
import Holumbus.Utility

import App.HolumbusWrapper.Parser (parseQuery)
import App.HolumbusWrapper.Types

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.ByteString.UTF8 as B

query :: QueryInfo -> HayooResult
query (QueryInfo q s _ i d _)  =  do
  let parsedQuery = parseQry s q
  let config = mkConfig s
  either (\err -> (emptyResult, err))
         (\p -> (processQuery config i d p, ""))
         parsedQuery 

rankedQuery :: QueryInfo -> HayooResult
rankedQuery (QueryInfo q s _ i d _)  = either err ranked parsedQuery
  where 
    parsedQuery = parseQry s q
    err e = (emptyResult, e)
    ranked qry = (rank rankCfg $ proc qry, "")
      where
        proc = processQuery (mkConfig s) i d
        rankCfg = RankConfig (docsRanking s tms) -- rank documents by context and module / package
                             (wordsRanking tms) -- rank word by exact query fitting and
          where tms = extractTerms qry

-- | Rank docs by context and settings
docsRanking :: QuerySettings -> [String] -> DocId -> DocInfo FunctionInfo -> DocContextHits -> Score
docsRanking qs tms id di ch = contextScore
                            * count -- multiply with found occurences
                            * exact
                            * (moduleDepth getMdl)
                            * (settingsModuleScore getMdl)
                            * settingsPackageScore
  where contextScore = M.foldWithKey (\c _ _ -> (lookupContextWeight c) / maxContextWeight) 0.0 ch
        count = docRankByCount id di ch
        settingsModuleScore = lookupMdl (useModules qs)
          where lookupMdl [] _ = 1.0
                lookupMdl ((PMRank p s):xs) mdl = if L.elem p mdl then s else (lookupMdl xs mdl)
                lookupMdl (_:xs) mdl = lookupMdl xs mdl
        settingsPackageScore = lookupPkg pkg getPkg
          where pkg = (usePackages qs) ++ corePackages -- add score for base package, containers, etc. here
                getPkg = maybe "" (\cust -> B.toString $ package cust) $ custom $ document di
                lookupPkg [] _ = 1.0
                lookupPkg ((PMRank p s):xs) name = if p == name then s else lookupPkg xs name
                lookupPkg (_:xs) name = lookupPkg xs name
        exact = if L.elem (title $ document di) tms then 4.0 else 1.0
        moduleDepth l = ((1.0 / (fromIntegral $ length l)) - 0.5) * 0.1 + 1.0 -- for base modules it is (* 1.05)
        getMdl = maybe [] (\cust -> split "." $ B.toString $ moduleName cust) $ custom $ document di

-- | Rank words by count and increase exact matches
wordsRanking :: [String] -> Word -> WordInfo -> WordContextHits -> Score
wordsRanking tms w wi ch = exact 
                        * (wordRankByCount w wi ch)
                        * (wordRankWeightedByCount contextWeights w wi ch)
  where exact = if L.elem w tms then 4.0 else 1.0

-- | Weight of core packages
corePackages :: [PMConfig]
corePackages = [ (PMRank "base" 2) 
               , (PMRank "containers" 2)
               ]

-- | Returns maximum of contextWeights
maxContextWeight :: Score
maxContextWeight = 0.9

-- | Weights for context weighted ranking.
contextWeights :: [(Context, Score)]
contextWeights = [ ("name", 0.9)
                 , ("partial", 0.8)
                 , ("module", 0.7)
                 , ("hierarchy", 0.6)
                 , ("package", 0.5)
                 , ("signature", 0.4)
                 , ("description", 0.2)
                 , ("normalized", 0.1)
                 ]

lookupContextWeight :: Context -> Score
lookupContextWeight c = maybe 0.0 id $ L.lookup c contextWeights

mkConfig :: QuerySettings -> ProcessConfig
--mkConfig NoSettings = ProcessConfig (FuzzyConfig False True 1.0 []) True 50
mkConfig s = ProcessConfig (fuzzyCfg s) (optimizeQry s) (wordLmt s)

-- TODO use settings
parseQry :: QuerySettings -> String -> Either String Query
parseQry NoSettings s = parseQuery s
parseQry qs s = parseQuery s

eitherQry :: Either a b -> (a -> b) -> b
eitherQry (Left e) s  = s e
eitherQry (Right v) _ = v
