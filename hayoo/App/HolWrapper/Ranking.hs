module App.HolWrapper.Ranking
  ( customRanking
  , defaultRanking
  , emptyRanking
  ) where

import App.HolWrapper.Types

import Holumbus.Index.Common
--import Holumbus.Query.Language.Grammar
import Holumbus.Query.Ranking
import Holumbus.Query.Result
import Holumbus.Utility

import Hawk.Controller.Util.Text (splitAll)

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.ByteString.UTF8 as B

-- | Generate Ranking from QuerySettings and hard coded Rankings
-- / (to be customized)
customRanking :: QuerySettings -> Maybe (RankConfig FunctionInfo)
customRanking qs = Just $ RankConfig (docsRanking qs tms) -- rank documents by context and module / package
                                     (wordsRanking tms) -- rank word by exact query fitting and
                   where tms = splitAll (== ' ') $ searchString qs -- TODO extractTerms $ query qs

-- | Generate a minimal Ranking by occurrence
defaultRanking :: Ranking -- Maybe (RankConfig FunctionInfo)
defaultRanking _ = Just $ RankConfig docRankByCount wordRankByCount

-- | Do not use any Ranking
emptyRanking :: Ranking
emptyRanking _ = Nothing

-- ## Private

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
        settingsModuleScore = lookupMdl (modules qs)
          where lookupMdl [] _ = 1.0
                lookupMdl ((Rank (p,s)):xs) mdl = if L.elem p mdl then s else (lookupMdl xs mdl)
                lookupMdl (_:xs) mdl = lookupMdl xs mdl
        settingsPackageScore = lookupPkg pkg getPkg
          where pkg = (packages qs) ++ corePackages -- add score for base package, containers, etc. here
                getPkg = maybe "" (\cust -> B.toString $ package cust) $ custom $ document di
                lookupPkg [] _ = 1.0
                lookupPkg ((Rank (p,s)):xs) name = if p == name then s else lookupPkg xs name
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

lookupContextWeight :: Context -> Score
lookupContextWeight c = maybe 0.0 id $ L.lookup c contextWeights

-- ## Static hard coded Rankings

-- | Weight of core packages
corePackages :: [RConfig]
corePackages = [ Rank ("base", 2) 
               , Rank ("containers", 2)
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
