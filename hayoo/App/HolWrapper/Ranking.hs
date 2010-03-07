module App.HolWrapper.Ranking
  ( customRanking
  , defaultRanking
  , emptyRanking
  ) where

-- | Generate Ranking from QuerySettings and hard coded Rankings
-- / (to be customized)
customRanking :: QuerySettings -> Maybe RankConfig
customRanking qs =

-- | Generate a minimal Ranking by occurrence
defaultRanking :: Maybe RankConfig
defaultRanking = RankConfig docRankByCount wordRankByCount

-- | Do not use any Ranking
emptyRanking :: Maybe RankConfig
emptyRanking = Nothing
