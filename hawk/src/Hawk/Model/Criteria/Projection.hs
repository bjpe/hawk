-- --------------------------------------------------------------------------
{- |
   Module      :  $Header$
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Projection.hs 291 2009-05-01 13:01:55Z inf6509 $

   Projection for the criteria api
-}
-- ----------------------------------------------------------------------------
module Hawk.Model.Criteria.Projection
  ( Projection
  , colP, asP
  , rowCountP, countP
  , sumP, avgP
  , minP, maxP
  , customP
  ) where

import Hawk.Model.Criteria.Types

-- | The Data for Aggregate Functions
data Op = Count
        | Sum
        | Avg
        | Min
        | Max
        | Custom String

-- | A Projection is the defnintion for one Column
data Projection = Projection
  { as     :: Maybe ColumnName -- ^ rename a column (@AS@)
  , op     :: Maybe Op         -- ^ Operation (@COUNT@, @SUM@, @AVG@ ...)
  , column :: ColumnName       -- ^ the column name
  }

-- | Creates a SimpleProject from a Name
colP :: ColumnName -> Projection
colP = Projection Nothing Nothing

-- | Counts how many rows are in the table
rowCountP :: Projection
rowCountP = countP "*"

-- | Count how many rows have not @NULL@ Value in the given Column.
countP :: ColumnName -> Projection
countP = Projection Nothing $ Just Count

-- (TODO what happen if NULL is returnd)
-- | Sums all values in the column. Gives @NULL@ if one Column ist @NULL@.
sumP :: ColumnName -> Projection
sumP = Projection Nothing $ Just Sum

-- | Return the average value of all non-@NULL@ values in the column.
avgP :: ColumnName -> Projection
avgP = Projection Nothing $ Just Avg

-- | Return the minimum non-@NULL@ value of all values in the column.
minP :: ColumnName -> Projection
minP = Projection Nothing $ Just Min

-- | Return the maximum value of all values in the column.
maxP :: ColumnName -> Projection
maxP = Projection Nothing $ Just Max

-- | Apply a custom SQL-Function
customP :: String -> ColumnName -> Projection
customP =  Projection Nothing .  Just . Custom

-- | Give a SimpleProjection a other Name.
asP :: ColumnName -> Projection -> Projection
asP a p = p { as = Just a }

instance ShowSql Op where
  showSql Count      = "COUNT"
  showSql Sum        = "SUM"
  showSql Avg        = "AVG"
  showSql Min        = "MIN"
  showSql Max        = "MAX"
  showSql (Custom c) = c

instance ShowSql Projection where
  showSql p = case op p of
            Just o' -> showSql o' ++ paren (column p) ++ showAs (as p)
            Nothing -> column p ++ showAs (as p)
    where showAs = maybe "" ((++) " AS ")

paren :: String -> String
paren s = '(' : s ++ ")"
