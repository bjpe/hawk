-- --------------------------------------------------------------------------
{- |
   Module      :  $Header$
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Types.hs 307 2009-05-06 10:29:12Z inf6254 $

   Types for the criteria api
-}
-- ----------------------------------------------------------------------------
module Hawk.Model.Criteria.Types
    ( ShowSql (..)
    , GetValues (..)
    , toExprPair
    , SqlExprPair
    , module Hawk.Model.Types
    ) where

import Hawk.Model.Types

-- | A Pair from query string and sql values for HDBC
type SqlExprPair = (String, [SqlValue])

-- | SQL-Generating
class ShowSql p where
  showSql :: p -> String

-- | Getting out the values
class GetValues c where
  getValues :: c -> [SqlValue]

-- | Converts a ExprPair for HDBC
toExprPair :: (GetValues c, ShowSql c) => c -> SqlExprPair
toExprPair c = (showSql c, getValues c)

instance ShowSql SqlValue where
  showSql _ = "?"
