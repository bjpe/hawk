-- --------------------------------------------------------------------------
{- |
   Module      :  $Header$
   Copyright   :  Copyright (C) 2009-2010 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Types.hs 307 2009-05-06 10:29:12Z inf6254 $

   Types for the criteria api
-}
-- ----------------------------------------------------------------------------
module Hawk.Model.Criteria.Types
    ( GetSqlString (..)
    , GetSqlValues (..)
    , sqlExprPair
    , SqlExprPair
    , module Hawk.Model.Types
    ) where

import Hawk.Model.Types

-- | A Pair from query string and sql values for HDBC
type SqlExprPair = (String, [SqlValue])

-- | Type class for generating the SQL string
class GetSqlString p where
  -- | Building the SQL string
  sqlString :: p -> String

-- | Type class for extracting the parameter values of an SQL expression
class GetSqlValues c where
  -- | Extracting the SQL values
  sqlValues :: c -> [SqlValue]

-- | Converts an expression reflecting an SQL expression into an ExprPair
sqlExprPair :: (GetSqlString c, GetSqlValues c) => c -> SqlExprPair
sqlExprPair c = (sqlString c, sqlValues c)

instance GetSqlString SqlValue where
  sqlString _ = "?"
