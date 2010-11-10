-- --------------------------------------------------------------------------
{- |
   Module      :  $Header$
   Copyright   :  Copyright (C) 2009-2010 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Order.hs 291 2009-05-01 13:01:55Z inf6509 $

   The order part (ORDER BY) of the criteria api
-}
-- ----------------------------------------------------------------------------
module Hawk.Model.Criteria.Order
  ( Order
  , asc
  , desc
  ) where

import Hawk.Model.Criteria.Types
  ( GetSqlString (sqlString)
  , ColumnName
  )

-- | Ordering directions of SQL
data OrderType
  = Asc   -- ^ Ascending order
  | Desc  -- ^ Descending order

-- | Abstract data type representing an SQL order of a column
data Order = Order OrderType ColumnName

-- | Sort ascending by the column
asc :: ColumnName -> Order
asc = Order Asc

-- | Sort descending by the column
desc :: ColumnName -> Order
desc = Order Desc

instance GetSqlString OrderType where
  sqlString Asc  = " ASC"
  sqlString Desc = " DESC"

instance GetSqlString Order where
  sqlString (Order o c) = c ++ sqlString o
