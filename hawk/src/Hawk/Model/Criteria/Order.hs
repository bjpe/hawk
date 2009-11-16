-- --------------------------------------------------------------------------
{- |
   Module      :  Hawk.Model.Criteria.Criteria
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
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

data OrderType = Asc
               | Desc

data Order = Order OrderType ColumnName

-- | Sort Ascending by the column
asc :: ColumnName -> Order
asc = Order Asc

-- | Sort Descending by the column
desc :: ColumnName -> Order
desc = Order Desc

instance ShowSql OrderType where
  showSql Asc  = " ASC"
  showSql Desc = " DESC"

instance ShowSql Order where
  showSql (Order o c) = c ++ showSql o
