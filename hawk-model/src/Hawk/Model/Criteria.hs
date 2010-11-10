-- --------------------------------------------------------------------------
{- |
   Module      :  $Header$
   Copyright   :  Copyright (C) 2009-2010 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Criteria.hs 291 2009-05-01 13:01:55Z inf6509 $

   A SQL Query api data, this module rexport all needed types and functions.
-}
-- ----------------------------------------------------------------------------
module Hawk.Model.Criteria
    ( module Hawk.Model.Criteria.Criteria
    , module Hawk.Model.Criteria.Projection
    , module Hawk.Model.Criteria.Restriction
    , module Hawk.Model.Criteria.Order
    , querySelect
    , executeManipulation
    ) where

import Hawk.Model.Criteria.Criteria
import Hawk.Model.Criteria.Projection
import Hawk.Model.Criteria.Restriction
import Hawk.Model.Criteria.Order
import Hawk.Model.Criteria.Types
import Hawk.Model.MonadDB

-- | Select a list of 'SqlValue's by a 'Query'
querySelect :: MonadDB m => Query -> m [[SqlValue]]
querySelect = uncurry sqlSelect . sqlExprPair

executeManipulation :: MonadDB m => Manipulation -> m Integer
executeManipulation = uncurry sqlExecute . sqlExprPair