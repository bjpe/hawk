-- --------------------------------------------------------------------------
{- |
   Module      :  Hawk.Model.Types
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Main.hs 57 2009-05-29 11:33:59Z inf6254 $

   Some types used throughout the model
-}
-- --------------------------------------------------------------------------
module Hawk.Model.Types
  ( PrimaryKey
  , ForeignKey
  , TableName
  , ColumnName
  , SqlValue
  , fromSql
  , toSql
  ) where

import Database.HDBC 
  ( SqlValue
  , fromSql
  , toSql
  )

-- | The type for primary keys
type PrimaryKey = Integer

-- | The type for foreign keys
type ForeignKey = Maybe PrimaryKey

-- | The name of a database table
type TableName  = String

-- | The name of a database column
type ColumnName = String
