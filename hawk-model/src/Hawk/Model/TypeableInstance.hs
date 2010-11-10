-- --------------------------------------------------------------------------
{- |
   Module      :  $Header$
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Main.hs 57 2009-05-29 11:33:59Z inf6254 $

-}
-- --------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, UndecidableInstances, DeriveDataTypeable #-}

module Hawk.Model.TypeableInstance where

import Data.Data
import Database.HDBC
import Data.Maybe
import Data.Convertible.Base (Convertible)

import Hawk.Model.Persistence.Types
import Data.Generics.Aliases
import Data.Generics.Schemes

instance Data a => Persistent a where
  --toList :: p -> [SqlValue]  
  toList = catMaybes . gmapQ (Nothing `mkQ` f')
--  where
  --fromList :: [SqlValue] -> p
  fromList  = undefined
  
  -- persistentType :: p -> String
  persistentType = show . constr
  
  --tableColumns :: p -> [ColumnName]
  tableColumns = constrFields . constr

constr p = indexConstr (dataTypeOf p) 1

data Test = Test {a:: String, b:: String}
    deriving (Show, Typeable, Data)

t = Test {a="c", b="d"}

f = catMaybes . gmapQ (Nothing `mkQ` f')

--f' :: (Convertible a SqlValue) => a -> Maybe SqlValue
f' :: String -> Maybe SqlValue
f' = Just . toSql

--f = everything (++) ([SqlNull] `mkQ` f')

