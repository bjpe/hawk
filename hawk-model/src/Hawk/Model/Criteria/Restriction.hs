-- --------------------------------------------------------------------------
{- |
   Module      :  $Header$
   Copyright   :  Copyright (C) 2009-2010 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Restriction.hs 307 2009-05-06 10:29:12Z inf6254 $

   Restrictions for the criteria api
-}
-- ----------------------------------------------------------------------------
{-# LANGUAGE ExistentialQuantification, FlexibleContexts #-}
module Hawk.Model.Criteria.Restriction
  (
  -- * Data types and constructors
    Restriction
  , CompareValue
  , col
  , val
  , sqlVal
  -- * Restriction combinators
  , andExpr
  , (.&&.)
  , orExpr
  , (.||.)
  , allExpr
  , anyExpr
  , notExpr
  -- * SimpleRestriction constructors
  , nullExpr
  , notNullExpr
  , eqExpr
  , (.==.)  
  , neExpr
  , (./=.)
  , gtExpr
  , (.>.)
  , ltExpr
  , (.<.)
  , geExpr
  , (.>=.)
  , leExpr
  , (.<=.)
  , likeExpr
  , notlikeExpr
  , betweenExpr
  , inExpr
  ) where

import Hawk.Model.Criteria.Types
import Data.List (intercalate)
import Data.Convertible (Convertible)

-- | A Restriction
data Restriction 
  = Simple SimpleRestriction
  | NotExpr Restriction
  | LogicExpr LogicOp [Restriction]

-- | The Operators And and Or
data LogicOp = And | Or

-- * Restriction combinators
-- | True if both Restrictions are true
andExpr :: Restriction -> Restriction -> Restriction
andExpr lhs rhs = allExpr [lhs, rhs]

(.&&.) :: Restriction -> Restriction -> Restriction
(.&&.) = andExpr

-- | True if one Restrictions is true
orExpr :: Restriction -> Restriction -> Restriction
orExpr lhs rhs = anyExpr [lhs, rhs]

(.||.) :: Restriction -> Restriction -> Restriction
(.||.) = orExpr

-- | True if all Restrictions are true
allExpr :: [Restriction] -> Restriction
allExpr = LogicExpr And

-- | True if one Restrictions is true
anyExpr :: [Restriction] -> Restriction
anyExpr = LogicExpr Or

-- | The not Operator
notExpr :: Restriction -> Restriction
notExpr = NotExpr

-- | Simple Restriction
data SimpleRestriction 
  = forall a. UnExpr UnOp (CompareValue a)
  | forall a. BinExpr BinOp (CompareValue a) (CompareValue a)
  | forall a. Between (CompareValue a) (CompareValue a) (CompareValue a)
  | forall a. In (CompareValue a) [SqlValue]

data CompareValue a
  = Col ColumnName
  | Val SqlValue

-- | Construct a CompareValue from a Value in a Column
col :: ColumnName -> CompareValue a
col = Col

-- | Construct a CompareValue from a Value
val :: Convertible a SqlValue => a -> CompareValue a
val = Val . toSql

-- | Construct a CompareValue from a SqlValue
sqlVal :: SqlValue -> CompareValue SqlValue
sqlVal = Val

-- | unary Operations
data UnOp = IsNull
          | IsNotNull

-- | binary Operations
data BinOp = Equal
           | NotEqual
           | GreaterThan
           | LessThan
           | GreaterThanOrEqual
           | LessThanOrEqual
           | Like
           | NotLike

--  SimpleRestriction constructors
-- | true if a column have a NULL Value
nullExpr :: CompareValue a -> Restriction
nullExpr = Simple . UnExpr IsNull

-- | true if a column have a not a NULL Value
notNullExpr :: CompareValue a -> Restriction
notNullExpr = Simple . UnExpr IsNotNull

-- | Compare the Value of two Columns. True if equal.
eqExpr :: CompareValue a -> CompareValue a -> Restriction
eqExpr l = Simple . BinExpr Equal l

(.==.) :: CompareValue a -> CompareValue a -> Restriction
(.==.) = eqExpr

-- | Compare the Value of two Columns. True if not equal.
neExpr :: CompareValue a -> CompareValue a -> Restriction
neExpr l = Simple . BinExpr NotEqual l

(./=.) :: CompareValue a -> CompareValue a -> Restriction
(./=.) = neExpr

-- | Compare the Value of two Columns. True if first value is greater then the second.
gtExpr :: CompareValue a -> CompareValue a -> Restriction
gtExpr l = Simple . BinExpr GreaterThan l

(.>.) :: CompareValue a -> CompareValue a -> Restriction
(.>.) = gtExpr

-- | Compare the Value of two Columns. True if first value is less then the second.
ltExpr :: CompareValue a -> CompareValue a -> Restriction
ltExpr l = Simple . BinExpr LessThan l

(.<.) :: CompareValue a -> CompareValue a -> Restriction
(.<.) = ltExpr

-- | Compare the Value of two Columns. True if first value is greater or equal then the second.
geExpr :: CompareValue a -> CompareValue a -> Restriction
geExpr l = Simple . BinExpr GreaterThanOrEqual l

(.>=.) :: CompareValue a -> CompareValue a -> Restriction
(.>=.) = geExpr

-- | Compare the Value of two Columns. True if first value is less or equal then the second.
leExpr :: CompareValue a -> CompareValue a -> Restriction
leExpr l = Simple . BinExpr LessThanOrEqual l

(.<=.) :: CompareValue a -> CompareValue a -> Restriction
(.<=.) = leExpr

-- | Test, if a column is like a String
likeExpr :: CompareValue String -> CompareValue String -> Restriction
likeExpr l = Simple . BinExpr Like l

-- | Test, if a column is not like a String
notlikeExpr :: CompareValue String -> CompareValue String -> Restriction
notlikeExpr l = Simple . BinExpr NotLike l

-- | Test, if the first value is between the second and third
betweenExpr :: CompareValue a -> CompareValue a -> CompareValue a -> Restriction
betweenExpr v l = Simple . Between v l

-- | Test, if the first row is in the list of values.
inExpr :: Convertible a SqlValue => CompareValue a -> [a] -> Restriction
inExpr l = Simple . In l . map toSql

-- instances

instance GetSqlString LogicOp where
  sqlString And = " AND "
  sqlString Or  = " OR "

instance GetSqlString UnOp where
  sqlString IsNull    = " IS NULL "
  sqlString IsNotNull = " IS NOT NULL "

instance GetSqlString BinOp where
  sqlString Equal              = " == "
  sqlString NotEqual           = " <> "
  sqlString GreaterThan        = " > "
  sqlString LessThan           = " < "
  sqlString GreaterThanOrEqual = " >= "
  sqlString LessThanOrEqual    = " <= "
  sqlString Like               = " LIKE "
  sqlString NotLike            = " NOT LIKE "

instance GetSqlString (CompareValue a) where
  sqlString (Col c) = c
  sqlString (Val v) = sqlString v

instance GetSqlString SimpleRestriction where
  sqlString (UnExpr op c)
    = sqlString op ++ paren (sqlString c)
  sqlString (BinExpr op c1 c2) = sqlString c1 ++ sqlString op ++ sqlString c2
  sqlString (Between c1 c2 c3)
    = sqlString c1 ++ " BETWEEEN " ++ sqlString c2 ++ " AND " ++ sqlString c3
  sqlString (In c vs)  = sqlString c ++ " IN " ++ paren placeHolder
    where placeHolder = intercalate ", " $ map sqlString vs

instance GetSqlString Restriction where
  sqlString (Simple s)        = sqlString s
  sqlString (NotExpr c)       = " NOT " ++ paren (sqlString c)
  sqlString (LogicExpr op cs)
    = intercalate (sqlString op) $ map (paren . sqlString) cs

instance GetSqlValues (CompareValue a) where
  sqlValues (Col _) = []
  sqlValues (Val v) = [v]

instance GetSqlValues SimpleRestriction where
  sqlValues (UnExpr  _      c) = sqlValues c
  sqlValues (BinExpr _  c1 c2) = concatMap sqlValues [c1,c2]
  sqlValues (Between c1 c2 c3) = concatMap sqlValues [c1,c2,c3]
  sqlValues (In c vs)          = sqlValues c ++ vs

instance GetSqlValues Restriction where
  sqlValues (Simple s)       = sqlValues s
  sqlValues (NotExpr c)      = sqlValues c
  sqlValues (LogicExpr _ cs) = concatMap sqlValues cs

paren :: String -> String
paren s = '(' : s ++ ")"
